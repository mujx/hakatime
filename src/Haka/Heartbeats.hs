{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Haka.Heartbeats
  ( User (..),
    API,
    server,
  )
where

import Control.Exception.Safe (throw)
import Control.Monad.Reader (asks)
import Data.Aeson (ToJSON)
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Filesystem.Path (splitExtension)
import Filesystem.Path.CurrentOS (fromText)
import GHC.Generics
import Haka.App (AppCtx (..), AppM)
import qualified Haka.DatabaseOperations as DbOps
import Haka.Errors (HeartbeatApiResponse (..))
import qualified Haka.Errors as Err
import Haka.Types
import Hasql.Pool (Pool)
import Katip
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import Servant

data User = User
  { name :: T.Text,
    age :: Int,
    email :: T.Text,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

newtype HeartbeatResponses = HeartbeatResponses
  { responses :: [HeartbeatIdAndStatusCode]
  }
  deriving (Show, Generic)

data HeartbeatIdAndStatusCode = Foo | Bar | Baz
  deriving (Show, Generic, Enum)

newtype UserResponse = UserResponse {resData :: [User]}
  deriving (Show, Generic)

instance ToJSON UserResponse

type SingleHeartbeat =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "heartbeats"
    :> Header "X-Machine-Name" T.Text
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] HeartbeatPayload
    :> Post '[JSON] HeartbeatApiResponse

type MultipleHeartbeats =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "heartbeats.bulk"
    :> Header "X-Machine-Name" T.Text
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] [HeartbeatPayload]
    :> Post '[JSON] HeartbeatApiResponse

type API = SingleHeartbeat :<|> MultipleHeartbeats

server ::
  ( Maybe T.Text ->
    Maybe ApiToken ->
    HeartbeatPayload ->
    AppM HeartbeatApiResponse
  )
    :<|> ( Maybe T.Text ->
           Maybe ApiToken ->
           [HeartbeatPayload] ->
           AppM HeartbeatApiResponse
         )
server = heartbeatHandler :<|> multiHeartbeatHandler

mkHeartbeatId :: T.Text -> HearbeatData
mkHeartbeatId i = HearbeatData {heartbeatData = HeartbeatId {heartbeatId = i}}

handleSingleDbResult :: [Int64] -> AppM HeartbeatApiResponse
handleSingleDbResult ids =
  pure $ SingleHeartbeatApiResponse $ mkHeartbeatId (T.pack $ show (head ids))

handleManyDbResults :: [Int64] -> AppM HeartbeatApiResponse
handleManyDbResults ids =
  pure $
    BulkHeartbeatApiResponse (BulkHeartbeatData {bResponses = mkResponseItem ids})
  where
    mkResponseItem :: [Int64] -> [[ReturnBulkStruct]]
    mkResponseItem =
      map
        ( \x ->
            [ ReturnData $ mkHeartbeatId (T.pack $ show x),
              ReturnCode 201
            ]
        )

heartbeatHandler ::
  -- | X-Machine-Name header field with the hostname.
  Maybe T.Text ->
  -- | Authorization header field with the Api token.
  Maybe ApiToken ->
  HeartbeatPayload ->
  AppM HeartbeatApiResponse
heartbeatHandler _ Nothing _ = throw Err.missingAuthError
heartbeatHandler machineId (Just token) heartbeat = do
  $(logTM) InfoS "received a heartbeat"
  p <- asks pool
  res <- storeHeartbeats p token machineId [heartbeat]
  mkResponse res

multiHeartbeatHandler ::
  -- | X-Machine-Name header field with the hostname.
  Maybe T.Text ->
  -- | Authorization header field with the Api token.
  Maybe ApiToken ->
  [HeartbeatPayload] ->
  AppM HeartbeatApiResponse
multiHeartbeatHandler _ Nothing _ = throw Err.missingAuthError
multiHeartbeatHandler machineId (Just token) heartbeats = do
  $(logTM) InfoS (logStr ("received " <> show (length heartbeats) <> " heartbeats"))
  p <- asks pool
  res <- storeHeartbeats p token machineId heartbeats
  mkResponse res

-- | Construct an API Heartbeat response depending on the size of the response.
mkResponse :: Either DbOps.DatabaseException [Int64] -> AppM HeartbeatApiResponse
mkResponse res = do
  values <- either Err.logError pure res

  if length values > 1
    then handleManyDbResults values
    else handleSingleDbResult values

addMissingLang :: HeartbeatPayload -> HeartbeatPayload
addMissingLang hb@HeartbeatPayload {language = Nothing, ty = FileType} =
  let lang = convertToLang $ findExt (entity hb)
   in hb {language = lang}
  where
    findExt :: T.Text -> Maybe T.Text
    findExt = snd . splitExtension . fromText

    convertToLang :: Maybe T.Text -> Maybe T.Text
    convertToLang (Just ext) = case ext of
      "" -> Nothing
      "." -> Nothing
      a -> Just $ T.toUpper a
    convertToLang _ = Nothing
addMissingLang hb = hb

-- | Run the necessary database operations to store the incoming heartbeats.
storeHeartbeats ::
  Pool ->
  ApiToken ->
  Maybe T.Text ->
  [HeartbeatPayload] ->
  AppM (Either DbOps.DatabaseException [Int64])
storeHeartbeats p token machineId heartbeats =
  runM
    . embedToMonadIO
    . runError
    $ DbOps.interpretDatabaseIO $
      DbOps.processHeartbeatRequest p token machineId (map addMissingLang heartbeats)

-- TODO: Discard timestamps from the future
