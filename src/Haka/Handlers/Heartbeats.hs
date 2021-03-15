{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Haka.Handlers.Heartbeats
  ( User (..),
    API,
    server,
  )
where

import Control.Exception.Safe (throw, try)
import Data.Aeson (ToJSON)
import Data.Text (toUpper)
import Data.Time.Calendar (Day)
import Filesystem.Path (splitExtension)
import Filesystem.Path.CurrentOS (fromText)
import Haka.App (AppCtx (..), AppM)
import qualified Haka.Database as Db
import Haka.Errors (HeartbeatApiResponse (..))
import qualified Haka.Errors as Err
import Haka.Types
import Hasql.Pool (Pool)
import Katip
import qualified Relude.Unsafe as Unsafe
import Servant

data User = User
  { name :: Text,
    age :: Int,
    email :: Text,
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
    :> Header "X-Machine-Name" Text
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] HeartbeatPayload
    :> Post '[JSON] HeartbeatApiResponse

type MultipleHeartbeats =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "heartbeats.bulk"
    :> Header "X-Machine-Name" Text
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] [HeartbeatPayload]
    :> Post '[JSON] HeartbeatApiResponse

type API = SingleHeartbeat :<|> MultipleHeartbeats

server ::
  ( Maybe Text ->
    Maybe ApiToken ->
    HeartbeatPayload ->
    AppM HeartbeatApiResponse
  )
    :<|> ( Maybe Text ->
           Maybe ApiToken ->
           [HeartbeatPayload] ->
           AppM HeartbeatApiResponse
         )
server = heartbeatHandler :<|> multiHeartbeatHandler

mkHeartbeatId :: Text -> HearbeatData
mkHeartbeatId i = HearbeatData {heartbeatData = HeartbeatId {heartbeatId = i}}

handleSingleDbResult :: [Int64] -> AppM HeartbeatApiResponse
handleSingleDbResult ids =
  pure $ SingleHeartbeatApiResponse $ mkHeartbeatId (show (Unsafe.head ids))

handleManyDbResults :: [Int64] -> AppM HeartbeatApiResponse
handleManyDbResults ids =
  pure $
    BulkHeartbeatApiResponse (BulkHeartbeatData {bResponses = mkResponseItem ids})
  where
    mkResponseItem :: [Int64] -> [[ReturnBulkStruct]]
    mkResponseItem =
      map
        ( \x ->
            [ ReturnData $ mkHeartbeatId (show x),
              ReturnCode 201
            ]
        )

heartbeatHandler ::
  -- | X-Machine-Name header field with the hostname.
  Maybe Text ->
  -- | Authorization header field with the Api token.
  Maybe ApiToken ->
  HeartbeatPayload ->
  AppM HeartbeatApiResponse
heartbeatHandler _ Nothing _ = throw Err.missingAuthError
heartbeatHandler machineId (Just token) heartbeat = do
  logFM InfoS "received a heartbeat"
  p <- asks pool
  res <- storeHeartbeats p token machineId [heartbeat]
  mkResponse res

multiHeartbeatHandler ::
  -- | X-Machine-Name header field with the hostname.
  Maybe Text ->
  -- | Authorization header field with the Api token.
  Maybe ApiToken ->
  [HeartbeatPayload] ->
  AppM HeartbeatApiResponse
multiHeartbeatHandler _ Nothing _ = throw Err.missingAuthError
multiHeartbeatHandler machineId (Just token) heartbeats = do
  logFM InfoS ("received " <> showLS (length heartbeats) <> " heartbeats")
  p <- asks pool
  res <- storeHeartbeats p token machineId heartbeats
  mkResponse res

-- | Construct an API Heartbeat response depending on the size of the response.
mkResponse :: Either Db.DatabaseException [Int64] -> AppM HeartbeatApiResponse
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
    findExt :: Text -> Maybe Text
    findExt = snd . splitExtension . fromText

    convertToLang :: Maybe Text -> Maybe Text
    convertToLang (Just ext) = case ext of
      "" -> Nothing
      "." -> Nothing
      "org" -> Just "Org"
      "jinja" -> Just "Jinja"
      "jinja2" -> Just "Jinja"
      "tfvars" -> Just "Terraform"
      "cabal" -> Just "Cabal Config"
      "gotmpl" -> Just "Go template"
      "zig" -> Just "Zig"
      "purs" -> Just "PureScript"
      "dhall" -> Just "Dhall"
      a -> Just $ toUpper a
    convertToLang _ = Nothing
addMissingLang hb = hb

-- | Run the necessary database operations to store the incoming heartbeats.
storeHeartbeats ::
  Pool ->
  ApiToken ->
  Maybe Text ->
  [HeartbeatPayload] ->
  AppM (Either Db.DatabaseException [Int64])
storeHeartbeats p token machineId heartbeats = do
  let updatedHeartbeats = map ((\beat -> beat {machine = machineId}) . addMissingLang) heartbeats

  try $ liftIO $ Db.processHeartbeatRequest p token updatedHeartbeats

-- TODO: Discard timestamps from the future
