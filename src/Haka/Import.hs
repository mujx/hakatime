{-# LANGUAGE TemplateHaskell #-}

module Haka.Import
  ( API,
    server,
    handleImportRequest,
  )
where

import Control.Exception.Safe (Exception, Typeable, bracket, throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as Bs
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime (..))
import GHC.Generics
import Haka.AesonHelpers
  ( convertReservedWords,
    noPrefixOptions,
    untagged,
  )
import qualified Haka.Cli as Cli
import qualified Haka.DatabaseOperations as DbOps
import qualified Haka.Errors as Err
import Haka.Types (ApiToken, AppCtx (..), AppM, EntityType (..), HeartbeatPayload (..), runAppT)
import Haka.Utils (genDateRange)
import qualified Hasql.Connection as HasqlConn
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Queue.Low.AtLeastOnce as HasqlQueue
import Katip
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as R
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import Servant

newtype ImportRequestResponse = ImportRequestResponse
  { jobStatus :: Text
  }
  deriving (Generic, Show)

instance A.FromJSON ImportRequestResponse

instance A.ToJSON ImportRequestResponse

data QueueItem = QueueItem
  { reqPayload :: ImportRequestPayload,
    requester :: Text
  }
  deriving (Generic, Show)

instance A.FromJSON QueueItem

instance A.ToJSON QueueItem

data ImportRequestPayload = ImportRequestPayload
  { apiToken :: Text,
    startDate :: UTCTime,
    endDate :: UTCTime
  }
  deriving (Generic, Show)

instance A.FromJSON ImportRequestPayload

instance A.ToJSON ImportRequestPayload

queueName :: Text
queueName = "_import_requests_queue_channel"

wakatimeApi :: Text
wakatimeApi = "api.wakatime.com"

data ImportHeartbeatPayload = ImportHeartbeatPayload
  { wMachine_name_id :: Maybe Text,
    wUser_agent_id :: Text,
    wBranch :: Maybe Text,
    wCategory :: Maybe Text,
    wCursorpos :: Maybe Text,
    wDependencies :: Maybe [Text],
    wEntity :: Text,
    wIs_write :: Maybe Bool,
    wLanguage :: Maybe Text,
    wLineno :: Maybe Text,
    wLines :: Maybe Int64,
    wProject :: Maybe Text,
    wType :: EntityType,
    wTime :: Double
  }
  deriving (Eq, Show, Generic)

data HeartbeatList = HeartbeatList
  { listData :: [ImportHeartbeatPayload],
    listStart :: UTCTime,
    listEnd :: UTCTime,
    listTimezone :: Text
  }
  deriving (Show, Generic)

instance A.FromJSON ImportHeartbeatPayload where
  parseJSON = A.genericParseJSON noPrefixOptions

instance A.FromJSON HeartbeatList where
  parseJSON = A.genericParseJSON noPrefixOptions

data UserAgentPayload = UserAgentPayload
  { uaId :: Text,
    uaValue :: Text
  }
  deriving (Show, Generic)

data UserAgentList = UserAgentList
  { uaData :: [UserAgentPayload]
  }
  deriving (Show, Generic)

instance A.FromJSON UserAgentPayload where
  parseJSON = A.genericParseJSON noPrefixOptions

instance A.FromJSON UserAgentList where
  parseJSON = A.genericParseJSON noPrefixOptions

process :: QueueItem -> AppM ()
process item = do
  liftIO $ putStrLn $ "processing item: " <> show item
  let payload = reqPayload item
      header = R.header "Authorization" ("Basic " <> (encodeUtf8 $ apiToken payload))
      allDays = genDateRange (startDate payload) (endDate payload)

  bs <-
    R.req
      R.GET
      (R.https wakatimeApi /: "api" /: "v1" /: "users" /: "current" /: "user_agents")
      R.NoReqBody
      R.jsonResponse
      header

  let userAgents = (R.responseBody bs :: UserAgentList)

  traverse_
    ( \day -> do
        bs <-
          R.req
            R.GET
            (R.https wakatimeApi /: "api" /: "v1" /: "users" /: "current" /: "heartbeats")
            R.NoReqBody
            R.jsonResponse
            (("date" =: day) <> header)

        let heartbeatList = (R.responseBody bs :: HeartbeatList)

        liftIO $ print (show day <> " " <> (show $ length $ listData heartbeatList))

        let heartbeats = convertForDb userAgents heartbeatList

        pool' <- asks pool

        runM
          . embedToMonadIO
          . runError
          $ DbOps.interpretDatabaseIO $
            DbOps.importHeartbeats pool' (requester item) Nothing heartbeats
    )
    allDays

convertForDb :: UserAgentList -> HeartbeatList -> [HeartbeatPayload]
convertForDb = undefined

data ImportRequestException
  = ConnectionError (Maybe Bs.ByteString)
  | InvalidToken String
  | MalformedPaylod String
  deriving (Show, Typeable)

instance Exception ImportRequestException

handleImportRequest :: AppM ()
handleImportRequest = do
  settings <- liftIO Cli.getDbSettings
  ctx <- ask

  liftIO $
    bracket
      (acquireConn settings)
      HasqlConn.release
      ( \conn -> do
          liftIO $
            HasqlQueue.withDequeue
              queueName
              conn
              D.json
              numRetries
              numItems
              (processItems ctx)
      )
  where
    acquireConn settings = do
      res <- liftIO $ HasqlConn.acquire settings
      case res of
        Left e -> throw $ ConnectionError e
        Right conn -> pure conn

    processItems ctx items = do
      if length items == 0
        then throw $ MalformedPaylod "Received empty payload list"
        else do
          case A.fromJSON (head items) :: A.Result QueueItem of
            A.Success item -> runAppT ctx $ process item
            A.Error e -> throw $ MalformedPaylod e

    numRetries :: Int
    numRetries = 3

    numItems :: Int
    numItems = 1

type API = ImportRequest

type ImportRequest =
  "import"
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] ImportRequestPayload
    :> Post '[JSON] ImportRequestResponse

enqueueRequest :: A.Value -> IO ()
enqueueRequest payload = do
  settings <- Cli.getDbSettings
  res <- HasqlConn.acquire settings

  case res of
    Left Nothing -> error "failed to acquire connection while enqueuing import request"
    Left (Just e) -> error $ Bs.unpack e
    Right conn -> HasqlQueue.enqueue queueName conn E.json [payload]

server :: Maybe ApiToken -> ImportRequestPayload -> AppM ImportRequestResponse
server Nothing _ = throw Err.missingAuthError
server (Just token) payload = do
  $(logTM) InfoS "received an import request"

  p <- asks pool

  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getUserByToken p token

  user <- either Err.logError pure res

  liftIO $
    enqueueRequest
      ( A.toJSON $
          QueueItem
            { requester = user,
              reqPayload = payload
            }
      )

  return $ ImportRequestResponse {jobStatus = "ok"}
