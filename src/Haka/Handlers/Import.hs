module Haka.Handlers.Import
  ( API,
    server,
    handleImportRequest,
    logExceptions,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket, throw, try)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as Bs
import Data.Time.Clock (UTCTime (..))
import Haka.AesonHelpers (noPrefixOptions)
import Haka.App (AppCtx (..), AppM, runAppT)
import qualified Haka.Cli as Cli
import qualified Haka.Database as Db
import qualified Haka.Errors as Err
import qualified Haka.Logger as Log
import Haka.Types (ApiToken, EntityType (..), HeartbeatPayload (..))
import Haka.Utils (genDateRange)
import qualified Hasql.Connection as HasqlConn
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Queue.Low.AtLeastOnce as HasqlQueue
import Katip
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as R
import qualified Relude.Unsafe as Unsafe
import Servant

data JobStatus
  = JobSubmitted
  | JobPending
  | JobFailed
  | JobFinished
  deriving (Generic, Show)

instance A.ToJSON JobStatus

newtype ImportRequestResponse = ImportRequestResponse
  { jobStatus :: JobStatus
  }
  deriving (Generic, Show)

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
wakatimeApi = "wakatime.com"

data ImportHeartbeatPayload = ImportHeartbeatPayload
  { wMachine_name_id :: Maybe Text,
    wUser_agent_id :: Text,
    wBranch :: Maybe Text,
    wCategory :: Maybe Text,
    wCursorpos :: Maybe Int64,
    wDependencies :: Maybe [Text],
    wEntity :: Text,
    wIs_write :: Maybe Bool,
    wLanguage :: Maybe Text,
    wLineno :: Maybe Int64,
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

newtype UserAgentList = UserAgentList
  { uaData :: [UserAgentPayload]
  }
  deriving (Show, Generic)

instance A.FromJSON UserAgentPayload where
  parseJSON = A.genericParseJSON noPrefixOptions

instance A.FromJSON UserAgentList where
  parseJSON = A.genericParseJSON noPrefixOptions

newtype MachineNameList = MachineNameList
  { machineData :: [MachineNamePayload]
  }
  deriving (Show, Generic)

data MachineNamePayload = MachineNamePayload
  { machineId :: Text,
    machineValue :: Text
  }
  deriving (Show, Generic)

instance A.FromJSON MachineNameList where
  parseJSON = A.genericParseJSON noPrefixOptions

instance A.FromJSON MachineNamePayload where
  parseJSON = A.genericParseJSON noPrefixOptions

logExceptions :: LogEnv -> SomeException -> IO ()
logExceptions logenv e = do
  let logError msg = runKatipT logenv $ Log.logMs ErrorS msg

  logError "failed to execute import request"

  case fromException e :: Maybe R.HttpException of
    Just (R.VanillaHttpException (HttpClient.HttpExceptionRequest _ c)) ->
      logError ("http call to " <> show wakatimeApi <> " failed: " <> show c)
    Just (R.VanillaHttpException (HttpClient.InvalidUrlException url reason)) ->
      logError ("http call was made with invalid URL " <> show url <> ": " <> show reason)
    Just (R.JsonHttpException s) ->
      logError ("json decoding failed: " <> show s)
    Nothing ->
      case fromException e :: Maybe ImportRequestException of
        Just (ConnectionError (Just s)) -> do
          logError ("failed to connect to postgres: " <> decodeUtf8 s)
          threadDelay 5000000
        Just (ConnectionError Nothing) -> do
          logError "failed to connect to postgres"
          threadDelay 5000000
        Just (InvalidToken s) ->
          logError ("invalid token was given: " <> show s)
        Just (MalformedPaylod s) ->
          logError ("malformed payload was sent: " <> show s)
        Nothing -> logError (show e)

  threadDelay 2000000

process :: QueueItem -> AppM ()
process item = do
  logFM InfoS ("processing import request for user " <> showLS (requester item))

  let payload = reqPayload item
      header = R.header "Authorization" ("Basic " <> encodeUtf8 (apiToken payload))
      allDays = genDateRange (startDate payload) (endDate payload)

  uaRes <-
    R.req
      R.GET
      (R.https wakatimeApi /: "api" /: "v1" /: "users" /: "current" /: "user_agents")
      R.NoReqBody
      R.jsonResponse
      header

  machinesRes <-
    R.req
      R.GET
      (R.https wakatimeApi /: "api" /: "v1" /: "users" /: "current" /: "machine_names")
      R.NoReqBody
      R.jsonResponse
      header

  let userAgents = (R.responseBody uaRes :: UserAgentList)
  let machineNames = (R.responseBody machinesRes :: MachineNameList)

  traverse_
    ( \day -> do
        heartbeatsRes <-
          R.req
            R.GET
            (R.https wakatimeApi /: "api" /: "v1" /: "users" /: "current" /: "heartbeats")
            R.NoReqBody
            R.jsonResponse
            (("date" =: day) <> header)

        let heartbeatList = (R.responseBody heartbeatsRes :: HeartbeatList)

        logFM InfoS ("importing " <> showLS (length $ listData heartbeatList) <> " heartbeats for day " <> showLS day)

        let heartbeats =
              convertForDb
                (requester item)
                (machineData machineNames)
                (uaData userAgents)
                (listData heartbeatList)

        pool' <- asks pool

        res <- try $ liftIO $ Db.importHeartbeats pool' (requester item) heartbeats

        either Err.logError pure res
    )
    allDays

  logFM InfoS "import completed"

convertForDb :: Text -> [MachineNamePayload] -> [UserAgentPayload] -> [ImportHeartbeatPayload] -> [HeartbeatPayload]
convertForDb user machineNames userAgents = map convertSchema
  where
    getMachineName :: [MachineNamePayload] -> Maybe Text
    getMachineName [] = Just "wakatime-import"
    getMachineName xs = Just $ machineValue $ Unsafe.head xs

    convertSchema payload =
      let userAgentValue = uaValue $ Unsafe.head $ filter (\x -> uaId x == wUser_agent_id payload) userAgents
          machineName = getMachineName $ filter (\x -> Just (machineId x) == wMachine_name_id payload) machineNames
       in HeartbeatPayload
            { branch = wBranch payload,
              category = wCategory payload,
              cursorpos = wCursorpos payload,
              dependencies = wDependencies payload,
              editor = Nothing,
              plugin = Nothing,
              platform = Nothing,
              machine = machineName,
              entity = wEntity payload,
              file_lines = wLines payload,
              is_write = wIs_write payload,
              language = wLanguage payload,
              lineno = wLineno payload,
              project = wProject payload,
              user_agent = userAgentValue,
              sender = Just user,
              time_sent = wTime payload,
              ty = wType payload
            }

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
      if null items
        then throw $ MalformedPaylod "Received empty payload list"
        else do
          case A.fromJSON (Unsafe.head items) :: A.Result QueueItem of
            A.Success item -> runAppT ctx $ process item
            A.Error e -> throw $ MalformedPaylod e

    numRetries :: Int
    numRetries = 3

    numItems :: Int
    numItems = 1

type API = ImportRequest :<|> ImportRequestStatus

type ImportRequest =
  "import"
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] ImportRequestPayload
    :> Post '[JSON] ImportRequestResponse

type ImportRequestStatus =
  "import" :> "status"
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] ImportRequestPayload
    :> Post '[JSON] ImportRequestResponse

enqueueRequest :: A.Value -> IO ()
enqueueRequest payload = do
  settings <- Cli.getDbSettings
  res <- HasqlConn.acquire settings

  case res of
    Left Nothing -> error "failed to acquire connection while enqueuing import request"
    Left (Just e) -> error $ decodeUtf8 e
    Right conn -> HasqlQueue.enqueue queueName conn E.json [payload]

type ImportRequestHandler = Maybe ApiToken -> ImportRequestPayload -> AppM ImportRequestResponse

server :: ImportRequestHandler :<|> ImportRequestHandler
server = importRequestHandler :<|> checkRequestStatusHandler

checkRequestStatusHandler :: Maybe ApiToken -> ImportRequestPayload -> AppM ImportRequestResponse
checkRequestStatusHandler Nothing _ = throw Err.missingAuthError
checkRequestStatusHandler (Just token) payload = do
  p <- asks pool

  res <- try $ liftIO $ Db.getUserByToken p token

  user <- either Err.logError pure res

  logFM InfoS ("checking pending import request for user " <> showLS user)

  let item =
        A.toJSON $
          QueueItem
            { requester = user,
              reqPayload = payload
            }

  statusResult <- try $ liftIO $ Db.getJobStatus p item

  statusMaybe <- either Err.logError pure statusResult

  let status = maybe JobFinished (\s -> if s == "failed" then JobFailed else JobPending) statusMaybe

  logFM InfoS ("import request for user " <> showLS user <> ": " <> showLS status)

  return $ ImportRequestResponse {jobStatus = status}

importRequestHandler :: Maybe ApiToken -> ImportRequestPayload -> AppM ImportRequestResponse
importRequestHandler Nothing _ = throw Err.missingAuthError
importRequestHandler (Just token) payload = do
  p <- asks pool

  userResult <- try $ liftIO $ Db.getUserByToken p token

  user <- either Err.logError pure userResult

  logFM InfoS ("received an import request from user " <> showLS user)

  let item =
        A.toJSON $
          QueueItem
            { requester = user,
              reqPayload = payload
            }

  -- Delete previous failed jobs with the same parameters.
  affectedRows <- try $ liftIO $ Db.deleteFailedJobs p item

  _ <- either Err.logError pure affectedRows

  liftIO $ enqueueRequest item

  return $
    ImportRequestResponse
      { jobStatus = JobSubmitted
      }
