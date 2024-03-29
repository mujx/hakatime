module Main
  ( main,
  )
where

import Control.Concurrent (forkIO)
import Control.Exception.Safe (catchAny, try)
import qualified GHC.IO.Encoding
import qualified Haka.Api as Api
import Haka.App
  ( AppCtx (..),
    AppM,
    LogState (..),
    RegistrationStatus (..),
    RemoteWriteConfig (..),
    ServerSettings (..),
    runAppT,
  )
import qualified Haka.Cli as Cli
import qualified Haka.Handlers.Import as Import
import qualified Haka.Logger as Log
import qualified Haka.Middleware as Middleware
import qualified Hasql.Connection as HasqlConn
import qualified Hasql.Pool as HasqlPool
import Hasql.Queue.Migrate (migrate)
import Katip
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import qualified Options.Applicative as Opt
import Servant
import System.Environment.MrEnv (envAsBool, envAsInt, envAsString)
import System.Posix.Env.ByteString (getEnvDefault)

-- | Convert our 'App' type to a 'Servant.Handler', for a given 'AppCtx'.
nt :: AppCtx -> AppM a -> Handler a
nt ctx = Handler . ExceptT . try . runAppT ctx

app :: ServerSettings -> AppCtx -> Application
app settings conf =
  Middleware.jsonResponse $
    cors (const $ Just policy) $
      serve Api.api $
        hoistServer Api.api (nt conf) (Api.server settings)
  where
    policy = simpleCorsResourcePolicy

initApp :: ServerSettings -> (AppCtx -> Application) -> IO ()
initApp settings unApp = do
  dbSettings <- Cli.getDbSettings
  dbPool <- HasqlPool.acquire 10 Nothing dbSettings

  -- Set up the db schema for the postgres queue.
  res <- HasqlConn.acquire dbSettings
  case res of
    Left e -> do
      let errMsg = "Failed to connect to the database:\n"
      case e of
        Just errS -> die $ errMsg <> decodeUtf8 errS
        Nothing -> die errMsg
    Right conn -> migrate conn "json"

  logenv <-
    Log.setupLogEnv
      (hakaRunEnv settings)
      (fromMaybe InfoS (textToSeverity $ toText $ hakaLogLevel settings))
  let logState' =
        LogState
          { lsNamespace = Namespace {unNamespace = ["api"]},
            lsLogEnv = logenv,
            lsContext = mempty
          }
      appCtx =
        AppCtx
          { pool = dbPool,
            logState = logState',
            srvSettings = settings
          }

  -- Handle import requests on another thread.
  _ <-
    forkIO $
      forever $
        runAppT appCtx Import.handleImportRequest `catchAny` Import.logExceptions logenv

  if hakaHasHttpLogger settings
    then do
      withStdoutLogger $ \logger -> do
        let conf = setPort (hakaPort settings) $ setLogger logger defaultSettings
        runSettings conf (unApp appCtx)
    else do
      run (hakaPort settings) (unApp appCtx)

emptyStr :: String
emptyStr = ""

getServerSettings :: IO ServerSettings
getServerSettings = do
  p <- envAsInt "HAKA_PORT" 8080
  badgeUrl <- getEnvDefault "HAKA_BADGE_URL" "http://localhost:8080"
  dashboardPath <- envAsString "HAKA_DASHBOARD_PATH" "./dashboard/dist"
  apiPrefix <- envAsString "HAKA_API_PREFIX" emptyStr
  shieldsIOUrl <- envAsString "HAKA_SHIELDS_IO_URL" "https://img.shields.io"
  enableRegistration <- envAsBool "HAKA_ENABLE_REGISTRATION" True
  sessionExpiry <- envAsInt "HAKA_SESSION_EXPIRY" 24
  logLevel <- envAsString "HAKA_LOG_LEVEL" "info"
  rEnv <- envAsString "HAKA_ENV" "prod"
  enableHttpLog <- envAsBool "HAKA_HTTP_LOG" True
  remoteWriteUrl <- envAsString "HAKA_REMOTE_WRITE_URL" emptyStr
  remoteWriteToken <- envAsString "HAKA_REMOTE_WRITE_TOKEN" emptyStr
  when (sessionExpiry <= 0) (error "Session expiry should be a positive integer")
  return $
    ServerSettings
      { hakaPort = p,
        hakaApiPrefix = apiPrefix,
        hakaBadgeUrl = badgeUrl,
        hakaDashboardPath = dashboardPath,
        hakaShieldsIOUrl = shieldsIOUrl,
        hakaLogLevel = logLevel,
        hakaHasHttpLogger = enableHttpLog,
        hakaRunEnv = case rEnv of
          "prod" -> Log.Prod
          "production" -> Log.Prod
          _ -> Log.Dev,
        hakaRemoteWriteConfig =
          if remoteWriteUrl /= emptyStr && remoteWriteToken /= emptyStr
            then
              Just $
                RemoteWriteConfig
                  { heartbeatUrl = toText remoteWriteUrl,
                    token = toText remoteWriteToken
                  }
            else Nothing,
        hakaEnableRegistration =
          if enableRegistration
            then EnabledRegistration
            else DisabledRegistration,
        hakaSessionExpiry = fromIntegral sessionExpiry
      }

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

  parsedOpts <- Opt.execParser Cli.opts
  s <- getServerSettings
  Cli.handleCommand (Cli.serverCmd parsedOpts) (initApp s (app s))
