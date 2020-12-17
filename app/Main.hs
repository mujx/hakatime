module Main
  ( main,
  )
where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.ByteString.Char8 as Bs
import qualified Haka.Api as Api
import qualified Haka.Cli as Cli
import Haka.Types
  ( AppCtx (..),
    AppM,
    LogState (..),
    RegistrationStatus (..),
    ServerSettings (..),
    runAppT,
  )
import qualified Hasql.Pool as HasqlPool
import Katip
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import qualified Options.Applicative as Opt
import Servant
import System.Environment.MrEnv (envAsBool, envAsInt, envAsString)
import System.IO (stdout)
import qualified Haka.Middleware as Middleware

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
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type", "authorization"],
          -- TODO: Make this list configurable.
          corsOrigins = Just ([hakaCorsUrl settings], True)
        }

-- TODO: Write method to initialize logging based on ENV variables.
-- Env
-- LogLevel
-- Verbosity

initApp :: ServerSettings -> (AppCtx -> Application) -> IO ()
initApp settings unApp = do
  dbsettings <- Cli.getDbSettings
  dbPool <- HasqlPool.acquire (10, 1, dbsettings)
  let ns = Namespace {unNamespace = ["server"]}
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  logEnv' <- registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "hakatime" "dev"
  let logState' =
        LogState
          { lsNamespace = ns,
            lsLogEnv = logEnv',
            lsContext = mempty
          }
  withStdoutLogger $ \logger -> do
    let conf = setPort (hakaPort settings) $ setLogger logger defaultSettings
    runSettings
      conf
      ( unApp
          AppCtx
            { pool = dbPool,
              logState = logState',
              srvSettings = settings
            }
      )

getServerSettings :: IO ServerSettings
getServerSettings = do
  p <- envAsInt "HAKA_PORT" 8080
  corsUrl <- Bs.pack <$> envAsString "HAKA_CORS_URL" "http://localhost:8080"
  dashboardPath <- envAsString "HAKA_DASHBOARD_PATH" "./dashboard/dist"
  shieldsIOUrl <- envAsString "HAKA_SHIELDS_IO_URL" "https://img.shields.io"
  enableRegistration <- envAsBool "HAKA_ENABLE_REGISTRATION" True
  sessionExpiry <- envAsInt "HAKA_SESSION_EXPIRY" 24
  when (sessionExpiry <= 0) (error "Session expiry should be a positive integer")
  return $
    ServerSettings
      { hakaPort = p,
        hakaCorsUrl = corsUrl,
        hakaDashboardPath = dashboardPath,
        hakaShieldsIOUrl = shieldsIOUrl,
        hakaEnableRegistration =
          if enableRegistration
            then EnabledRegistration
            else DisabledRegistration,
        hakaSessionExpiry = fromIntegral sessionExpiry
      }

main :: IO ()
main = do
  parsedOpts <- Opt.execParser Cli.opts
  s <- getServerSettings
  Cli.handleCommand (Cli.serverCmd parsedOpts) (initApp s (app s))
