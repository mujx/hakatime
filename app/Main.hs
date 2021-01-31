module Main
  ( main,
  )
where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT (..))
import qualified GHC.IO.Encoding
import qualified Haka.Api as Api
import qualified Haka.Cli as Cli
import qualified Haka.Middleware as Middleware
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
  badgeUrl <- getEnvDefault "HAKA_BADGE_URL" "http://localhost:8080"
  dashboardPath <- envAsString "HAKA_DASHBOARD_PATH" "./dashboard/dist"
  apiPrefix <- envAsString "HAKA_API_PREFIX" ""
  shieldsIOUrl <- envAsString "HAKA_SHIELDS_IO_URL" "https://img.shields.io"
  enableRegistration <- envAsBool "HAKA_ENABLE_REGISTRATION" True
  sessionExpiry <- envAsInt "HAKA_SESSION_EXPIRY" 24
  when (sessionExpiry <= 0) (error "Session expiry should be a positive integer")
  return $
    ServerSettings
      { hakaPort = p,
        hakaApiPrefix = apiPrefix,
        hakaBadgeUrl = badgeUrl,
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
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

  parsedOpts <- Opt.execParser Cli.opts
  s <- getServerSettings
  Cli.handleCommand (Cli.serverCmd parsedOpts) (initApp s (app s))
