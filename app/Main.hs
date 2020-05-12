module Main
  ( main,
  )
where

import Control.Exception (try)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bs
import qualified Haka.Authentication as Auth
import qualified Haka.Cli as Cli
import qualified Haka.Heartbeats as Heartbeats
import qualified Haka.Projects as Projects
import qualified Haka.Stats as Stats
import Haka.Types (AppCtx (..), AppM, LogState (..), runAppT)
import qualified Hasql.Pool as HasqlPool
import Katip
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import qualified Options.Applicative as Opt
import Servant
import System.Environment.MrEnv (envAsInt, envAsString)
import System.IO (stdout)

type Static = Raw

-- Combined API type for each sub-api available.
type HakaAPI =
  Heartbeats.API
    :<|> Stats.API
    :<|> Projects.API
    :<|> Auth.API
    :<|> Static

-- The API handlers should be presented in the same order as in the API type.
server :: ServerSettings -> ServerT HakaAPI AppM
server settings =
  Heartbeats.server
    :<|> Stats.server
    :<|> Projects.server
    :<|> Auth.server
    :<|> serveDirectoryFileServer (hakaDashboardPath settings)

api :: Proxy HakaAPI
api = Proxy

-- | Convert our 'App' type to a 'Servant.Handler', for a given 'AppCtx'.
nt :: AppCtx -> AppM a -> Handler a
nt ctx = Handler . ExceptT . try . runAppT ctx

app :: ServerSettings -> AppCtx -> Application
app settings conf =
  cors (const $ Just policy)
    $ serve api
    $ hoistServer api (nt conf) (server settings)
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
              logState = logState'
            }
      )

-- | Server configuration settings.
data ServerSettings
  = ServerSettings
      { -- | Where the service will listen to.
        hakaPort :: Int,
        -- | What domain to allow.
        hakaCorsUrl :: ByteString,
        -- | Where to look for dashboard's static files.
        hakaDashboardPath :: FilePath
      }

getServerSettings :: IO ServerSettings
getServerSettings = do
  hPort <- envAsInt "HAKA_PORT" 8080
  hCorsUrl <- Bs.pack <$> envAsString "HAKA_CORS_URL" "http://localhost:8080"
  hDashboardPath <- envAsString "HAKA_DASHBOARD_PATH" "./dashboard/dist"
  return $
    ServerSettings
      { hakaPort = hPort,
        hakaCorsUrl = hCorsUrl,
        hakaDashboardPath = hDashboardPath
      }

main :: IO ()
main = do
  parsedOpts <- Opt.execParser Cli.opts
  srvSettings <- getServerSettings
  Cli.handleCommand (Cli.serverCmd parsedOpts) (initApp srvSettings (app srvSettings))
