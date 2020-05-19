module Haka.Api
  ( api,
    server,
    heartbeatApi,
  )
where

import qualified Haka.Authentication as Auth
import qualified Haka.Heartbeats as Heartbeats
import qualified Haka.Projects as Projects
import qualified Haka.Stats as Stats
import Haka.Types (AppM, ServerSettings (..))
import Servant

type Static = Raw

heartbeatApi :: Proxy Heartbeats.API
heartbeatApi = Proxy

-- Combined API type for each sub-api available.
type HakaAPI =
  Heartbeats.API
    :<|> Stats.API
    :<|> Projects.API
    :<|> Auth.API
    :<|> Static

api :: Proxy HakaAPI
api = Proxy

-- The API handlers should be presented in the same order as in the API type.
server :: ServerSettings -> ServerT HakaAPI AppM
server settings =
  Heartbeats.server
    :<|> Stats.server
    :<|> Projects.server
    :<|> Auth.server (hakaEnableRegistration settings)
    :<|> serveDirectoryFileServer (hakaDashboardPath settings)
