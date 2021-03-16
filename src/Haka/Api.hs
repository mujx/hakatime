module Haka.Api
  ( api,
    server,
    heartbeatApi,
  )
where

import Haka.App (AppM, ServerSettings (..))
import qualified Haka.Handlers.Authentication as Auth
import qualified Haka.Handlers.Badges as Badges
import qualified Haka.Handlers.Commits as Commits
import qualified Haka.Handlers.Heartbeats as Heartbeats
import qualified Haka.Handlers.Import as Import
import qualified Haka.Handlers.Leaderboards as Leaderboards
import qualified Haka.Handlers.Projects as Projects
import qualified Haka.Handlers.Stats as Stats
import qualified Haka.Handlers.Users as Users
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
    :<|> Badges.API
    :<|> Users.API
    :<|> Import.API
    :<|> Leaderboards.API
    :<|> Commits.API
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
    :<|> Badges.server
    :<|> Users.server
    :<|> Import.server
    :<|> Leaderboards.server
    :<|> Commits.server
    :<|> serveDirectoryFileServer (hakaDashboardPath settings)
