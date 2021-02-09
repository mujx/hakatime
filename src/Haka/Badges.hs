{-# LANGUAGE MultiParamTypeClasses #-}

module Haka.Badges
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as Bs
import qualified Data.UUID.Types as UUID
import Haka.App (AppCtx (..), AppM, ServerSettings (..))
import qualified Haka.DatabaseOperations as DbOps
import qualified Haka.Errors as Err
import Haka.Types (ApiToken (..), BadgeRow (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media ((//))
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import qualified Relude.Unsafe as Unsafe
import Servant
import Text.Printf (printf)

-- SVG MIME type.
data SVG

instance Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG Bs.ByteString where
  mimeRender _ = fromStrict

type API = GetBadgeLink :<|> GetBadgeSvg

server ::
  (Text -> Maybe ApiToken -> AppM BadgeResponse)
    :<|> (UUID.UUID -> Maybe Int64 -> AppM Bs.ByteString)
server = badgeLinkHandler :<|> badgeSvgHandler

newtype BadgeResponse = BadgeResponse
  { badgeUrl :: Text
  }
  deriving (Generic, Show)

instance FromJSON BadgeResponse

instance ToJSON BadgeResponse

type GetBadgeLink =
  "badge"
    :> "link"
    :> Capture "project" Text
    :> Header "Authorization" ApiToken
    :> Get '[JSON] BadgeResponse

type GetBadgeSvg =
  "badge"
    :> "svg"
    :> Capture "svg" UUID.UUID
    :> QueryParam "days" Int64
    :> Get '[SVG] Bs.ByteString

badgeLinkHandler :: Text -> Maybe ApiToken -> AppM BadgeResponse
badgeLinkHandler _ Nothing = throw Err.missingAuthError
badgeLinkHandler proj (Just tkn) = do
  p <- asks pool
  ss <- asks srvSettings
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.mkBadgeLink p proj tkn

  badgeId <- either Err.logError pure res

  return $
    BadgeResponse
      { badgeUrl = decodeUtf8 (hakaBadgeUrl ss) <> "/badge/svg/" <> UUID.toText badgeId
      }

badgeSvgHandler :: UUID.UUID -> Maybe Int64 -> AppM Bs.ByteString
badgeSvgHandler badgeId daysParam = do
  p <- asks pool

  badgeInfoResult <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getBadgeLinkInfo p badgeId

  badgeRow <- either Err.logError pure badgeInfoResult

  timeResult <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getTotalActivityTime
          p
          (badgeUsername badgeRow)
          (fromMaybe 7 daysParam)
          (badgeProject badgeRow)

  activityTime <- either Err.logError pure timeResult

  ss <- asks srvSettings

  manager <- liftIO $ newManager tlsManagerSettings
  request <-
    parseRequest
      ( hakaShieldsIOUrl ss
          <> "/static/v1?"
          <> "label="
          <> toString (badgeProject badgeRow)
          <> ("&message=" :: String)
          <> toString (compoundDuration activityTime)
          <> "&color=blue"
      )
  response <- liftIO $ httpLbs request manager

  return $ toStrict $ responseBody response

reduceBy :: Integral a => a -> [a] -> [a]
n `reduceBy` xs = n' : ys where (n', ys) = mapAccumR quotRem n xs

durLabs :: [(Int64, Text)]
durLabs = [(0, "wk"), (7, "day"), (24, "hrs"), (60, "min"), (60, "sec")]

computeDurations :: Int64 -> [(Int64, Text)]
computeDurations t =
  let ds = t `reduceBy` map fst (Unsafe.tail durLabs)
   in filter ((/= 0) . fst) $ zip ds (map snd durLabs)

compoundDuration :: Maybe Int64 -> Text
compoundDuration Nothing = "no data"
compoundDuration (Just v) =
  let durations = computeDurations v
   in if length durations > 0
        then unwords $ map (toText . \(n, s) -> printf "%d %s" n s :: String) $ Unsafe.init durations
        else "no data"
