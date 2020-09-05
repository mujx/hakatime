{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Haka.Badges
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as LBs
import Data.Int (Int64)
import Data.List (intercalate, mapAccumR)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.UUID.Types as UUID
import GHC.Generics
import qualified Haka.DatabaseOperations as DbOps
import qualified Haka.Errors as Err
import Haka.Types
import Katip
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media ((//))
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import Servant
import Text.Printf (printf)

-- SVG MIME type.
data SVG

instance Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG Bs.ByteString where
  mimeRender _ = LBs.fromStrict

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

  case res of
    Left e -> do
      $(logTM) ErrorS (logStr $ show e)
      throw (DbOps.toJSONError e)
    Right badgeId -> do
      return $
        BadgeResponse
          { badgeUrl = decodeUtf8 (hakaCorsUrl ss) <> "/badge/svg/" <> UUID.toText badgeId
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

  badgeRow <- either logError pure badgeInfoResult

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

  activityTime <- either logError pure timeResult

  manager <- liftIO $ newManager tlsManagerSettings
  request <-
    parseRequest
      ( "https://img.shields.io/badge/"
          <> unpack (badgeProject badgeRow)
          <> "-"
          <> compoundDuration activityTime
          <> "-blue"
      )
  response <- liftIO $ httpLbs request manager

  return $ LBs.toStrict $ responseBody response
  where
    logError e = do
      $(logTM) ErrorS (logStr $ show e)
      throw $ DbOps.toJSONError e

-- TODO: the projects table should have (user, project) as primary key.
--

reduceBy :: Integral a => a -> [a] -> [a]
n `reduceBy` xs = n' : ys where (n', ys) = mapAccumR quotRem n xs

durLabs :: [(Int64, String)]
durLabs = [(undefined, "wk"), (7, "day"), (24, "hrs"), (60, "min"), (60, "sec")]

compdurs :: Int64 -> [(Int64, String)]
compdurs t =
  let ds = t `reduceBy` map fst (tail durLabs)
   in filter ((/= 0) . fst) $ zip ds (map snd durLabs)

compoundDuration :: Int64 -> String
compoundDuration = intercalate " " . map (uncurry $ printf "%d %s") . init . compdurs
