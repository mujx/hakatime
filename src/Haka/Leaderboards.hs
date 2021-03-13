module Haka.Leaderboards
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw, try)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.List (groupBy)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Haka.AesonHelpers (noPrefixOptions)
import Haka.App (AppCtx (..), AppM)
import qualified Haka.Database as Db
import qualified Haka.Errors as Err
import Haka.Types (ApiToken (..), LeaderboardRow (..))
import qualified Haka.Utils as Utils
import Katip
import qualified Relude.Unsafe as Unsafe
import Servant

data UserTime = UserTime
  { utName :: Text,
    utValue :: Int64
  }
  deriving (Show, Generic)

instance ToJSON UserTime where
  toJSON = genericToJSON noPrefixOptions

data LeaderboardsPayload = LeaderboardsPayload
  { lGlobal :: [UserTime],
    lLang :: Map.Map Text [UserTime]
  }
  deriving (Show, Generic)

instance ToJSON LeaderboardsPayload where
  toJSON = genericToJSON noPrefixOptions

type GetLeaderboards =
  "api"
    :> "v1"
    :> "leaderboards"
    :> QueryParam "start" UTCTime
    :> QueryParam "end" UTCTime
    :> Header "Authorization" ApiToken
    :> Get '[JSON] LeaderboardsPayload

type API = GetLeaderboards

server ::
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe ApiToken ->
  AppM LeaderboardsPayload
server = getLeaderboardsHandler

getLeaderboardsHandler :: Maybe UTCTime -> Maybe UTCTime -> Maybe ApiToken -> AppM LeaderboardsPayload
getLeaderboardsHandler _ _ Nothing = throw Err.missingAuthError
getLeaderboardsHandler t0' t1' (Just token) = do
  _pool <- asks pool

  currTs <- liftIO getCurrentTime

  let (t0, t1) = case (t0', t1') of
        (Nothing, Nothing) -> (Utils.removeAMonth currTs, currTs)
        (Nothing, Just b) -> (Utils.removeAMonth b, b)
        (Just a, Nothing) -> (a, Utils.addAMonth a)
        (Just a, Just b) -> (max a (Utils.removeAYear b), b)

  userRes <- try $ liftIO $ Db.getUserByToken _pool token
  _username <- either Err.logError pure userRes

  logFM DebugS ("User " <> ls _username <> " requested leaderboards")

  leaderbRes <- try $ liftIO $ Db.getLeaderboards _pool t0 t1
  leaderRows <- either Err.logError pure leaderbRes

  let users = groupByUser leaderRows
  let langs = groupByLanguage leaderRows

  return $
    LeaderboardsPayload
      { lGlobal = mkGlobalList users,
        lLang = mkLangList langs
      }
  where
    groupByUser = groupBy (\x y -> leadSender x == leadSender y) . sortOn leadSender

    groupByLanguage = groupBy (\x y -> leadLanguage x == leadLanguage y) . sortOn leadLanguage

    mkGlobalList users =
      take 20 $
        sortBy (\a b -> compare (utValue b) (utValue a)) $
          filter (\x -> utValue x > 60) $
            map
              ( \xs ->
                  UserTime
                    { utName = leadSender $ Unsafe.head xs,
                      utValue = sum $ map leadTotalSeconds xs
                    }
              )
              users

    mkLangList langs =
      Map.fromList $
        filter (\x -> length (snd x) > 0) $
          map
            ( \xs ->
                ( leadLanguage $ Unsafe.head xs,
                  mkGlobalList (groupByUser xs)
                )
            )
            langs
