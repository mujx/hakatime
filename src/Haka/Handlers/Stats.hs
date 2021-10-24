{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Haka.Handlers.Stats
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw, try)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Time (addDays, diffDays, diffUTCTime)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Haka.AesonHelpers (noPrefixOptions)
import Haka.App (AppCtx (..), AppM)
import qualified Haka.Database as Db
import Haka.Errors (missingAuthError)
import qualified Haka.Errors as Err
import Haka.Types (ApiToken (..), StatRow (..), TimelineRow (..))
import Haka.Utils (compoundDuration, defaultLimit)
import Katip
import PostgreSQL.Binary.Data (Scientific)
import qualified Relude.Unsafe as Unsafe
import Servant

newtype DayTextValue = DayTextValue
  { tText :: Text
  }
  deriving (Show, Generic)

data DayGrandTotal = DayGrandTotal
  { tGrand_total :: DayTextValue,
    tCategories :: [Text]
  }
  deriving (Show, Generic)

newtype StatusBarPayload = StatusBarPayload
  { tData :: DayGrandTotal
  }
  deriving (Show, Generic)

instance ToJSON StatusBarPayload where
  toJSON = genericToJSON noPrefixOptions

instance ToJSON DayGrandTotal where
  toJSON = genericToJSON noPrefixOptions

instance ToJSON DayTextValue where
  toJSON = genericToJSON noPrefixOptions

data ResourceStats = ResourceStats
  { -- | The name of the resource.
    pName :: Text,
    -- | Total number of seconds spent on the resource for the time range.
    pTotalSeconds :: Int64,
    -- | Percentage of the total time in the range spent on the resource.
    pTotalPct :: Scientific,
    -- | Total seconds spend on the project for each day in the range.
    pTotalDaily :: [Int64],
    -- | Percentage of the day spent on the resource for each day in the range.
    pPctDaily :: [Scientific]
  }
  deriving (Show, Generic)

instance ToJSON ResourceStats where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON ResourceStats where
  parseJSON = genericParseJSON noPrefixOptions

-- TODO: Move data types to a separate module
data StatsPayload = StatsPayload
  { -- The first day in the range (inclusive).
    startDate :: UTCTime,
    -- The last day in the range (inclusive).
    endDate :: UTCTime,
    -- | Total coding activity as seconds for the given range of time.
    totalSeconds :: Int64,
    -- | Average coding activity per day as seconds for the given range of time.
    dailyAvg :: Double,
    -- | Total coding activity for each day.
    dailyTotal :: [Int64],
    -- Statistics for all each project separately.
    projects :: [ResourceStats],
    -- Statistics for all each language separately.
    languages :: [ResourceStats],
    -- Statistics for all each platform separately.
    platforms :: [ResourceStats],
    -- Statistics for all each machine separately.
    machines :: [ResourceStats],
    -- Statistics for all each editor separately.
    editors :: [ResourceStats]
  }
  deriving (Show, Generic)

instance ToJSON StatsPayload

instance FromJSON StatsPayload

data TimelineItem = TimelineItem
  { tName :: Text,
    tRangeStart :: UTCTime,
    tRangeEnd :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON TimelineItem

instance FromJSON TimelineItem

newtype TimelinePayload = TimelinePayload
  { timelineLangs :: Map.Map Text [TimelineItem]
  }
  deriving (Show, Generic)

instance ToJSON TimelinePayload

instance FromJSON TimelinePayload

type TimelineStats =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "timeline"
    :> QueryParam "start" UTCTime
    :> QueryParam "end" UTCTime
    :> QueryParam "timeLimit" Int64
    :> Header "Authorization" ApiToken
    :> Get '[JSON] TimelinePayload

type TotalStats =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "stats"
    :> QueryParam "start" UTCTime
    :> QueryParam "end" UTCTime
    :> QueryParam "tag" Text
    :> QueryParam "timeLimit" Int64
    :> Header "Authorization" ApiToken
    :> Get '[JSON] StatsPayload

type TodayTime =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "statusbar"
    :> "today"
    :> Header "Authorization" ApiToken
    :> Get '[JSON] StatusBarPayload

type API = TotalStats :<|> TimelineStats :<|> TodayTime

server = statsHandler :<|> timelineStatsHandler :<|> todayTimeHandler

defaultTimeRange :: Maybe UTCTime -> Maybe UTCTime -> IO (UTCTime, UTCTime)
defaultTimeRange t0 t1 = do
  now <- getCurrentTime
  case (t0, t1) of
    (Nothing, Nothing) -> pure (removeAWeek now, now)
    (Nothing, Just b) -> pure (removeAWeek b, b)
    (Just a, Nothing) -> pure (a, addAWeek a)
    (Just a, Just b) -> pure (max a (removeAYear b), b)
  where
    removeAWeek, removeAYear, addAWeek :: UTCTime -> UTCTime
    removeAWeek t =
      UTCTime
        { utctDay = addDays (-7) (utctDay t),
          utctDayTime = 0
        }
    removeAYear t =
      UTCTime
        { utctDay = addDays (-365) (utctDay t),
          utctDayTime = 0
        }
    addAWeek t =
      UTCTime
        { utctDay = addDays 7 (utctDay t),
          utctDayTime = 0
        }

timelineStatsHandler ::
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int64 ->
  Maybe ApiToken ->
  AppM TimelinePayload
timelineStatsHandler _ _ _ Nothing = throw missingAuthError
timelineStatsHandler t0Param t1Param timeLimit (Just token) = do
  p <- asks pool
  (t0, t1) <- liftIO $ defaultTimeRange t0Param t1Param
  res <- try $ liftIO $ Db.getTimeline p token (fromMaybe defaultLimit timeLimit) (t0, t1)

  rows <- either Err.logError pure res

  return $ mkTimelinePayload rows
  where
    mkTimelinePayload :: [TimelineRow] -> TimelinePayload
    mkTimelinePayload rows =
      TimelinePayload
        { timelineLangs = go Map.empty rows
        }
    secondsSince :: UTCTime -> UTCTime -> Integer
    secondsSince t0 t1 = i
      where
        (i, _) = properFraction $ diffUTCTime t1 t0
    go :: Map.Map Text [TimelineItem] -> [TimelineRow] -> Map.Map Text [TimelineItem]
    go m [] = m
    go m (x : xs) =
      let key = tmLang x
          value =
            TimelineItem
              { tName = tmProject x,
                tRangeStart = tmRangeStart x,
                tRangeEnd = tmRangeEnd x
              }
       in if secondsSince (tmRangeStart x) (tmRangeEnd x) < 60
            then go m xs
            else case Map.lookup key m of
              Just v -> go (Map.insert key (v ++ [value]) m) xs
              Nothing -> go (Map.insert key [value] m) xs

todayTimeHandler :: Maybe ApiToken -> AppM StatusBarPayload
todayTimeHandler Nothing = throw missingAuthError
todayTimeHandler (Just apiTkn) = do
  let ctx = sl "day" ("today" :: Text)

  logF ctx "statusbar" DebugS "requesting today's statusbar activity"

  dbPool <- asks pool

  userRes <- try $ liftIO $ Db.getUser dbPool apiTkn

  user <- either Err.logError pure userRes

  case user of
    Nothing -> throw Err.invalidTokenError
    Just user' -> do
      res <- try $ liftIO $ Db.getTotalTimeToday dbPool user'

      totalTime <- either Err.logError pure res

      return $
        StatusBarPayload
          { tData =
              DayGrandTotal
                { tCategories = [],
                  tGrand_total =
                    DayTextValue
                      { tText = compoundDuration (Just totalTime)
                      }
                }
          }

statsHandler ::
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Int64 ->
  Maybe ApiToken ->
  AppM StatsPayload
statsHandler _ _ _ _ Nothing = throw missingAuthError
statsHandler t0Param t1Param tagName timeLimit (Just token) = do
  let ctx = sl "t0" t0Param <> sl "t1" t1Param <> sl "tag" tagName <> sl "limit" timeLimit

  logF ctx "stats" DebugS "total user activity request"

  p <- asks pool
  (t0, t1) <- liftIO $ defaultTimeRange t0Param t1Param
  res <- try $ liftIO $ Db.generateStatistics p token (fromMaybe defaultLimit timeLimit) tagName (t0, t1)

  rows <- either Err.logError pure res

  return $ toStatsPayload t0 t1 rows

fillMissing :: [UTCTime] -> [[StatRow]] -> [[StatRow]]
fillMissing times rows = go times rows ([] :: [[StatRow]])
  where
    go :: [UTCTime] -> [[StatRow]] -> [[StatRow]] -> [[StatRow]]
    go [] _ res = res
    go _ [] res = res
    go (t : ts) rows'@(r : rs) res =
      if utctDay t == utctDay (rDay $ Unsafe.head r)
        then go ts rs (res ++ [r])
        else go ts rows' (res ++ [[]])

genDates :: UTCTime -> UTCTime -> [UTCTime]
genDates t0 t1 =
  [ UTCTime
      { utctDay = addDays d (utctDay t0),
        utctDayTime = 0
      }
    | d <- [0 .. (diffDays (utctDay t1) (utctDay t0))]
  ]

-- | Generate a statistics payload object from the statistics rows.
toStatsPayload :: UTCTime -> UTCTime -> [StatRow] -> StatsPayload
toStatsPayload t0 t1 xs =
  StatsPayload
    { totalSeconds = allSecs,
      startDate = t0,
      endDate = t1,
      dailyAvg = dailyAvgSecs,
      dailyTotal = map (sum . map rTotalSeconds) byDate,
      projects = getSegment rProject,
      editors = getSegment rEditor,
      languages = getSegment rLanguage,
      platforms = getSegment rPlatform,
      machines = getSegment rMachine
    }
  where
    getSegment fn =
      let all' :: [(UTCTime, Map.Map Text CalcStatistics)]
          all' = mapMaybe (aggregateBy fn) byDate
          uniqProjectNames :: [Text]
          uniqProjectNames = List.nub $ concatMap ((map fst . Map.toList) . snd) all'
       in map
            ( \name ->
                let (secs', pct', dailyPct') =
                      unzip3 $ map (\(_, m') -> fromMaybe (0, 0, 0) (Map.lookup name m')) all'
                 in ResourceStats
                      { pName = name,
                        pTotalSeconds = sum secs',
                        pTotalPct = sum pct',
                        pTotalDaily = secs',
                        pPctDaily = dailyPct'
                      }
            )
            uniqProjectNames
    allSecs :: Int64
    allSecs = sum $ [rTotalSeconds x | x <- xs]
    dailyAvgSecs :: Double
    dailyAvgSecs = fromIntegral allSecs / numOfDays
    byDate :: [[StatRow]]
    byDate = fillMissing (genDates t0 t1) (List.groupBy (\a b -> rDay a == rDay b) xs)
    numOfDays :: Double
    numOfDays = fromIntegral $ length byDate

type RangePct = Scientific

type DailyPct = Scientific

type CalcStatistics = (Int64, RangePct, DailyPct)

aggregateBy ::
  -- | Which field to extract from the row.
  (StatRow -> Text) ->
  -- | All the rows for a particular day.
  [StatRow] ->
  -- | Total seconds of activity per field for that day.
  Maybe (UTCTime, Map.Map Text CalcStatistics)
-- aggregateBy _ [] = Nothing
aggregateBy f rows = Just (rDay $ Unsafe.head rows, go rows Map.empty)
  where
    go :: [StatRow] -> Map.Map Text CalcStatistics -> Map.Map Text CalcStatistics
    go [] m' = m'
    go (x : xs) m' =
      go
        xs
        ( Map.insertWith
            ( \(a', b', c') (a, b, c) -> (a' + a, b' + b, c' + c)
            )
            (f x)
            (rTotalSeconds x, rPct x, rDailyPct x)
            m'
        )
