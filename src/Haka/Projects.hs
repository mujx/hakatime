{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Haka.Projects
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time (addDays, diffDays)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import GHC.Generics
import Haka.AesonHelpers (noPrefixOptions)
import qualified Haka.DatabaseOperations as DbOps
import Haka.Errors (missingAuthError)
import qualified Haka.Errors as Err
import Haka.Types (ApiToken (..), AppM, ProjectStatRow (..), pool)
import Haka.Utils (defaultLimit)
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import PostgreSQL.Binary.Data (Scientific)
import Servant

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

data ProjectStatistics = ProjectStatistics
  { -- The first day in the range (inclusive).
    startDate :: UTCTime,
    -- The last day in the range (inclusive).
    endDate :: UTCTime,
    -- | Total coding activity as seconds for the given range of time.
    totalSeconds :: Int64,
    -- | Total coding activity for each day.
    dailyTotal :: [Int64],
    -- Statistics for all each language separately.
    languages :: [ResourceStats],
    -- Statistics for all each language separately.
    files :: [ResourceStats],
    -- Statistics for all each day of the week.
    weekDay :: [ResourceStats],
    -- Statistics for all each time of the day
    hour :: [ResourceStats]
  }
  deriving (Show, Generic)

instance ToJSON ProjectStatistics

instance FromJSON ProjectStatistics

type API = ProjectStats

type ProjectStats =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "projects"
    :> Capture "project" Text
    :> QueryParam "start" UTCTime
    :> QueryParam "end" UTCTime
    :> QueryParam "timeLimit" Int64
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ProjectStatistics

server ::
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int64 ->
  Maybe ApiToken ->
  AppM ProjectStatistics
server _ _ _ _ Nothing = throw missingAuthError
server project t0Param t1Param timeLimit (Just token) = do
  t1def <- liftIO getCurrentTime
  p <- asks pool
  let (t0, t1) = case (t0Param, t1Param) of
        (Nothing, Nothing) -> (removeAWeek t1def, t1def)
        (Nothing, Just b) -> (removeAWeek b, b)
        (Just a, Nothing) -> (a, addAWeek a)
        (Just a, Just b) -> (max a (removeAYear b), b)
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.genProjectStatistics p token project (fromMaybe defaultLimit timeLimit) (t0, t1)

  rows <- either Err.logError pure res

  return $ toStatsPayload t0 t1 rows
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

-- | Generate a statistics payload object from the statistics rows.
toStatsPayload :: UTCTime -> UTCTime -> [ProjectStatRow] -> ProjectStatistics
toStatsPayload t0 t1 xs =
  ProjectStatistics
    { totalSeconds = allSecs,
      startDate = t0,
      endDate = t1,
      dailyTotal = map (sum . map prTotalSeconds) byDate,
      languages = getSegment prLanguage,
      files = getSegment prEntity,
      weekDay = getSegment prWeekday,
      hour = getSegment prHour
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
                      unzip3 $ map (\(_, m') -> Data.Maybe.fromMaybe (0, 0, 0) (Map.lookup name m')) all'
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
    allSecs = sum $ [prTotalSeconds x | x <- xs]
    byDate :: [[ProjectStatRow]]
    byDate = fillMissing (genDates t0 t1) (List.groupBy (\a b -> prDay a == prDay b) xs)

fillMissing :: [UTCTime] -> [[ProjectStatRow]] -> [[ProjectStatRow]]
fillMissing times rows = go times rows ([] :: [[ProjectStatRow]])
  where
    go :: [UTCTime] -> [[ProjectStatRow]] -> [[ProjectStatRow]] -> [[ProjectStatRow]]
    go [] _ res = res
    go _ [] res = res
    go (t : ts) rows'@(r : rs) res =
      if utctDay t == utctDay (prDay $ head r)
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

type RangePct = Scientific

type DailyPct = Scientific

type CalcStatistics = (Int64, RangePct, DailyPct)

aggregateBy ::
  -- | Which field to extract from the row.
  (ProjectStatRow -> Text) ->
  -- | All the rows for a particular day.
  [ProjectStatRow] ->
  -- | Total seconds of activity per field for that day.
  Maybe (UTCTime, Map.Map Text CalcStatistics)
-- aggregateBy _ [] = Nothing
aggregateBy f rows = Just (prDay $ head rows, go rows Map.empty)
  where
    go :: [ProjectStatRow] -> Map.Map Text CalcStatistics -> Map.Map Text CalcStatistics
    go [] m' = m'
    go (x : xs) m' =
      go
        xs
        ( Map.insertWith
            ( \(a', b', c') (a, b, c) -> (a' + a, b' + b, c' + c)
            )
            (f x)
            (prTotalSeconds x, prPct x, prDailyPct x)
            m'
        )
