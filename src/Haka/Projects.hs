{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Haka.Projects
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw, try)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Time (addDays, diffDays)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import qualified Data.Vector as V
import Haka.AesonHelpers (noPrefixOptions)
import Haka.App (AppCtx (..), AppM)
import qualified Haka.Database as Db
import Haka.Errors (missingAuthError)
import qualified Haka.Errors as Err
import Haka.Types (ApiToken (..), Project (..), ProjectStatRow (..), StoredUser (..))
import Haka.Utils (defaultLimit)
import Katip
import PostgreSQL.Binary.Data (Scientific)
import qualified Relude.Unsafe as Unsafe
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

newtype TagsPayload = TagsPayload
  { tags :: V.Vector Text
  }
  deriving (Show, Generic)

instance FromJSON TagsPayload

instance ToJSON TagsPayload

newtype ProjectListPayload = ProjectListPayload
  { projects :: V.Vector Text
  }
  deriving (Show, Generic)

instance ToJSON ProjectListPayload

type API =
  ProjectStats
    :<|> SetProjectTags
    :<|> GetProjectTags
    :<|> GetUserTags
    :<|> ProjectList

type SetProjectTags =
  "api"
    :> "v1"
    :> "projects"
    :> Capture "project" Text
    :> "tags"
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] TagsPayload
    :> PostNoContent

type GetProjectTags =
  "api"
    :> "v1"
    :> "projects"
    :> Capture "project" Text
    :> "tags"
    :> Header "Authorization" ApiToken
    :> Get '[JSON] TagsPayload

type GetUserTags =
  "api"
    :> "v1"
    :> "tags"
    :> Header "Authorization" ApiToken
    :> Get '[JSON] TagsPayload

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

type ProjectList =
  "api"
    :> "v1"
    :> "projects"
    :> QueryParam "start" UTCTime
    :> QueryParam "end" UTCTime
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ProjectListPayload

server =
  projectStatsHandler
    :<|> setTagsHandler
    :<|> getTagsHandler
    :<|> getUserTagsHandler
    :<|> getProjectList

getProjectList :: Maybe UTCTime -> Maybe UTCTime -> Maybe ApiToken -> AppM ProjectListPayload
getProjectList _ _ Nothing = throw Err.missingAuthError
getProjectList t0' t1' (Just token) = do
  currTs <- liftIO getCurrentTime
  _pool <- asks pool

  let (t0, t1) = case (t0', t1') of
        (Nothing, Nothing) -> (removeAMonth currTs, currTs)
        (Nothing, Just b) -> (removeAMonth b, b)
        (Just a, Nothing) -> (a, addAMonth a)
        (Just a, Just b) -> (max a (removeAYear b), b)

  dbResult <- try $ liftIO $ Db.getUserProjects _pool token t0 t1
  allProjects <- either Err.logError pure dbResult

  return $ ProjectListPayload {projects = allProjects}

getUserTagsHandler :: Maybe ApiToken -> AppM TagsPayload
getUserTagsHandler Nothing = throw Err.missingAuthError
getUserTagsHandler (Just token) = do
  _pool <- asks pool

  dbResult <- try $ liftIO $ Db.getUserTags _pool token
  allTags <- either Err.logError pure dbResult

  return $ TagsPayload {tags = allTags}

getTagsHandler :: Text -> Maybe ApiToken -> AppM TagsPayload
getTagsHandler _ Nothing = throw Err.missingAuthError
getTagsHandler project (Just token) = do
  _pool <- asks pool

  dbResult <- try $ liftIO $ Db.validateUserAndProject _pool token (Project project)
  user <- either Err.logError pure dbResult

  dbResult' <- try $ liftIO $ Db.getTags _pool user (Project project)
  retrievedTags <- either Err.logError pure dbResult'

  return $ TagsPayload {tags = retrievedTags}

setTagsHandler :: Text -> Maybe ApiToken -> TagsPayload -> AppM NoContent
setTagsHandler _ Nothing _ = throw Err.missingAuthError
setTagsHandler project (Just token) tagsPayload = do
  _pool <- asks pool

  dbResult <- try $ liftIO $ Db.validateUserAndProject _pool token (Project project)
  user@(StoredUser username) <- either Err.logError pure dbResult

  logFM
    InfoS
    ( "setting tags "
        <> showLS (tags tagsPayload)
        <> " to "
        <> ls username
        <> "/"
        <> ls project
    )

  dbResult' <- try $ liftIO $ Db.setTags _pool user (Project project) (tags tagsPayload)
  tagsNum <- either Err.logError pure dbResult'

  logFM InfoS ("inserted " <> showLS tagsNum <> " tags on " <> ls username <> "/" <> ls project)

  return NoContent

projectStatsHandler ::
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int64 ->
  Maybe ApiToken ->
  AppM ProjectStatistics
projectStatsHandler _ _ _ _ Nothing = throw missingAuthError
projectStatsHandler project t0Param t1Param timeLimit (Just token) = do
  t1def <- liftIO getCurrentTime
  p <- asks pool
  let (t0, t1) = case (t0Param, t1Param) of
        (Nothing, Nothing) -> (removeAWeek t1def, t1def)
        (Nothing, Just b) -> (removeAWeek b, b)
        (Just a, Nothing) -> (a, addAWeek a)
        (Just a, Just b) -> (max a (removeAYear b), b)

  dbResult <- try $ liftIO $ Db.validateUserAndProject p token (Project project)
  (StoredUser username) <- either Err.logError pure dbResult

  res <- try $ liftIO $ Db.getProjectStats p username project (t0, t1) (fromMaybe defaultLimit timeLimit)

  rows <- either Err.logError pure res

  return $ toStatsPayload t0 t1 rows

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
      if utctDay t == utctDay (prDay $ Unsafe.head r)
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
aggregateBy f rows = Just (prDay $ Unsafe.head rows, go rows Map.empty)
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

removeAWeek, removeAMonth, removeAYear, addAWeek, addAMonth :: UTCTime -> UTCTime
removeAWeek t = UTCTime {utctDay = addDays (-7) (utctDay t), utctDayTime = 0}
removeAMonth t = UTCTime {utctDay = addDays (-30) (utctDay t), utctDayTime = 0}
removeAYear t = UTCTime {utctDay = addDays (-365) (utctDay t), utctDayTime = 0}
addAWeek t = UTCTime {utctDay = addDays 7 (utctDay t), utctDayTime = 0}
addAMonth t = UTCTime {utctDay = addDays 30 (utctDay t), utctDayTime = 0}
