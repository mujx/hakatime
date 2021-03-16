module Haka.Handlers.Commits
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw, try)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime (..))
import qualified Data.Vector as V
import Haka.AesonHelpers (noPrefixOptions)
import Haka.App (AppCtx (..), AppM)
import qualified Haka.Database as Db
import qualified Haka.Errors as Err
import Haka.Types (ApiToken (..))
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as R
import qualified Relude.Unsafe as Unsafe
import Servant
import System.Environment.MrEnv (envAsString)

defaultNumOfCommits :: Int64
defaultNumOfCommits = 20

githubEnvVariable :: String
githubEnvVariable = "GITHUB_TOKEN_M"

emptyToken :: String
emptyToken = ""

newtype CommitReport = CommitReport
  { commits :: [CommitPayload]
  }
  deriving (Show, Generic)

instance ToJSON CommitReport

type API = GetCommitReport

server :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int64 -> Maybe ApiToken -> AppM CommitReport
server = commitReportHandler

type GetCommitReport =
  "api"
    :> "v1"
    :> "commits"
    :> Capture "project" Text
    :> "report"
    :> QueryParam "repo_name" Text
    :> QueryParam "repo_owner" Text
    :> QueryParam "user" Text
    :> QueryParam "limit" Int64
    :> Header "Authorization" ApiToken
    :> Get '[JSON] CommitReport

data AuthorData = AuthorData
  { authorName :: Text,
    authorEmail :: Text,
    authorDate :: UTCTime
  }
  deriving (Show, Generic)

data CommitterData = CommitterData
  { committerName :: Text,
    committerEmail :: Text,
    committerDate :: UTCTime
  }
  deriving (Show, Generic)

data CommitData = CommitData
  { dataUrl :: Text,
    dataAuthor :: AuthorData,
    dataCommitter :: CommitterData,
    dataMessage :: Text
  }
  deriving (Show, Generic)

newtype AuthorPayload = AuthorPayload
  { authorLogin :: Text
  }
  deriving (Show, Generic)

data CommitPayload = CommitPayload
  { pUrl :: Text,
    pSha :: Text,
    pHtml_url :: Text,
    pCommit :: CommitData,
    pAuthor :: AuthorPayload,
    pTotal_seconds :: Maybe Int64
  }
  deriving (Show, Generic)

instance FromJSON CommitPayload where
  parseJSON = genericParseJSON noPrefixOptions

instance FromJSON CommitData where
  parseJSON = genericParseJSON noPrefixOptions

instance FromJSON AuthorData where
  parseJSON = genericParseJSON noPrefixOptions

instance FromJSON CommitterData where
  parseJSON = genericParseJSON noPrefixOptions

instance FromJSON AuthorPayload where
  parseJSON = genericParseJSON noPrefixOptions

instance ToJSON CommitPayload where
  toJSON = genericToJSON noPrefixOptions

instance ToJSON CommitData where
  toJSON = genericToJSON noPrefixOptions

instance ToJSON AuthorData where
  toJSON = genericToJSON noPrefixOptions

instance ToJSON CommitterData where
  toJSON = genericToJSON noPrefixOptions

instance ToJSON AuthorPayload where
  toJSON = genericToJSON noPrefixOptions

githubApi :: Text
githubApi = "api.github.com"

getCommitsFor ::
  (R.MonadHttp m) =>
  String ->
  Text ->
  Text ->
  Int64 ->
  m (Either Text [CommitPayload])
getCommitsFor githubToken repoOwner repoName limit = do
  let header =
        R.header "Authorization" ("Basic " <> encodeUtf8 githubToken)
          <> R.header "User-Agent" "Hakatime Server"
      params = "per_page" =: limit

  res <-
    R.req
      R.GET
      (R.https githubApi /: "repos" /: repoOwner /: repoName /: "commits")
      R.NoReqBody
      R.jsonResponse
      (params <> header)

  pure $ Right (R.responseBody res :: [CommitPayload])

-- Replace the entries with the calculated time in the final commit list.
updateCommits :: Map.Map Text CommitPayload -> [CommitPayload] -> [CommitPayload]
updateCommits _ [] = []
updateCommits map' (x : xs) = case Map.lookup (pSha x) map' of
  Just value -> value : updateCommits map' xs
  Nothing -> x : updateCommits map' xs

commitReportHandler ::
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int64 ->
  Maybe ApiToken ->
  AppM CommitReport
commitReportHandler _ _ _ _ _ Nothing = throw Err.missingAuthError
commitReportHandler _ Nothing _ _ _ _ = throw $ Err.missingQueryParam "repo_name"
commitReportHandler _ _ Nothing _ _ _ = throw $ Err.missingQueryParam "repo_owner"
commitReportHandler _ _ _ Nothing _ _ = throw $ Err.missingQueryParam "user"
commitReportHandler proj (Just repoName) (Just repoOwner) (Just user) limit (Just token) = do
  let numCommits = fromMaybe defaultNumOfCommits limit

  githubToken <- liftIO $ envAsString githubEnvVariable emptyToken

  if githubToken == emptyToken
    then throw Err.missingGithubToken
    else do
      -- We append one extra commit because the time for the last commit cannot not be calculated.
      res <- getCommitsFor githubToken repoOwner repoName (numCommits + 1)
      repoCommits <- either Err.logStrErr pure res

      let usersCommits = filter (\x -> authorLogin (pAuthor x) == user) repoCommits

      _pool <- asks pool

      userRes <- try $ liftIO $ Db.getUserByToken _pool token
      username <- either Err.logError pure userRes

      let commitGaps = zip (Unsafe.tail usersCommits) (Unsafe.init usersCommits)
      let timePairs =
            V.fromList $
              map
                ( \(a, b) ->
                    ( username,
                      proj,
                      authorDate $ dataAuthor $ pCommit a,
                      authorDate $ dataAuthor $ pCommit b
                    )
                )
                commitGaps

      timeRes <- try $ liftIO $ Db.getTotalTimeBetween _pool timePairs
      timeSpent <- either Err.logError pure timeRes

      let commitsWithTime =
            Map.fromList $
              zipWith
                (curry (\((_, b), c) -> (pSha b, b {pTotal_seconds = Just c})))
                commitGaps
                timeSpent

      -- We remove the last commit which doesn't include the time spent.
      let result = take (fromIntegral numCommits) $ updateCommits commitsWithTime repoCommits

      return $ CommitReport {commits = result}
