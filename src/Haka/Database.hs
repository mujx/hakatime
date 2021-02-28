{-# LANGUAGE BlockArguments #-}

module Haka.Database
  ( processHeartbeatRequest,
    importHeartbeats,
    Db (..),
    getUserByToken,
    validateUserAndProject,
    mkBadgeLink,
    getApiTokens,
    deleteApiToken,
    genProjectStatistics,
    getTimeline,
    createNewApiToken,
    clearTokens,
    generateStatistics,
    DatabaseException (..),
    createAuthTokens,
    refreshAuthTokens,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import Data.Aeson as A
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import qualified Haka.Db.Sessions as Sessions
import Haka.Errors (DatabaseException (..))
import qualified Haka.PasswordUtils as PUtils
import Haka.Types
  ( ApiToken (..),
    BadgeRow (..),
    HeartbeatPayload (..),
    Project (..),
    ProjectStatRow (..),
    StatRow (..),
    StoredApiToken,
    StoredUser (..),
    TimelineRow (..),
    TokenData (..),
  )
import qualified Haka.Utils as Utils
import qualified Hasql.Pool as HqPool
import PostgreSQL.Binary.Data (UUID)

data OperationError = UsageError | Text
  deriving (Show)

class (Monad m, MonadThrow m) => Db m where
  -- | Given an Api token return the user that it belongs to.
  getUser :: HqPool.Pool -> ApiToken -> m (Maybe Text)

  -- | Given a refresh token return the user that it belongs to.
  getUserByRefreshToken :: HqPool.Pool -> Text -> m (Maybe Text)

  -- | Check if the credentials are valid.
  validateCredentials :: HqPool.Pool -> Text -> Text -> m (Maybe Text)

  -- | Store the given heartbeats in the Db and return their IDs.
  saveHeartbeats :: HqPool.Pool -> [HeartbeatPayload] -> m [Int64]

  -- | Retrieve a list of statistics within the given time range.
  getTotalStats :: HqPool.Pool -> Text -> (UTCTime, UTCTime) -> Maybe Text -> Int64 -> m [StatRow]

  -- | Retrieve the activity timeline for a period of time.
  getTimelineStats :: HqPool.Pool -> Text -> (UTCTime, UTCTime) -> Int64 -> m [TimelineRow]

  -- | Retrieve a list of statistics within the given time range.
  getProjectStats :: HqPool.Pool -> Text -> Text -> (UTCTime, UTCTime) -> Int64 -> m [ProjectStatRow]

  -- | Create a pair of an access token a refresh token for use on web interface.
  createWebToken :: HqPool.Pool -> Text -> Int64 -> m TokenData

  -- | Register a new user.
  registerUser :: HqPool.Pool -> Text -> Text -> Int64 -> m TokenData

  -- | Delete the given auth and refresh tokens from the Db.
  deleteTokens :: HqPool.Pool -> ApiToken -> Text -> m Int64

  -- | Create a new API token that can be used on the client (no expiry date).
  createAPIToken :: HqPool.Pool -> Text -> m Text

  -- | Return a list of active API tokens.
  listApiTokens :: HqPool.Pool -> Text -> m [StoredApiToken]

  -- | Delete an API token.
  deleteToken :: HqPool.Pool -> ApiToken -> m ()

  -- | Update the last used timestamp for the token.
  updateTokenUsage :: HqPool.Pool -> ApiToken -> m ()

  -- | Get the total number of seconds spent on a given user/project combination.
  getTotalActivityTime :: HqPool.Pool -> Text -> Int64 -> Text -> m (Maybe Int64)

  -- | Create a unique badge link for the user/project combination.
  createBadgeLink :: HqPool.Pool -> Text -> Text -> m UUID

  -- | Find the user/project combination from the badge id.
  getBadgeLinkInfo :: HqPool.Pool -> UUID -> m BadgeRow

  -- | Get the status of a queue item.
  getJobStatus :: HqPool.Pool -> A.Value -> m (Maybe Text)

  -- | Delete stale failed jobs.
  deleteFailedJobs :: HqPool.Pool -> A.Value -> m Int64

  -- | Attach the given tags to a project.
  setTags :: HqPool.Pool -> StoredUser -> Project -> V.Vector Text -> m Int64

  -- | Get the tags associated with a project.
  getTags :: HqPool.Pool -> StoredUser -> Project -> m (V.Vector Text)

  -- | Validate that a project has the given owner.
  checkProjectOwner :: HqPool.Pool -> StoredUser -> Project -> m Bool

instance Db IO where
  getUser pool token = do
    res <- HqPool.use pool (Sessions.getUser token)
    either (throw . SessionException) pure res
  getUserByRefreshToken pool token = do
    res <- HqPool.use pool (Sessions.getUserByRefreshToken token)
    either (throw . SessionException) pure res
  validateCredentials pool user passwd = do
    res <- HqPool.use pool (Sessions.validateUser PUtils.validatePassword user passwd)
    either
      (throw . SessionException)
      ( \isValid -> if isValid then pure $ Just user else pure Nothing
      )
      res
  saveHeartbeats pool heartbeats = do
    res <- HqPool.use pool (Sessions.saveHeartbeats heartbeats)
    either (throw . SessionException) pure res
  getTotalStats pool user trange tagName cutOffLimit = do
    res <- HqPool.use pool (Sessions.getTotalStats user trange tagName cutOffLimit)
    either (throw . SessionException) pure res
  getTimelineStats pool user trange cutOffLimit = do
    res <- HqPool.use pool (Sessions.getTimeline user trange cutOffLimit)
    either (throw . SessionException) pure res
  getProjectStats pool user proj trange cutOffLimit = do
    res <- HqPool.use pool (Sessions.getProjectStats user proj trange cutOffLimit)
    either (throw . SessionException) pure res
  deleteTokens pool token refreshToken = do
    res <- HqPool.use pool (Sessions.deleteTokens token refreshToken)
    either (throw . SessionException) pure res
  createAPIToken pool user = do
    res <- HqPool.use pool (Sessions.createAPIToken user)
    either (throw . SessionException) pure res
  createWebToken pool user expiry = do
    tknData <- mkTokenData user
    res <- HqPool.use pool (Sessions.createAccessTokens expiry tknData)
    whenLeft tknData res (throw . SessionException)
  registerUser pool user passwd expiry = do
    tknData <- mkTokenData user
    hashUser <- PUtils.mkUser user passwd
    case hashUser of
      Left err -> throw $ RegistrationFailed (show err)
      Right hashUser' -> do
        u <- PUtils.createUser pool hashUser'
        case u of
          Left e -> throw $ OperationException (Utils.toStrError e)
          Right userCreated ->
            if userCreated
              then do
                res <- HqPool.use pool (Sessions.createAccessTokens expiry tknData)
                whenLeft tknData res (throw . SessionException)
              else throw $ UsernameExists user
  listApiTokens pool user = do
    res <- HqPool.use pool (Sessions.listApiTokens user)
    either (throw . SessionException) pure res
  deleteToken pool tkn = do
    res <- HqPool.use pool (Sessions.deleteToken tkn)
    either (throw . SessionException) pure res
  updateTokenUsage pool (ApiToken tkn) = do
    res <- HqPool.use pool (Sessions.updateTokenUsage tkn)
    either (throw . SessionException) pure res
  getTotalActivityTime pool user days proj = do
    res <- HqPool.use pool (Sessions.getTotalActivityTime user days proj)
    either (throw . SessionException) pure res
  createBadgeLink pool user proj = do
    res <- HqPool.use pool (Sessions.createBadgeLink user proj)
    either (throw . SessionException) pure res
  getBadgeLinkInfo pool badgeId = do
    res <- HqPool.use pool (Sessions.getBadgeLinkInfo badgeId)
    either (throw . SessionException) pure res
  getJobStatus pool payload = do
    res <- HqPool.use pool (Sessions.getJobStatus payload)
    either (throw . SessionException) pure res
  deleteFailedJobs pool payload = do
    res <- HqPool.use pool (Sessions.deleteFailedJobs payload)
    either (throw . SessionException) pure res
  setTags pool user projectName tags = do
    res <- HqPool.use pool (Sessions.setTags user projectName tags)
    either (throw . SessionException) pure res
  getTags pool user projectName = do
    res <- HqPool.use pool (Sessions.getTags user projectName)
    either (throw . SessionException) pure res
  checkProjectOwner pool user projectName = do
    res <- HqPool.use pool (Sessions.checkProjectOwner user projectName)
    either (throw . SessionException) pure res

mkTokenData :: Text -> IO TokenData
mkTokenData user = do
  refreshToken <- Utils.toBase64 <$> Utils.randomToken
  token <- Utils.toBase64 <$> Utils.randomToken
  pure $
    TokenData
      { tknOwner = user,
        tknToken = token,
        tknRefreshToken = refreshToken
      }

processHeartbeatRequest :: Db m => HqPool.Pool -> ApiToken -> [HeartbeatPayload] -> m [Int64]
processHeartbeatRequest pool token heartbeats = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just userName -> do
      updateTokenUsage pool token
      saveHeartbeats pool (updateHeartbeats heartbeats userName)

editorInfo :: [HeartbeatPayload] -> [Utils.EditorInfo]
editorInfo = map (Utils.userAgentInfo . user_agent)

-- Update the missing fields with info gatherred from the user-agent.
updateHeartbeats :: [HeartbeatPayload] -> Text -> [HeartbeatPayload]
updateHeartbeats heartbeats name =
  zipWith
    ( \info beat ->
        beat
          { sender = Just name,
            editor = Utils.editor info,
            plugin = Utils.plugin info,
            platform = Utils.platform info
          }
    )
    (editorInfo heartbeats)
    heartbeats

importHeartbeats :: Db m => HqPool.Pool -> Text -> [HeartbeatPayload] -> m [Int64]
importHeartbeats pool username heartbeats = do
  saveHeartbeats pool (updateHeartbeats heartbeats username)

generateStatistics ::
  Db m =>
  HqPool.Pool ->
  ApiToken ->
  Int64 ->
  Maybe Text ->
  (UTCTime, UTCTime) ->
  m [StatRow]
generateStatistics pool token timeLimit tagName tmRange = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just username -> getTotalStats pool username tmRange tagName timeLimit

getTimeline ::
  Db m =>
  HqPool.Pool ->
  ApiToken ->
  Int64 ->
  (UTCTime, UTCTime) ->
  m [TimelineRow]
getTimeline pool token timeLimit tmRange = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just username -> getTimelineStats pool username tmRange timeLimit

genProjectStatistics ::
  Db m =>
  HqPool.Pool ->
  ApiToken ->
  Text ->
  Int64 ->
  (UTCTime, UTCTime) ->
  m [ProjectStatRow]
genProjectStatistics pool token proj timeLimit tmRange = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just username -> getProjectStats pool username proj tmRange timeLimit

createNewApiToken :: Db m => HqPool.Pool -> ApiToken -> m Text
createNewApiToken pool token = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just username -> createAPIToken pool username

createAuthTokens :: Db m => Text -> Text -> HqPool.Pool -> Int64 -> m TokenData
createAuthTokens user passwd pool expiry = do
  res <- validateCredentials pool user passwd
  case res of
    Nothing -> throw InvalidCredentials
    Just u -> createWebToken pool u expiry

refreshAuthTokens :: Db m => Maybe Text -> HqPool.Pool -> Int64 -> m TokenData
refreshAuthTokens Nothing _ _ = throw MissingRefreshTokenCookie
refreshAuthTokens (Just refreshToken) pool expiry = do
  res <- getUserByRefreshToken pool refreshToken
  case res of
    Nothing -> throw ExpiredRefreshToken
    Just u -> createWebToken pool u expiry

clearTokens :: Db m => ApiToken -> Maybe Text -> HqPool.Pool -> m ()
clearTokens _ Nothing _ = throw MissingRefreshTokenCookie
clearTokens token (Just refreshToken) pool = do
  res <- deleteTokens pool token refreshToken
  -- TODO: Improve this.
  case res of
    0 -> throw InvalidCredentials
    1 -> throw InvalidCredentials
    2 -> pass
    _ -> throw (OperationException "failed to delete all the tokens while logout")

getApiTokens :: Db m => HqPool.Pool -> ApiToken -> m [StoredApiToken]
getApiTokens pool token = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just username -> listApiTokens pool username

deleteApiToken :: Db m => HqPool.Pool -> ApiToken -> Text -> m ()
deleteApiToken pool token tokenId = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just _ -> deleteToken pool (ApiToken tokenId)

mkBadgeLink :: Db m => HqPool.Pool -> Text -> ApiToken -> m UUID
mkBadgeLink pool proj token = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just user -> createBadgeLink pool user proj

getUserByToken :: Db m => HqPool.Pool -> ApiToken -> m Text
getUserByToken pool token = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just user -> pure user

validateUserAndProject :: Db m => HqPool.Pool -> ApiToken -> Project -> m StoredUser
validateUserAndProject pool token projectName = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UnknownApiToken
    Just user -> do
      isOk <- checkProjectOwner pool (StoredUser user) projectName

      if isOk
        then pure $ StoredUser user
        else throw (InvalidRelation (StoredUser user) projectName)
