{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Haka.DatabaseOperations
  ( processHeartbeatRequest,
    interpretDatabaseIO,
    getBadgeLinkInfo,
    getTotalActivityTime,
    mkBadgeLink,
    getApiTokens,
    deleteApiToken,
    genProjectStatistics,
    getTimeline,
    registerUser,
    createNewApiToken,
    clearTokens,
    generateStatistics,
    DatabaseException (..),
    createAuthTokens,
    refreshAuthTokens,
    toJSONError,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import qualified Haka.Db.Sessions as Sessions
import qualified Haka.Errors as Err
import Haka.Types
  ( ApiToken (..),
    BadgeRow (..),
    HeartbeatPayload (..),
    ProjectStatRow (..),
    RequestConfig (..),
    StatRow (..),
    StoredApiToken,
    TimelineRow (..),
    TokenData (..),
  )
import Haka.Users (createUser, mkUser, validatePassword)
import qualified Haka.Utils as Utils
import qualified Hasql.Pool as HqPool
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import PostgreSQL.Binary.Data (UUID)
import Servant (ServerError (..))

data OperationError = UsageError | Text
  deriving (Show)

-- All database operations might throw the exception below.
data DatabaseException
  = SessionException HqPool.UsageError
  | UserNotFound
  | InvalidCredentials
  | MissingRefreshTokenCookie
  | ExpiredToken
  | UsernameExists Text
  | RegistrationFailed Text
  | OperationException Text
  deriving (Show)

-- | Convert a database exception to a serializable error message.
toJSONError :: DatabaseException -> ServerError
toJSONError UserNotFound = Err.invalidTokenError
toJSONError ExpiredToken = Err.expiredToken
toJSONError InvalidCredentials = Err.invalidCredentials
toJSONError (SessionException e) = Err.genericError (pack $ show e)
toJSONError (OperationException e) = Err.genericError e
toJSONError (UsernameExists _) = Err.usernameExists
toJSONError (RegistrationFailed _) = Err.registerError
toJSONError MissingRefreshTokenCookie = Err.missingRefreshTokenCookie

-- Effect model
data Database m a where
  -- | Given an Api token return the user that it belongs to.
  GetUser :: HqPool.Pool -> ApiToken -> Database m (Maybe Text)
  -- | Given a refresh token return the user that it belongs to.
  GetUserByRefreshToken :: HqPool.Pool -> Text -> Database m (Maybe Text)
  -- | Check if the credentials are valid.
  ValidateCredentials :: HqPool.Pool -> Text -> Text -> Database m (Maybe Text)
  -- | Store the given heartbeats in the database and return their IDs.
  SaveHeartbeats :: HqPool.Pool -> [HeartbeatPayload] -> Database m [Int64]
  -- | Retrieve a list of statistics within the given time range.
  GetTotalStats :: HqPool.Pool -> Text -> (UTCTime, UTCTime) -> Int64 -> Database m [StatRow]
  -- | Retrieve the activity timeline for a period of time.
  GetTimelineStats :: HqPool.Pool -> Text -> (UTCTime, UTCTime) -> Int64 -> Database m [TimelineRow]
  -- | Retrieve a list of statistics within the given time range.
  GetProjectStats :: HqPool.Pool -> Text -> Text -> (UTCTime, UTCTime) -> Int64 -> Database m [ProjectStatRow]
  -- | Create a pair of an access token a refresh token for use on web interface.
  CreateWebToken :: HqPool.Pool -> Text -> Int64 -> Database m TokenData
  -- | Register a new user.
  RegisterUser :: HqPool.Pool -> Text -> Text -> Int64 -> Database m TokenData
  -- | Delete the given auth and refresh tokens from the database.
  DeleteTokens :: HqPool.Pool -> ApiToken -> Text -> Database m Int64
  -- | Create a new API token that can be used on the client (no expiry date).
  CreateAPIToken :: HqPool.Pool -> Text -> Database m Text
  -- | Return a list of active API tokens.
  ListApiTokens :: HqPool.Pool -> Text -> Database m [StoredApiToken]
  -- | Delete an API token.
  DeleteToken :: HqPool.Pool -> ApiToken -> Database m ()
  -- | Update the last used timestamp for the token.
  UpdateTokenUsage :: HqPool.Pool -> ApiToken -> Database m ()
  -- | Get the total number of seconds spent on a given user/project combination.
  GetTotalActivityTime :: HqPool.Pool -> Text -> Int64 -> Text -> Database m (Maybe Int64)
  -- | Create a unique badge link for the user/project combination.
  CreateBadgeLink :: HqPool.Pool -> Text -> Text -> Database m UUID
  -- | Find the user/project combination from the badge id.
  GetBadgeLinkInfo :: HqPool.Pool -> UUID -> Database m BadgeRow

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

interpretDatabaseIO ::
  ( Member (Embed IO) r,
    Member (Error DatabaseException) r
  ) =>
  Sem (Database : r) a ->
  Sem r a
interpretDatabaseIO =
  interpret $ \case
    GetUser pool token -> do
      res <- liftIO $ HqPool.use pool (Sessions.getUser token)
      either (throw . SessionException) pure res
    GetUserByRefreshToken pool token -> do
      res <- liftIO $ HqPool.use pool (Sessions.getUserByRefreshToken token)
      either (throw . SessionException) pure res
    ValidateCredentials pool user pass -> do
      res <- liftIO $ HqPool.use pool (Sessions.validateUser validatePassword user pass)
      either
        (throw . SessionException)
        ( \isValid -> if isValid then pure $ Just user else pure Nothing
        )
        res
    SaveHeartbeats pool heartbeats -> do
      res <- liftIO $ HqPool.use pool (Sessions.saveHeartbeats heartbeats)
      either (throw . SessionException) pure res
    GetTotalStats pool user trange cutOffLimit -> do
      res <- liftIO $ HqPool.use pool (Sessions.getTotalStats user trange cutOffLimit)
      either (throw . SessionException) pure res
    GetTimelineStats pool user trange cutOffLimit -> do
      res <- liftIO $ HqPool.use pool (Sessions.getTimeline user trange cutOffLimit)
      either (throw . SessionException) pure res
    GetProjectStats pool user proj trange cutOffLimit -> do
      res <- liftIO $ HqPool.use pool (Sessions.getProjectStats user proj trange cutOffLimit)
      either (throw . SessionException) pure res
    DeleteTokens pool token refreshToken -> do
      res <- liftIO $ HqPool.use pool (Sessions.deleteTokens token refreshToken)
      either (throw . SessionException) pure res
    CreateAPIToken pool user -> do
      res <- liftIO $ HqPool.use pool (Sessions.createAPIToken user)
      either (throw . SessionException) pure res
    CreateWebToken pool user expiry -> do
      tknData <- liftIO $ mkTokenData user
      res <- liftIO $ HqPool.use pool (Sessions.createAccessTokens expiry tknData)
      either (throw . SessionException) (\_ -> pure tknData) res
    RegisterUser pool user pass expiry -> do
      tknData <- liftIO $ mkTokenData user
      hashUser <- liftIO $ mkUser user pass
      case hashUser of
        Left err -> throw $ RegistrationFailed (pack $ show err)
        Right hashUser' -> do
          u <- liftIO $ createUser pool hashUser'
          case u of
            Left e -> throw $ OperationException (Utils.toStrError e)
            Right userCreated ->
              if userCreated
                then do
                  res <- liftIO $ HqPool.use pool (Sessions.createAccessTokens expiry tknData)
                  either (throw . SessionException) (\_ -> pure tknData) res
                else throw $ UsernameExists "Username already exists"
    ListApiTokens pool user -> do
      res <- liftIO $ HqPool.use pool (Sessions.listApiTokens user)
      either (throw . SessionException) pure res
    DeleteToken pool tkn -> do
      res <- liftIO $ HqPool.use pool (Sessions.deleteToken tkn)
      either (throw . SessionException) pure res
    UpdateTokenUsage pool (ApiToken tkn) -> do
      res <- liftIO $ HqPool.use pool (Sessions.updateTokenUsage tkn)
      either (throw . SessionException) pure res
    GetTotalActivityTime pool user days proj -> do
      res <- liftIO $ HqPool.use pool (Sessions.getTotalActivityTime user days proj)
      either (throw . SessionException) pure res
    CreateBadgeLink pool user proj -> do
      res <- liftIO $ HqPool.use pool (Sessions.createBadgeLink user proj)
      either (throw . SessionException) pure res
    GetBadgeLinkInfo pool badgeId -> do
      res <- liftIO $ HqPool.use pool (Sessions.getBadgeLinkInfo badgeId)
      either (throw . SessionException) pure res

makeSem ''Database

processHeartbeatRequest ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r,
    Member (Reader RequestConfig) r
  ) =>
  [HeartbeatPayload] ->
  Sem r [Int64]
processHeartbeatRequest heartbeats = do
  reqConfig <- ask
  let pool = dbPool reqConfig
      token = apiToken reqConfig
      machineId = machineName reqConfig
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just userName -> do
      updateTokenUsage pool token
      saveHeartbeats pool (updateHeartbeats userName machineId)
  where
    editorInfo :: [Utils.EditorInfo]
    editorInfo = map (Utils.userAgentInfo . user_agent) heartbeats
    -- Update the missing fields with info gatherred from the user-agent
    -- header field & the machine id.
    updateHeartbeats :: Text -> Maybe Text -> [HeartbeatPayload]
    updateHeartbeats name machineId =
      map
        ( \(info, beat) ->
            beat
              { sender = Just name,
                machine = machineId,
                editor = Utils.editor info,
                plugin = Utils.plugin info,
                platform = Utils.platform info
              }
        )
        (zip editorInfo heartbeats)

generateStatistics ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  HqPool.Pool ->
  ApiToken ->
  Int64 ->
  (UTCTime, UTCTime) ->
  Sem r [StatRow]
generateStatistics pool token timeLimit tmRange = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just username -> getTotalStats pool username tmRange timeLimit

getTimeline ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  HqPool.Pool ->
  ApiToken ->
  Int64 ->
  (UTCTime, UTCTime) ->
  Sem r [TimelineRow]
getTimeline pool token timeLimit tmRange = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just username -> getTimelineStats pool username tmRange timeLimit

genProjectStatistics ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  HqPool.Pool ->
  ApiToken ->
  Text ->
  Int64 ->
  (UTCTime, UTCTime) ->
  Sem r [ProjectStatRow]
genProjectStatistics pool token proj timeLimit tmRange = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just username -> getProjectStats pool username proj tmRange timeLimit

createNewApiToken ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  HqPool.Pool ->
  ApiToken ->
  Sem r Text
createNewApiToken pool token = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just username -> createAPIToken pool username

createAuthTokens ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  Text ->
  Text ->
  HqPool.Pool ->
  Int64 ->
  Sem r TokenData
createAuthTokens user pass pool expiry = do
  res <- validateCredentials pool user pass
  case res of
    Nothing -> throw InvalidCredentials
    Just u -> createWebToken pool u expiry

refreshAuthTokens ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  Maybe Text ->
  HqPool.Pool ->
  Int64 ->
  Sem r TokenData
refreshAuthTokens Nothing _ _ = throw MissingRefreshTokenCookie
refreshAuthTokens (Just refreshToken) pool expiry = do
  res <- getUserByRefreshToken pool refreshToken
  case res of
    Nothing -> throw ExpiredToken
    Just u -> createWebToken pool u expiry

clearTokens ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  ApiToken ->
  Maybe Text ->
  HqPool.Pool ->
  Sem r ()
clearTokens _ Nothing _ = throw MissingRefreshTokenCookie
clearTokens token (Just refreshToken) pool = do
  res <- deleteTokens pool token refreshToken
  -- TODO: Improve this.
  case res of
    0 -> throw InvalidCredentials
    1 -> throw InvalidCredentials
    2 -> pure ()
    _ -> throw (OperationException "failed to delete all the tokens while logout")

getApiTokens ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  HqPool.Pool ->
  ApiToken ->
  Sem r [StoredApiToken]
getApiTokens pool token = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just username -> listApiTokens pool username

deleteApiToken ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  HqPool.Pool ->
  ApiToken ->
  Text ->
  Sem r ()
deleteApiToken pool token tokenId = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just _ -> deleteToken pool (ApiToken tokenId)

mkBadgeLink ::
  forall r.
  ( Member Database r,
    Member (Error DatabaseException) r
  ) =>
  HqPool.Pool ->
  Text ->
  ApiToken ->
  Sem r UUID
mkBadgeLink pool proj token = do
  retrievedUser <- getUser pool token
  case retrievedUser of
    Nothing -> throw UserNotFound
    Just user -> createBadgeLink pool user proj
