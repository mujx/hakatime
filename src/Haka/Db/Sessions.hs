{-# LANGUAGE TupleSections #-}

module Haka.Db.Sessions
  ( getUser,
    getUserByRefreshToken,
    createBadgeLink,
    getTotalActivityTime,
    deleteFailedJobs,
    getJobStatus,
    updateTokenUsage,
    deleteToken,
    createAPIToken,
    listApiTokens,
    saveHeartbeats,
    getTotalStats,
    getTimeline,
    getProjectStats,
    insertToken,
    insertUser,
    validateUser,
    deleteTokens,
    getBadgeLinkInfo,
    createAccessTokens,
    setTags,
    checkProjectOwner,
    getTags,
  )
where

import qualified Crypto.Error as CErr
import Data.Aeson as A
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Vector as V
import qualified Haka.Db.Statements as Statements
import Haka.Types
  ( ApiToken (..),
    BadgeRow (..),
    HeartbeatPayload (..),
    Project (..),
    ProjectStatRow (..),
    RegisteredUser (..),
    StatRow (..),
    StoredApiToken,
    StoredUser (..),
    TimelineRow (..),
    TokenData (..),
  )
import qualified Haka.Utils as Utils
import Hasql.Session (Session, statement)
import qualified Hasql.Transaction as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import PostgreSQL.Binary.Data (UUID)

updateTokenUsage :: Text -> Session ()
updateTokenUsage tkn = statement tkn Statements.updateTokenUsage

listApiTokens :: Text -> Session [StoredApiToken]
listApiTokens usr = statement usr Statements.listApiTokens

getUser :: ApiToken -> Session (Maybe Text)
getUser (ApiToken token) = do
  now <- liftIO getCurrentTime
  statement (token, now) Statements.getUserByToken

getUserByRefreshToken :: Text -> Session (Maybe Text)
getUserByRefreshToken token = do
  now <- liftIO getCurrentTime
  statement (token, now) Statements.getUserByRefreshToken

deleteToken :: ApiToken -> Session ()
deleteToken (ApiToken token) = do
  _ <- statement token Statements.deleteAuthToken
  pass

deleteTokens :: ApiToken -> Text -> Session Int64
deleteTokens (ApiToken token) refreshToken = do
  r1 <-
    transaction
      Serializable
      Write
      (Transaction.statement token Statements.deleteAuthToken)
  r2 <-
    transaction
      Serializable
      Write
      (Transaction.statement refreshToken Statements.deleteRefreshToken)
  pure (r1 + r2)

saveHeartbeats :: [HeartbeatPayload] -> Session [Int64]
saveHeartbeats [] = pure []
saveHeartbeats payloadData = do
  -- Create the projects first so they can be referenced from the heartbeats.
  mapM_ (`statement` Statements.insertProject) uniqueProjects
  -- Insert the heartbeats.
  mapM (`statement` Statements.insertHeartBeat) payloadData
  where
    uniqueProjects =
      ordNub $
        mapMaybe
          ( \x ->
              case (sender x, project x) of
                (Just user, Just proj) -> Just (user, proj)
                _ -> Nothing
          )
          payloadData

getTotalActivityTime :: Text -> Int64 -> Text -> Session (Maybe Int64)
getTotalActivityTime user days proj = statement (user, days, proj) Statements.getTotalActivityTime

createBadgeLink :: Text -> Text -> Session UUID
createBadgeLink user proj = statement (user, proj) Statements.createBadgeLink

getBadgeLinkInfo :: UUID -> Session BadgeRow
getBadgeLinkInfo badgeId = statement badgeId Statements.getBadgeLinkInfo

-- | TODO: Impose a max limit
-- | Retrieve computed statistics for a given range.
getTotalStats :: Text -> (UTCTime, UTCTime) -> Int64 -> Session [StatRow]
getTotalStats user (startDate, endDate) cutOffLimit =
  statement (user, startDate, endDate, cutOffLimit) Statements.getUserActivity

getTimeline :: Text -> (UTCTime, UTCTime) -> Int64 -> Session [TimelineRow]
getTimeline user (startDate, endDate) cutOffLimit =
  statement (user, startDate, endDate, cutOffLimit) Statements.getTimeline

getProjectStats :: Text -> Text -> (UTCTime, UTCTime) -> Int64 -> Session [ProjectStatRow]
getProjectStats user proj (startDate, endDate) cutOffLimit =
  statement (user, proj, startDate, endDate, cutOffLimit) Statements.getProjectStats

insertUser :: RegisteredUser -> Session Bool
insertUser aUser = do
  r <- statement (username aUser) Statements.isUserAvailable
  case r of
    Just _ -> pure False
    Nothing -> do
      statement aUser Statements.insertUser
      pure True

validateUser ::
  (RegisteredUser -> Text -> Text -> Either CErr.CryptoError Bool) ->
  Text ->
  Text ->
  Session Bool
validateUser validate name passwd = do
  res <- statement name Statements.getUserByName
  case res of
    Nothing -> pure False
    Just savedUser ->
      case validate savedUser name passwd of
        Left e -> do
          liftIO $
            putStrLn $
              "failed to validate user password (" <> show name <> "): " <> show e
          pure False
        Right v -> pure v

-- | Insert a newly generated token for the given user.
-- | The token must be hashed and the salt should be saved.
insertToken :: Text -> Text -> Session ()
insertToken token name = statement (token, name) Statements.insertToken

createAccessTokens :: Int64 -> TokenData -> Session ()
createAccessTokens refreshTokenExpiryHours tknData = do
  transaction
    Serializable
    Write
    (Transaction.statement tknData (Statements.createAccessTokens refreshTokenExpiryHours))
  transaction
    Serializable
    Write
    (Transaction.statement tknData Statements.deleteExpiredTokens)

createAPIToken :: Text -> Session Text
createAPIToken usr = do
  newToken <- liftIO Utils.randomToken
  statement (usr, Utils.toBase64 newToken) Statements.createAPIToken
  pure newToken

deleteFailedJobs :: A.Value -> Session Int64
deleteFailedJobs payload = statement payload Statements.deleteFailedJobs

getJobStatus :: A.Value -> Session (Maybe Text)
getJobStatus payload = statement payload Statements.getJobStatus

setTags :: StoredUser -> Project -> V.Vector Text -> Session Int64
setTags (StoredUser user) (Project projectName) tags = do
  tagIds <- statement tags Statements.insertTags
  statement (projectName, user) Statements.deleteExistingTags
  statement (V.map (projectName,user,) tagIds) Statements.addTagsToProject

getTags :: StoredUser -> Project -> Session (V.Vector Text)
getTags (StoredUser user) (Project projectName) = statement (projectName, user) Statements.getTags

checkProjectOwner :: StoredUser -> Project -> Session Bool
checkProjectOwner (StoredUser user) (Project projectName) = do
  res <- statement (projectName, user) Statements.checkProjectOwner

  case res of
    Just _ -> pure True
    Nothing -> pure False
