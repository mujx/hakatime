module Haka.Db.Sessions
  ( getUser,
    getUserByRefreshToken,
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
    createAccessTokens,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Error as CErr
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Haka.Db.Statements as Statements
import Haka.Types
  ( ApiToken (..),
    HeartbeatPayload (..),
    ProjectStatRow (..),
    RegisteredUser (..),
    StatRow (..),
    StoredApiToken,
    TimelineRow (..),
    TokenData (..),
  )
import qualified Haka.Utils as Utils
import Hasql.Session (Session, statement)
import qualified Hasql.Transaction as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)

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
saveHeartbeats payloadData = do
  -- Create the projects first so they can be referenced from the heartbeats.
  mapM_ (`statement` Statements.insertProject) uniqueProjects
  -- Insert the heartbeats.
  mapM (`statement` Statements.insertHeartBeat) payloadData
  where
    uniqueProjects = nub $ mapMaybe project payloadData

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

insertUser :: RegisteredUser -> Session ()
insertUser aUser = statement aUser Statements.insertUser

validateUser ::
  (RegisteredUser -> Text -> Text -> Either CErr.CryptoError Bool) ->
  Text ->
  Text ->
  Session Bool
validateUser validate name pass = do
  res <- statement name Statements.getUserByName
  case res of
    Nothing -> pure False
    Just savedUser ->
      case validate savedUser name pass of
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

createAccessTokens :: TokenData -> Session ()
createAccessTokens tknData = do
  transaction
    Serializable
    Write
    (Transaction.statement tknData Statements.createAccessTokens)
  transaction
    Serializable
    Write
    (Transaction.statement tknData Statements.deleteExpiredTokens)

createAPIToken :: Text -> Session Text
createAPIToken usr = do
  newToken <- liftIO Utils.randomToken
  statement (usr, Utils.toBase64 newToken) Statements.createAPIToken
  pure newToken
