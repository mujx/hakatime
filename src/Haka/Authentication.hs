{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Haka.Authentication
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as Bs
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import GHC.Generics
import qualified Haka.DatabaseOperations as DbOps
import qualified Haka.Errors as Err
import Haka.Types (ApiToken, AppCtx (..), AppM, RegistrationStatus (..), StoredApiToken, TokenData (..))
import Katip
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import Safe (headMay)
import Servant
import Web.Cookie

data AuthRequest = AuthRequest
  { username :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance FromJSON AuthRequest

instance ToJSON AuthRequest

data LoginResponse = LoginResponse
  { token :: Text,
    tokenExpiry :: UTCTime,
    tokenUsername :: Text
  }
  deriving (Show, Generic)

instance ToJSON LoginResponse

instance FromJSON LoginResponse

newtype TokenResponse = TokenResponse
  { apiToken :: Text
  }
  deriving (Show, Generic)

instance ToJSON TokenResponse

instance FromJSON TokenResponse

type LoginResponse' = Headers '[Header "Set-Cookie" SetCookie] LoginResponse

type Login =
  "auth"
    :> "login"
    :> ReqBody '[JSON] AuthRequest
    :> Post '[JSON] LoginResponse'

type Register =
  "auth"
    :> "register"
    :> ReqBody '[JSON] AuthRequest
    :> Post '[JSON] LoginResponse'

type RefreshToken =
  "auth"
    :> "refresh_token"
    :> Header "Cookie" Text
    :> Post '[JSON] LoginResponse'

type Logout =
  "auth"
    :> "logout"
    :> Header "Authorization" ApiToken
    :> Header "Cookie" Text
    :> PostNoContent

type CreateAPIToken =
  "auth"
    :> "create_api_token"
    :> Header "Authorization" ApiToken
    :> Post '[JSON] TokenResponse

type ListAPITokens =
  "auth"
    :> "tokens"
    :> Header "Authorization" ApiToken
    :> Get '[JSON] [StoredApiToken]

type DeleteToken =
  "auth"
    :> "token"
    :> Capture "id" Text
    :> Header "Authorization" ApiToken
    :> DeleteNoContent

type API =
  Login
    :<|> RefreshToken
    :<|> CreateAPIToken
    :<|> ListAPITokens
    :<|> DeleteToken
    :<|> Logout
    :<|> Register

getStoredApiTokensHandler :: Maybe ApiToken -> AppM [StoredApiToken]
getStoredApiTokensHandler Nothing = throw Err.missingAuthError
getStoredApiTokensHandler (Just tkn) = do
  dbPool <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $ DbOps.getApiTokens dbPool tkn
  case res of
    Left e -> do
      $(logTM) ErrorS (logStr $ show e)
      throw (DbOps.toJSONError e)
    Right v -> pure v

mkRefreshTokenCookie :: TokenData -> SetCookie
mkRefreshTokenCookie tknData =
  defaultSetCookie
    { setCookieName = "refresh_token",
      setCookieValue = encodeUtf8 $ tknRefreshToken tknData,
      setCookieSameSite = Just sameSiteStrict,
      setCookiePath = Just "/auth",
      setCookieHttpOnly = True
    }

mkLoginResponse :: TokenData -> UTCTime -> LoginResponse
mkLoginResponse tknData now =
  LoginResponse
    { token = tknToken tknData,
      tokenExpiry = addUTCTime (30 * 60) now,
      tokenUsername = tknOwner tknData
    }

getRefreshToken :: Bs.ByteString -> Maybe Text
getRefreshToken cookies =
  let value = headMay $ map snd $ filter (\(k, _) -> k == "refresh_token") (parseCookies cookies)
   in case value of
        Just v -> Just $ decodeUtf8 v
        Nothing -> Nothing

loginHandler :: AuthRequest -> AppM LoginResponse'
loginHandler creds = do
  now <- liftIO getCurrentTime
  dbPool <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.createAuthTokens (username creds) (password creds) dbPool
  case res of
    Left e -> do
      $(logTM) ErrorS (logStr $ show e)
      throw (DbOps.toJSONError e)
    Right tknData -> do
      let cookie = mkRefreshTokenCookie tknData
      return $
        addHeader cookie $
          mkLoginResponse tknData now

registerHandler :: RegistrationStatus -> AuthRequest -> AppM LoginResponse'
registerHandler DisabledRegistration _ = throw Err.disabledRegistration
registerHandler EnabledRegistration creds = do
  now <- liftIO getCurrentTime
  dbPool <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.registerUser dbPool (username creds) (password creds)
  case res of
    Left e -> do
      $(logTM) ErrorS (logStr $ show e)
      throw (DbOps.toJSONError e)
    Right tknData -> do
      let cookie = mkRefreshTokenCookie tknData
      return $
        addHeader cookie $
          mkLoginResponse tknData now

refreshTokenHandler :: Maybe Text -> AppM LoginResponse'
refreshTokenHandler Nothing = throw Err.missingRefreshTokenCookie
refreshTokenHandler (Just cookies) = do
  now <- liftIO getCurrentTime
  dbPool <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.refreshAuthTokens (getRefreshToken (encodeUtf8 cookies)) dbPool
  case res of
    Left e -> do
      $(logTM) ErrorS (logStr $ show e)
      throw (DbOps.toJSONError e)
    Right tknData -> do
      let cookie = mkRefreshTokenCookie tknData
      return $
        addHeader cookie $
          mkLoginResponse tknData now

logoutHandler :: Maybe ApiToken -> Maybe Text -> AppM NoContent
logoutHandler Nothing _ = throw Err.missingAuthError
logoutHandler _ Nothing = throw Err.missingRefreshTokenCookie
logoutHandler (Just tkn) (Just cookies) =
  do
    dbPool <- asks pool
    res <-
      runM
        . embedToMonadIO
        . runError
        $ DbOps.interpretDatabaseIO $
          DbOps.clearTokens tkn (getRefreshToken (encodeUtf8 cookies)) dbPool
    case res of
      Left e -> do
        $(logTM) ErrorS (logStr $ show e)
        throw (DbOps.toJSONError e)
      Right _ -> return NoContent

createAPITokenHandler :: Maybe ApiToken -> AppM TokenResponse
createAPITokenHandler Nothing = throw Err.missingAuthError
createAPITokenHandler (Just tkn) =
  do
    dbPool <- asks pool
    res <-
      runM
        . embedToMonadIO
        . runError
        $ DbOps.interpretDatabaseIO $
          DbOps.createNewApiToken dbPool tkn
    case res of
      Left e -> do
        $(logTM) ErrorS (logStr $ show e)
        throw (DbOps.toJSONError e)
      Right t -> return $ TokenResponse {apiToken = t}

deleteTokenHandler :: Text -> Maybe ApiToken -> AppM NoContent
deleteTokenHandler _ Nothing = throw Err.missingAuthError
deleteTokenHandler tokenId (Just tkn) = do
  dbPool <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $ DbOps.deleteApiToken dbPool tkn tokenId
  case res of
    Left e -> do
      $(logTM) ErrorS (logStr $ show e)
      throw (DbOps.toJSONError e)
    Right _ -> return NoContent

server settings =
  loginHandler
    :<|> refreshTokenHandler
    :<|> createAPITokenHandler
    :<|> getStoredApiTokensHandler
    :<|> deleteTokenHandler
    :<|> logoutHandler
    :<|> registerHandler settings
