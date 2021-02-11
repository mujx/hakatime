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
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as Bs
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Haka.App (AppCtx (..), AppM, RegistrationStatus (..), ServerSettings (..))
import qualified Haka.DatabaseOperations as DbOps
import qualified Haka.Errors as Err
import Haka.Types
  ( ApiToken,
    StoredApiToken,
    TokenData (..),
  )
import Haka.Utils (getRefreshToken)
import Katip
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import qualified Relude.Unsafe as Unsafe
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

  either Err.logError pure res

mkRefreshTokenCookie :: TokenData -> String -> SetCookie
mkRefreshTokenCookie tknData apiPrefix =
  defaultSetCookie
    { setCookieName = "refresh_token",
      setCookieValue = encodeUtf8 $ tknRefreshToken tknData,
      setCookieSameSite = Just sameSiteStrict,
      setCookiePath = Just (removeSlash apiPrefix <> "/auth"),
      setCookieHttpOnly = True
    }
  where
    removeSlash [] = Bs.empty
    removeSlash p = Bs.pack $ if Unsafe.last p == '/' then Unsafe.init p else p

mkLoginResponse :: TokenData -> UTCTime -> LoginResponse
mkLoginResponse tknData now =
  LoginResponse
    { token = tknToken tknData,
      tokenExpiry = addUTCTime (30 * 60) now,
      tokenUsername = tknOwner tknData
    }

loginHandler :: AuthRequest -> AppM LoginResponse'
loginHandler creds = do
  now <- liftIO getCurrentTime
  ctx <- ask

  $(logTM) InfoS ("login for user " <> showLS (username creds))

  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.createAuthTokens
          (username creds)
          (password creds)
          (pool ctx)
          (hakaSessionExpiry $ srvSettings ctx)

  tknData <- either Err.logError pure res

  return $
    addHeader (mkRefreshTokenCookie tknData (hakaApiPrefix $ srvSettings ctx)) $
      mkLoginResponse tknData now

registerHandler :: RegistrationStatus -> AuthRequest -> AppM LoginResponse'
registerHandler DisabledRegistration _ = throw Err.disabledRegistration
registerHandler EnabledRegistration creds =
  do
    now <- liftIO getCurrentTime
    ctx <- ask

    $(logTM) InfoS ("registering user " <> showLS (username creds))

    res <-
      runM
        . embedToMonadIO
        . runError
        $ DbOps.interpretDatabaseIO $
          DbOps.registerUser
            (pool ctx)
            (username creds)
            (password creds)
            (hakaSessionExpiry $ srvSettings ctx)

    tknData <- either Err.logError pure res

    return $
      addHeader (mkRefreshTokenCookie tknData (hakaApiPrefix $ srvSettings ctx)) $
        mkLoginResponse tknData now

refreshTokenHandler :: Maybe Text -> AppM LoginResponse'
refreshTokenHandler Nothing = throw Err.missingRefreshTokenCookie
refreshTokenHandler (Just cookies) = do
  now <- liftIO getCurrentTime
  ctx <- ask

  $(logTM) DebugS "refresh token request"

  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.refreshAuthTokens
          (getRefreshToken (encodeUtf8 cookies))
          (pool ctx)
          (hakaSessionExpiry $ srvSettings ctx)

  tknData <- either Err.logError pure res

  return $
    addHeader (mkRefreshTokenCookie tknData (hakaApiPrefix $ srvSettings ctx)) $
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

    _ <- either Err.logError pure res

    return NoContent

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

    t <- either Err.logError pure res

    return $ TokenResponse {apiToken = t}

deleteTokenHandler :: Text -> Maybe ApiToken -> AppM NoContent
deleteTokenHandler _ Nothing = throw Err.missingAuthError
deleteTokenHandler tokenId (Just tkn) = do
  dbPool <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $ DbOps.deleteApiToken dbPool tkn tokenId

  _ <- either Err.logError pure res

  return NoContent

server settings =
  loginHandler
    :<|> refreshTokenHandler
    :<|> createAPITokenHandler
    :<|> getStoredApiTokensHandler
    :<|> deleteTokenHandler
    :<|> logoutHandler
    :<|> registerHandler settings
