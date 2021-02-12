{-# LANGUAGE TemplateHaskell #-}

module Haka.Errors
  ( missingAuthError,
    missingRefreshTokenCookie,
    HeartbeatApiResponse (..),
    logError,
    toJSONError,
    DatabaseException (..),
    invalidTokenError,
    disabledRegistration,
    genericError,
    usernameExists,
    registerError,
    mkApiError,
    expiredRefreshToken,
    invalidCredentials,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import Data.Aeson (FromJSON (..), ToJSON (..), encode, genericParseJSON, genericToJSON)
import qualified Data.ByteString.Char8 as C
import Data.CaseInsensitive (CI, mk)
import Haka.AesonHelpers (noPrefixOptions, untagged)
import Haka.Types (BulkHeartbeatData, HearbeatData)
import qualified Hasql.Pool as HqPool
import Katip
import Servant

data HeartbeatApiResponse
  = SingleHeartbeatApiResponse HearbeatData
  | BulkHeartbeatApiResponse BulkHeartbeatData
  | ApiError ApiErrorData
  deriving (Show, Generic)

instance ToJSON HeartbeatApiResponse where
  toJSON = genericToJSON untagged

instance FromJSON HeartbeatApiResponse where
  parseJSON = genericParseJSON untagged

newtype ApiErrorData = ApiErrorData {apiError :: Text}
  deriving (Show, Generic)

instance ToJSON ApiErrorData where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON ApiErrorData where
  parseJSON = genericParseJSON noPrefixOptions

mkApiError :: Text -> HeartbeatApiResponse
mkApiError msg = ApiError $ ApiErrorData {apiError = msg}

contentTypeHeader :: [(CI ByteString, ByteString)]
contentTypeHeader = [(mk (C.pack "Content-Type"), C.pack "application/json;charset=utf-8")]

missingAuthError :: ServerError
missingAuthError =
  err400
    { errBody = encode $ mkApiError "Missing the 'Authorization' header field",
      errHeaders = contentTypeHeader
    }

missingRefreshTokenCookie :: ServerError
missingRefreshTokenCookie =
  err400
    { errBody = encode $ mkApiError "Missing the 'refresh_token' cookie",
      errHeaders = contentTypeHeader
    }

invalidTokenError :: ServerError
invalidTokenError =
  err403
    { errBody = encode $ mkApiError "The given api token doesn't belong to a user",
      errHeaders = contentTypeHeader
    }

expiredRefreshToken :: ServerError
expiredRefreshToken =
  err403
    { errBody = encode $ mkApiError "The given api token has expired",
      errHeaders = contentTypeHeader
    }

disabledRegistration :: ServerError
disabledRegistration =
  err403
    { errBody = encode $ mkApiError "Registration is disabled",
      errHeaders = contentTypeHeader
    }

usernameExists :: Text -> ServerError
usernameExists u =
  err409
    { errBody = encode $ mkApiError $ "The username " <> u <> " already exists",
      errHeaders = contentTypeHeader
    }

registerError :: ServerError
registerError =
  err409
    { errBody = encode $ mkApiError "The registration failed due to an internal error",
      errHeaders = contentTypeHeader
    }

invalidCredentials :: ServerError
invalidCredentials =
  err403
    { errBody = encode $ mkApiError "Invalid credentials",
      errHeaders = contentTypeHeader
    }

genericError :: Text -> ServerError
genericError _ =
  err500
    { errBody = encode $ mkApiError "An internal error occured",
      errHeaders = contentTypeHeader
    }

-- All database operations might throw the exception below.
data DatabaseException
  = SessionException HqPool.UsageError
  | UnknownApiToken
  | InvalidCredentials
  | MissingRefreshTokenCookie
  | ExpiredRefreshToken
  | UsernameExists Text
  | RegistrationFailed Text
  | OperationException Text
  deriving (Show)

instance Exception DatabaseException

-- | Convert a database exception to a serializable error message.
toJSONError :: DatabaseException -> ServerError
toJSONError UnknownApiToken = invalidTokenError
toJSONError ExpiredRefreshToken = expiredRefreshToken
toJSONError InvalidCredentials = invalidCredentials
toJSONError (SessionException e) = genericError (show e :: Text)
toJSONError (OperationException e) = genericError e
toJSONError (UsernameExists u) = usernameExists u
toJSONError (RegistrationFailed _) = registerError
toJSONError MissingRefreshTokenCookie = missingRefreshTokenCookie

logError :: (KatipContext m, MonadThrow m) => DatabaseException -> m b
logError e@MissingRefreshTokenCookie = do
  $(logTM) WarningS (logStr ("Missing refresh_token cookie from auth call" :: String))
  throw $ toJSONError e
logError e@(UsernameExists u) = do
  $(logTM) WarningS (logStr ("Registration attempt for existing username: " <> u))
  throw $ toJSONError e
logError e@UnknownApiToken = do
  $(logTM) WarningS (logStr ("Failed to identify a user with the given token" :: String))
  throw $ toJSONError e
logError e@ExpiredRefreshToken = do
  $(logTM) WarningS (logStr ("The given refresh token expired" :: String))
  throw $ toJSONError e
logError e = do
  $(logTM) ErrorS (logStr (show e :: String))
  throw $ toJSONError e
