module Haka.Errors
  ( missingAuthError,
    missingGithubToken,
    missingQueryParam,
    missingRefreshTokenCookie,
    HeartbeatApiResponse (..),
    logError,
    logStrErr,
    logHttpError,
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
import Data.Aeson (FromJSON (..), ToJSON (..), encode, genericParseJSON, genericToJSON, omitNothingFields)
import qualified Data.ByteString.Char8 as C
import Data.CaseInsensitive (CI, mk)
import Haka.AesonHelpers (noPrefixOptions, untagged)
import Haka.Types (BulkHeartbeatData, HearbeatData, Project (..), StoredUser (..), Tag (..))
import qualified Hasql.Pool as HqPool
import Katip
import Network.HTTP.Client (HttpException (..), host)
import qualified Network.HTTP.Req as R
import Servant
import Text.Printf (printf)

data HeartbeatApiResponse
  = SingleHeartbeatApiResponse HearbeatData
  | BulkHeartbeatApiResponse BulkHeartbeatData
  | ApiError ApiErrorData
  deriving (Show, Generic)

instance ToJSON HeartbeatApiResponse where
  toJSON = genericToJSON untagged

instance FromJSON HeartbeatApiResponse where
  parseJSON = genericParseJSON untagged

data ApiErrorData = ApiErrorData {apiError :: Text, apiMessage :: Maybe Text}
  deriving (Show, Generic)

instance ToJSON ApiErrorData where
  toJSON = genericToJSON noPrefixOptions {omitNothingFields = True}

instance FromJSON ApiErrorData where
  parseJSON = genericParseJSON noPrefixOptions

mkApiError :: Text -> Maybe Text -> HeartbeatApiResponse
mkApiError msg extraMsg = ApiError $ ApiErrorData {apiError = msg, apiMessage = extraMsg}

contentTypeHeader :: [(CI ByteString, ByteString)]
contentTypeHeader = [(mk (C.pack "Content-Type"), C.pack "application/json;charset=utf-8")]

missingAuthError :: ServerError
missingAuthError =
  err400
    { errBody = encode $ mkApiError "Missing the 'Authorization' header field" Nothing,
      errHeaders = contentTypeHeader
    }

missingQueryParam :: Text -> ServerError
missingQueryParam param =
  err400
    { errBody = encode $ mkApiError ("Missing query parameter " <> param) Nothing,
      errHeaders = contentTypeHeader
    }

missingRefreshTokenCookie :: ServerError
missingRefreshTokenCookie =
  err400
    { errBody = encode $ mkApiError "Missing the 'refresh_token' cookie" Nothing,
      errHeaders = contentTypeHeader
    }

invalidTokenError :: ServerError
invalidTokenError =
  err403
    { errBody = encode $ mkApiError "The given api token doesn't belong to a user" Nothing,
      errHeaders = contentTypeHeader
    }

invalidRelation :: StoredUser -> Project -> ServerError
invalidRelation (StoredUser user) (Project project) =
  err404
    { errBody =
        encode $
          mkApiError
            (toText (printf "The user %s doesn't have access to project %s" user project :: String))
            Nothing,
      errHeaders = contentTypeHeader
    }

invalidTagRelation :: StoredUser -> Tag -> ServerError
invalidTagRelation (StoredUser user) (Tag tag) =
  err404
    { errBody =
        encode $
          mkApiError
            (toText (printf "The user %s doesn't have access to the tag named '%s'" user tag :: String))
            Nothing,
      errHeaders = contentTypeHeader
    }

expiredRefreshToken :: ServerError
expiredRefreshToken =
  err403
    { errBody = encode $ mkApiError "The given api token has expired" Nothing,
      errHeaders = contentTypeHeader
    }

disabledRegistration :: ServerError
disabledRegistration =
  err403
    { errBody = encode $ mkApiError "Registration is disabled" Nothing,
      errHeaders = contentTypeHeader
    }

usernameExists :: Text -> ServerError
usernameExists u =
  err409
    { errBody = encode $ mkApiError ("The username " <> u <> " already exists") Nothing,
      errHeaders = contentTypeHeader
    }

registerError :: ServerError
registerError =
  err409
    { errBody = encode $ mkApiError "The registration failed due to an internal error" Nothing,
      errHeaders = contentTypeHeader
    }

invalidCredentials :: ServerError
invalidCredentials =
  err403
    { errBody = encode $ mkApiError "Invalid credentials" Nothing,
      errHeaders = contentTypeHeader
    }

missingGithubToken :: ServerError
missingGithubToken =
  err500
    { errBody = encode $ mkApiError "The environment variable GITHUB_TOKEN is not set" Nothing,
      errHeaders = contentTypeHeader
    }

genericError :: Text -> ServerError
genericError _ =
  err500
    { errBody = encode $ mkApiError "An internal error occured" Nothing,
      errHeaders = contentTypeHeader
    }

genericHttpError :: Text -> Maybe Text -> ServerError
genericHttpError e msg =
  err500
    { errBody = encode $ mkApiError e msg,
      errHeaders = contentTypeHeader
    }

-- All database operations might throw the exception below.
data DatabaseException
  = SessionException HqPool.UsageError
  | UnknownApiToken
  | InvalidCredentials
  | InvalidRelation StoredUser Project
  | InvalidTagRelation StoredUser Tag
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
toJSONError (InvalidRelation user project) = invalidRelation user project
toJSONError (InvalidTagRelation user tag) = invalidTagRelation user tag
toJSONError ExpiredRefreshToken = expiredRefreshToken
toJSONError InvalidCredentials = invalidCredentials
toJSONError (SessionException e) = genericError (show e :: Text)
toJSONError (OperationException e) = genericError e
toJSONError (UsernameExists u) = usernameExists u
toJSONError (RegistrationFailed _) = registerError
toJSONError MissingRefreshTokenCookie = missingRefreshTokenCookie

logStrErr :: (KatipContext m, MonadThrow m) => Text -> m b
logStrErr e = do
  logFM WarningS (ls e)
  throw $ genericError e

logHttpError :: (KatipContext m, MonadThrow m) => R.HttpException -> m b
logHttpError (R.VanillaHttpException exc@(HttpExceptionRequest r _)) = do
  logFM WarningS (show exc)
  throw $ genericHttpError ("HTTP call to " <> decodeUtf8 (host r) <> " failed") Nothing
logHttpError (R.VanillaHttpException (InvalidUrlException s e)) = do
  logFM WarningS ("Invalid url " <> ls s <> " : " <> show e)
  throw $ genericHttpError "InvalidUrlException" (Just ("Invalid url " <> show s <> " : " <> show e))
logHttpError (R.JsonHttpException e) = do
  logFM WarningS (ls e)
  throw $ genericHttpError "JsonHttpException" (Just $ show e)

logError :: (KatipContext m, MonadThrow m) => DatabaseException -> m b
logError e@MissingRefreshTokenCookie = do
  logFM WarningS (logStr ("Missing refresh_token cookie from auth call" :: String))
  throw $ toJSONError e
logError e@(UsernameExists u) = do
  logFM WarningS (logStr ("Registration attempt for existing username: " <> u))
  throw $ toJSONError e
logError e@UnknownApiToken = do
  logFM WarningS (logStr ("Failed to identify a user with the given token" :: String))
  throw $ toJSONError e
logError e@ExpiredRefreshToken = do
  logFM WarningS (logStr ("The given refresh token expired" :: String))
  throw $ toJSONError e
logError e@(InvalidRelation (StoredUser u) (Project p)) = do
  logFM WarningS (logStr (printf "User %s doesn't have a project named %s" u p :: String))
  throw $ toJSONError e
logError e@(InvalidTagRelation (StoredUser u) (Tag t)) = do
  logFM WarningS (logStr (printf "User %s doesn't have a tag named '%s'" u t :: String))
  throw $ toJSONError e
logError e = do
  logFM ErrorS (logStr (show e :: String))
  throw $ toJSONError e
