module Haka.Errors
  ( missingAuthError,
    missingRefreshTokenCookie,
    invalidTokenError,
    disabledRegistration,
    genericError,
    usernameExists,
    registerError,
    mkApiError,
    expiredToken,
    invalidCredentials,
  )
where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as C
import Data.CaseInsensitive (mk)
import Data.Text (Text)
import Haka.Types
  ( ApiErrorData (..),
    HeartbeatApiResponse (..),
  )
import Servant

mkApiError :: Text -> HeartbeatApiResponse
mkApiError msg = ApiError $ ApiErrorData {apiError = msg}

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

expiredToken :: ServerError
expiredToken =
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

usernameExists :: ServerError
usernameExists =
  err409
    { errBody = encode $ mkApiError "The username already exists",
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
