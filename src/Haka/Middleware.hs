module Haka.Middleware (jsonResponse) where

import Data.Aeson
import Data.ByteString.Builder
import Data.Text (isInfixOf)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal

-- | Middleware to convert client errors in JSON
jsonResponse :: Application -> Application
jsonResponse = modifyResponse responseModifier

responseModifier :: Response -> Response
responseModifier r
  | responseStatus r == status400 && not (isCustomMessage r "Bad Request") =
    buildResponse status400 "BadRequest" (customErrorBody r "BadRequest")
  | responseStatus r == status405 =
    buildResponse status400 "MethodNotAllowed" "Ensure that the Content-Type header field is set correctly"
  | otherwise = r

customErrorBody :: Response -> Text -> Text
customErrorBody (ResponseBuilder _ _ b) _ = decodeUtf8 $ toLazyByteString b
customErrorBody (ResponseRaw _ res) e = customErrorBody res e
customErrorBody _ e = e

isCustomMessage :: Response -> Text -> Bool
isCustomMessage r m = "{\"error\":" `isInfixOf` customErrorBody r m

buildResponse :: Status -> Text -> Text -> Response
buildResponse st err msg =
  responseBuilder
    st
    [("Content-Type", "application/json")]
    ( lazyByteString $
        encode $
          object
            [ "error" .= err,
              "message" .= msg
            ]
    )
