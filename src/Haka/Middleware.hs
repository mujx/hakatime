module Haka.Middleware (jsonResponse) where

import Blaze.ByteString.Builder (toLazyByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Data.Aeson
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
    buildResponse status400 "Bad Request" (customErrorBody r "BadRequest")
  | responseStatus r == status405 =
    buildResponse status405 "Method Not Allowed" "Method Not Allowed"
  | otherwise = r

customErrorBody :: Response -> Text -> Text
customErrorBody (ResponseBuilder _ _ b) _ = toStrict $ decodeUtf8 $ toLazyByteString b
customErrorBody (ResponseRaw _ res) e = customErrorBody res e
customErrorBody _ e = e

isCustomMessage :: Response -> Text -> Bool
isCustomMessage r m = "{\"error\":" `isInfixOf` customErrorBody r m

buildResponse :: Status -> Text -> Text -> Response
buildResponse st err msg =
  responseBuilder
    st
    [("Content-Type", "application/json")]
    ( fromByteString . toStrict . encode $
        object
          [ "error" .= err,
            "message" .= msg
          ]
    )
