{-# LANGUAGE TemplateHaskell #-}

module Haka.Import (API, server) where

import Control.Exception.Safe (throw)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as LBs
import Data.Text
import Data.Time.Clock (UTCTime (..))
import GHC.Generics
import qualified Haka.Errors as Err
import Haka.Types
import Katip
import Servant

newtype ImportRequestResponse = ImportRequestResponse
  { jobStatus :: Text
  }
  deriving (Generic, Show)

instance FromJSON ImportRequestResponse

instance ToJSON ImportRequestResponse

data ImportRequestPayload = ImportRequestPayload
  { apiToken :: Text,
    startDate :: UTCTime,
    endDate :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON ImportRequestPayload

instance ToJSON ImportRequestPayload

type API = ImportRequest

type ImportRequest =
  "import"
    :> Header "Authorization" ApiToken
    :> ReqBody '[JSON] ImportRequestPayload
    :> Post '[JSON] ImportRequestResponse

server :: Maybe ApiToken -> ImportRequestPayload -> AppM ImportRequestResponse
server Nothing _ = throw Err.missingAuthError
server (Just token) payload = do
  $(logTM) InfoS "received an import request"

  return $ ImportRequestResponse {jobStatus = "ok"}
