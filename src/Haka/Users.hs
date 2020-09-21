{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Haka.Users
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw)
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Haka.AesonHelpers (noPrefixOptions)
import qualified Haka.DatabaseOperations as DbOps
import qualified Haka.Errors as Err
import Haka.Types (AppM, pool)
import Haka.Utils (getRefreshToken)
import Katip
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import Servant

type API = CurrentUser

type CurrentUser =
  "auth"
    :> "users"
    :> "current"
    -- The user is logged in if the refresh_token is still active.
    :> Header "Cookie" Text
    :> Get '[JSON] UserStatusResponse

newtype UserStatusResponse = UserStatusResponse
  { rData :: UserStatus
  }
  deriving (Generic)

data UserStatus = UserStatus
  { rFull_name :: Text,
    rEmail :: Text,
    -- TODO: Use generate letter icon.
    rPhoto :: Text
  }
  deriving (Generic)

defaultUserStatus :: Text -> UserStatusResponse
defaultUserStatus u =
  UserStatusResponse
    { rData =
        UserStatus
          { rFull_name = u,
            rEmail = u <> "@hakatime.dev",
            rPhoto = ""
          }
    }

instance ToJSON UserStatusResponse where
  toJSON = genericToJSON noPrefixOptions

instance ToJSON UserStatus where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON UserStatus where
  parseJSON = genericParseJSON noPrefixOptions

instance FromJSON UserStatusResponse where
  parseJSON = genericParseJSON noPrefixOptions

server :: Maybe Text -> AppM UserStatusResponse
server Nothing = throw Err.missingRefreshTokenCookie
server (Just cookies) = do
  p <- asks pool

  let refreshTkn =
        getRefreshToken (encodeUtf8 cookies)

  when (isNothing refreshTkn) (throw Err.missingRefreshTokenCookie)

  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getUserByRefreshToken p (fromJust refreshTkn)

  case res of
    Left e -> do
      $(logTM) ErrorS (logStr $ show e)
      throw (DbOps.toJSONError e)
    Right userM -> do
      case userM of
        Nothing -> throw Err.expiredToken
        Just u -> return $ defaultUserStatus u
