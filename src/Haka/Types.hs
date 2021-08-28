{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Haka.Types
  ( HearbeatData (..),
    StoredApiToken (..),
    RegisteredUser (..),
    StoredUser (..),
    Project (..),
    Tag (..),
    BadgeRow (..),
    BulkHeartbeatData (..),
    HeartbeatId (..),
    ReturnBulkStruct (..),
    ApiToken (..),
    HeartbeatPayload (..),
    EntityType (..),
    StatRow (..),
    TimelineRow (..),
    ProjectStatRow (..),
    TokenData (..),
    LeaderboardRow (..),
    TokenMetadata (..),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Aeson as A
import qualified Data.ByteString as Bs
import Data.Text (strip)
import Data.Time.Clock (UTCTime)
import Haka.AesonHelpers (convertReservedWords, noPrefixOptions, untagged)
import qualified Haka.Utils as Utils
import PostgreSQL.Binary.Data (Scientific)
import Servant

data StoredApiToken = StoredApiToken
  { -- Some characters to identify a token.
    tknId :: Text,
    -- When the token was used.
    lastUsage :: Maybe UTCTime,
    -- An optional name given to the token.
    tknName :: Maybe Text,
    -- An optiona description given to the token.
    tknDesc :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON StoredApiToken

instance ToJSON StoredApiToken

-- Data needed to generate a new auth token pair in the database.
data TokenData = TokenData
  { -- | The user to assign the token.
    tknOwner :: Text,
    -- | The actual token that will be used for authentication.
    tknToken :: Text,
    -- | The refresh token.
    tknRefreshToken :: Text
  }

newtype StoredUser = StoredUser Text
  deriving (Show)

newtype Project = Project Text
  deriving (Show)

newtype Tag = Tag Text
  deriving (Show)

data RegisteredUser = RegisteredUser
  { username :: Text,
    hashedPassword :: ByteString,
    saltUsed :: ByteString
  }
  deriving (Eq, Show)

-- | Aggregated data for the timeline chart.
data TimelineRow = TimelineRow
  { -- | The language that was used.
    tmLang :: Text,
    -- | The project that the activity uccurred.
    tmProject :: Text,
    -- | The start of the time block.
    tmRangeStart :: UTCTime,
    -- | The end of the block.
    tmRangeEnd :: UTCTime
  }
  deriving (Eq, Show)

-- | Aggregated data for coding activity.
data ProjectStatRow = ProjectStatRow
  { -- | Day that activity occurred.
    prDay :: UTCTime,
    -- | Weekday (0 - 6)
    prWeekday :: Text,
    -- | The hour of day (0 - 23)
    prHour :: Text,
    -- | Programming language used.
    prLanguage :: Text,
    -- | File entity.
    prEntity :: Text,
    -- | Total seconds occupied on that entity.
    prTotalSeconds :: Int64,
    -- | Percentage of total time (with the range in context) spent on the resource.
    prPct :: Scientific,
    -- | Percentage of daily time spent on the resource.
    prDailyPct :: Scientific
  }
  deriving (Eq, Show)

-- | Aggregated data for coding activity.
data StatRow = StatRow
  { -- | Day that activity occurred.
    rDay :: UTCTime,
    -- | Activity with that project.
    rProject :: Text,
    -- | Programming language used.
    rLanguage :: Text,
    -- | Programming editor used.
    rEditor :: Text,
    -- | CVS branch.
    rBranch :: Text,
    -- | Operating system.
    rPlatform :: Text,
    -- | Hostname.
    rMachine :: Text,
    -- | File edited.
    rEntity :: Text,
    -- | Total seconds occupied on that entity.
    rTotalSeconds :: Int64,
    -- | Percentage of total time (with the range in context) spent on the resource.
    rPct :: Scientific,
    -- | Percentage of daily time spent on the resource.
    rDailyPct :: Scientific
  }
  deriving (Eq, Show)

data BadgeRow = BadgeRow
  { badgeUsername :: Text,
    badgeProject :: Text
  }

newtype ApiToken = ApiToken Text
  deriving (Show, Generic)

instance ToHttpApiData ApiToken where
  toHeader (ApiToken t) = encodeUtf8 $ "Basic " <> Utils.toBase64 t
  toUrlPiece (ApiToken t) = Utils.toBase64 t

instance FromHttpApiData ApiToken where
  parseUrlPiece = error "ApiToken should only be used as Header param"

  parseHeader str
    | Just token <- Bs.stripPrefix "Basic" str =
      Right $
        ApiToken (strip $ decodeUtf8 token)
  parseHeader _ = Left "Authorization should follow the 'Basic <api-token>' format"

newtype HeartbeatId = HeartbeatId
  { heartbeatId :: Text
  }
  deriving (Show, Generic)

instance ToJSON HeartbeatId where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON HeartbeatId where
  parseJSON = genericParseJSON noPrefixOptions

newtype HearbeatData = HearbeatData
  { heartbeatData :: HeartbeatId
  }
  deriving (Show, Generic)

instance ToJSON HearbeatData where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON HearbeatData where
  parseJSON = genericParseJSON noPrefixOptions

data ReturnBulkStruct = ReturnData HearbeatData | ReturnCode Int
  deriving (Show, Generic)

instance ToJSON ReturnBulkStruct where
  toJSON = genericToJSON untagged

instance FromJSON ReturnBulkStruct where
  parseJSON = genericParseJSON untagged

newtype BulkHeartbeatData = BulkHeartbeatData
  { bResponses :: [[ReturnBulkStruct]]
  }
  deriving (Show, Generic)

instance ToJSON BulkHeartbeatData where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON BulkHeartbeatData where
  parseJSON = genericParseJSON noPrefixOptions

data HeartbeatPayload = HeartbeatPayload
  { -- | The code editor used.
    editor :: Maybe Text,
    -- | The wakatime plugin used by the code editor.
    plugin :: Maybe Text,
    -- | The operating system info.
    platform :: Maybe Text,
    -- | Usually the hostname of the machine that sent the heartbeat.
    -- Extracted from the X-Machine-Name header field.
    machine :: Maybe Text,
    -- | The user associated with this request. Referenced by the Api Token.
    sender :: Maybe Text,
    -- | Identifier of the client, code editor etc.
    user_agent :: Text,
    -- | The VCS branch of the project.
    branch :: Maybe Text,
    -- | The category of the project.
    category :: Maybe Text,
    cursorpos :: Maybe Text,
    -- | Software dependencies extracted from the source code.
    dependencies :: Maybe [Text],
    -- | The file, app etc.
    entity :: Text,
    -- | Whether or not the heartbeat was trigger by a file save.
    is_write :: Maybe Bool,
    -- | The language used by the entity.
    language :: Maybe Text,
    lineno :: Maybe Text,
    -- | Total number of lines for the entity.
    file_lines :: Maybe Int64,
    -- | Name of the project.
    project :: Maybe Text,
    -- | Type of the entity that triggered the heartbeat (file, app, domain)
    ty :: EntityType,
    -- | Unix timestamp for when the heartbeat was generated.
    time_sent :: Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON HeartbeatPayload where
  parseJSON = genericParseJSON convertReservedWords

instance ToJSON HeartbeatPayload where
  toJSON = genericToJSON convertReservedWords

data EntityType = FileType | AppType | DomainType
  deriving (Eq, Show, Generic)

instance FromJSON EntityType where
  parseJSON (A.String p) = case p of
    "file" -> return FileType
    "app" -> return AppType
    "domain" -> return DomainType
    _ -> fail "Value can only be one of ['file', 'app', 'domain']"
  parseJSON _ = fail "Expected a string value"

instance ToJSON EntityType where
  toJSON FileType = A.String "file"
  toJSON AppType = A.String "app"
  toJSON DomainType = A.String "domain"

data LeaderboardRow = LeaderboardRow
  { leadProject :: Text,
    leadLanguage :: Text,
    leadSender :: Text,
    leadTotalSeconds :: Int64
  }
  deriving (Eq, Show, Generic)

data TokenMetadata = TokenMetadata
  { tokenName :: Text,
    tokenId :: Text
  }
  deriving (Show, Generic)

instance ToJSON TokenMetadata

instance FromJSON TokenMetadata
