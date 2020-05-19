{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Haka.Types
  ( HeartbeatApiResponse (..),
    HearbeatData (..),
    RegistrationStatus (..),
    ServerSettings (..),
    BulkHeartbeatData (..),
    HeartbeatId (..),
    ApiErrorData (..),
    ReturnBulkStruct (..),
    ApiToken (..),
    HeartbeatPayload (..),
    EntityType (..),
    RequestConfig (..),
    RegisteredUser (..),
    StatRow (..),
    TimelineRow (..),
    ProjectStatRow (..),
    AppCtx (..),
    LogState (..),
    AppM,
    runAppT,
    mkAppT,
    AppT (..),
    TokenData (..),
  )
where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import qualified Data.Aeson as A
import Data.Aeson
  ( FromJSON,
    ToJSON (..),
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.ByteString as Bs
import Data.Int (Int64)
import Data.Text (Text, strip)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Haka.AesonHelpers
  ( convertReservedWords,
    noPrefixOptions,
    untagged,
  )
import qualified Haka.Utils as Utils
import qualified Hasql.Pool as HqPool
import qualified Katip as K
import PostgreSQL.Binary.Data (Scientific)
import Servant

data RegistrationStatus
  = EnabledRegistration
  | DisabledRegistration

-- | Server configuration settings.
data ServerSettings = ServerSettings
  { -- | Where the service will listen to.
    hakaPort :: Int,
    -- | What domain to allow.
    hakaCorsUrl :: Bs.ByteString,
    -- | Where to look for dashboard's static files.
    hakaDashboardPath :: FilePath,
    -- | Whether the registration is enabled.
    hakaEnableRegistration :: RegistrationStatus
  }

-- Data needed to generate a new auth token pair in the database.
data TokenData = TokenData
  { -- | The user to assign the token.
    tknOwner :: Text,
    -- | The actual token that will be used for authentication.
    tknToken :: Text,
    -- | The refresh token.
    tknRefreshToken :: Text
  }

data LogState = LogState
  { lsContext :: !K.LogContexts,
    lsNamespace :: !K.Namespace,
    lsLogEnv :: !K.LogEnv
  }

data AppCtx = AppCtx
  { pool :: HqPool.Pool,
    logState :: LogState
  }

newtype AppT m a = AppT
  { unAppT :: ReaderT AppCtx m a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadThrow,
      MonadReader AppCtx
    )

type AppM = AppT IO

-- | Implement a @Katip@ instance for our @AppT@ monad.
instance MonadIO m => K.Katip (AppT m) where
  getLogEnv = do
    logState' <- asks logState
    pure $ lsLogEnv logState'
  localLogEnv f (AppT m) =
    AppT
      ( local
          ( \ctx ->
              ctx
                { logState =
                    LogState
                      { lsContext = (lsContext . logState) ctx,
                        lsNamespace = (lsNamespace . logState) ctx,
                        lsLogEnv = f $ (lsLogEnv . logState) ctx
                      }
                }
          )
          m
      )

-- | Implement a @KatipContext@ instance for our @App@ monad.
instance MonadIO m => K.KatipContext (AppT m) where
  getKatipContext = do
    logState' <- asks logState
    pure $ lsContext logState'
  getKatipNamespace = do
    logState' <- asks logState
    pure $ lsNamespace logState'
  localKatipNamespace f (AppT m) =
    AppT
      ( local
          ( \ctx ->
              ctx
                { logState =
                    LogState
                      { lsContext = (lsContext . logState) ctx,
                        lsNamespace = f $ (lsNamespace . logState) ctx,
                        lsLogEnv = (lsLogEnv . logState) ctx
                      }
                }
          )
          m
      )

  localKatipContext f (AppT m) =
    AppT
      ( local
          ( \ctx ->
              ctx
                { logState =
                    LogState
                      { lsContext = f ((lsContext . logState) ctx),
                        lsNamespace = (lsNamespace . logState) ctx,
                        lsLogEnv = (lsLogEnv . logState) ctx
                      }
                }
          )
          m
      )

-- | Embed a function from some @Ctx@ to an arbitrary monad in @AppT@.
mkAppT :: (AppCtx -> m a) -> AppT m a
mkAppT = AppT . ReaderT

-- | Run an 'AppT' using the given 'Ctx'.
runAppT :: AppCtx -> AppT m a -> m a
runAppT ctx app = runReaderT (unAppT app) ctx

data RegisteredUser = RegisteredUser
  { username :: Text,
    hashedPassword :: Bs.ByteString,
    saltUsed :: Bs.ByteString
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

data HeartbeatApiResponse
  = SingleHeartbeatApiResponse HearbeatData
  | BulkHeartbeatApiResponse BulkHeartbeatData
  | ApiError ApiErrorData
  deriving (Show, Generic)

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

newtype ApiErrorData = ApiErrorData {apiError :: Text}
  deriving (Show, Generic)

instance ToJSON ApiErrorData where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON ApiErrorData where
  parseJSON = genericParseJSON noPrefixOptions

instance ToJSON HeartbeatApiResponse where
  toJSON = genericToJSON untagged

instance FromJSON HeartbeatApiResponse where
  parseJSON = genericParseJSON untagged

data HeartbeatPayload = HeartbeatPayload
  { -- | The code editor used.
    editor :: Maybe Text,
    -- | The wakatime plugin used by the code editor.
    plugin :: Maybe Text,
    -- | The operating system info.
    platform :: Maybe Text,
    -- | Usually the hostname of the machine that sent the heartbeat.
    -- Extracted from the X-Machine-Name header fiel.d
    machine :: Maybe Text,
    -- | The user associated with this request. Referenced by the Api Token.
    sender :: Maybe Text,
    -- | Identifier of the client, code editor etc.
    user_agent :: Text,
    -- | The VCS branch of the project.
    branch :: Maybe Text,
    -- | The category of the project.
    category :: Maybe Text,
    cursorpos :: Maybe Int64,
    -- | Software dependencies extracted from the source code.
    dependencies :: Maybe [Text],
    -- | The file, app etc.
    entity :: Text,
    -- | Whether or not the heartbeat was trigger by a file save.
    is_write :: Maybe Bool,
    -- | The language used by the entity.
    language :: Maybe Text,
    lineno :: Maybe Int64,
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

-- Global read-only state required for all the db operations.
data RequestConfig = RequestConfig
  { -- | The connection pool assigned with each request.
    dbPool :: HqPool.Pool,
    -- | The API token retrieved from the HTTP request headers.
    apiToken :: ApiToken,
    -- | Hostname information about the sender host.
    machineName :: Maybe Text
  }
  deriving (Show)
