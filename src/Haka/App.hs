{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Haka.App
  ( AppCtx (..),
    LogState (..),
    RemoteWriteConfig (..),
    AppM,
    runAppT,
    mkAppT,
    AppT (..),
    ServerSettings (..),
    RegistrationStatus (..),
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow, throw)
import qualified Haka.Logger as Log
import qualified Hasql.Pool as HqPool
import Katip as K
import qualified Network.HTTP.Req as Req

data LogState = LogState
  { lsContext :: !K.LogContexts,
    lsNamespace :: !K.Namespace,
    lsLogEnv :: !K.LogEnv
  }

data AppCtx = AppCtx
  { pool :: HqPool.Pool,
    logState :: LogState,
    srvSettings :: ServerSettings
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
      MonadCatch,
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

instance (MonadIO m, MonadThrow m) => Req.MonadHttp (AppT m) where
  handleHttpException = throw

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

data RegistrationStatus
  = EnabledRegistration
  | DisabledRegistration

data RemoteWriteConfig = RemoteWriteConfig
  { -- Wakatime compatible endpoint that can receive heartbeats.
    heartbeatUrl :: Text,
    -- Authentication token to use with the request.
    token :: Text
  }

-- | Server configuration settings.
data ServerSettings = ServerSettings
  { -- | Where the service will listen to.
    hakaPort :: Int,
    -- | If the api calls are made behind a proxy with a prefix,
    -- we'll have to adjust the Set-Cookie path.
    hakaApiPrefix :: String,
    -- | The external URL to be used for the badge generation.
    hakaBadgeUrl :: ByteString,
    -- | Where to look for dashboard's static files.
    hakaDashboardPath :: FilePath,
    -- | Whether the registration is enabled.
    hakaEnableRegistration :: RegistrationStatus,
    -- | Maximum duration of the dashboard session without activity.
    hakaSessionExpiry :: Int64,
    -- | A shields.io compatible endpoint to use for badge generation.
    hakaShieldsIOUrl :: String,
    -- | Control the logger output format. json for prod, key-value pair for dev.
    hakaRunEnv :: Log.EnvType,
    -- | Verbosity level.
    hakaLogLevel :: String,
    -- | Whether to log the HTTP requests.
    hakaHasHttpLogger :: Bool,
    -- | Configuration regarding remote write to a Wakatime compatible server.
    hakaRemoteWriteConfig :: Maybe RemoteWriteConfig
  }
