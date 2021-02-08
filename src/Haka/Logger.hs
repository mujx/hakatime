{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Haka.Logger
  ( devEnv,
    prodEnv,
    logMs,
    EnvType (..),
    getLogger,
    setupLogEnv,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Builder
import qualified Data.Text.Lazy as LT
import Katip
import Katip.Core (locationToString)
import Katip.Format.Time (formatAsIso8601)
import Katip.Scribes.Handle
import qualified Network.Wai as Wai
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.IO
import Prelude hiding (unwords)

data EnvType = Dev | Prod deriving (Show, Eq)

getLogger :: EnvType -> Wai.Middleware
getLogger Prod = logStdout
getLogger Dev = logStdoutDev

setupLogEnv :: EnvType -> Severity -> IO LogEnv
setupLogEnv Prod = prodEnv
setupLogEnv Dev = devEnv

logMs :: Katip m => Severity -> Text -> m ()
logMs sev msg =
  logMsg (Namespace {unNamespace = []}) sev (LogStr {unLogStr = fromText msg})

devEnv :: Severity -> IO LogEnv
devEnv sev = do
  logEnv <- initLogEnv (Namespace [""]) (Environment {getEnvironment = "dev"})
  scribe <-
    mkHandleScribeWithFormatter
      pairFormat
      ColorIfTerminal
      stdout
      (\item -> if _itemSeverity item >= sev then return True else return False)
      V3
  registerScribe "hakatime" scribe defaultScribeSettings logEnv

prodEnv :: Severity -> IO LogEnv
prodEnv sev = do
  logEnv <- initLogEnv (Namespace [""]) (Environment {getEnvironment = "prod"})
  scribe <-
    mkHandleScribeWithFormatter
      jsonFormat
      ColorIfTerminal
      stdout
      (\item -> if _itemSeverity item >= sev then return True else return False)
      V3
  registerScribe "hakatime" scribe defaultScribeSettings logEnv

pair :: Builder -> Text -> Builder
pair k v = k <> fromText "=" <> fromText v <> fromText " "

pairFormat :: ItemFormatter a
pairFormat withColor _ Item {..} =
  pair "ts" nowStr
    <> pair "level" (renderSeverity' _itemSeverity)
    <> pair "host" (T.pack _itemHost)
    <> pair "thread_id" (getThreadIdText _itemThread)
    <> maybe mempty (pair "loc" . T.pack . locationToString) _itemLoc
    <> pair "msg" (LT.toStrict $ toLazyText $ unLogStr _itemMessage)
  where
    nowStr = formatAsIso8601 _itemTime
    renderSeverity' severity =
      colorBySeverity withColor severity (renderSeverity severity)
