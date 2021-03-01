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

import Data.Text.Internal.Builder
import Katip
import Katip.Format.Time (formatAsIso8601)
import Katip.Scribes.Handle
import qualified Network.Wai as Wai
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

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
  logEnv <- initLogEnv (Namespace []) (Environment {getEnvironment = "dev"})
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
  logEnv <- initLogEnv (Namespace []) (Environment {getEnvironment = "prod"})
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

pairFormat :: LogItem a => ItemFormatter a
pairFormat withColor verb Item {..} =
  pair "ts" nowStr
    <> pair "level" (renderSeverity' _itemSeverity)
    <> pair "host" (toText _itemHost)
    <> mconcat ks
    <> pair "msg" (toStrict $ toLazyText $ unLogStr _itemMessage)
  where
    ks = map (<> " ") $ getKeys verb _itemPayload
    nowStr = formatAsIso8601 _itemTime
    renderSeverity' severity =
      colorBySeverity withColor severity (renderSeverity severity)
