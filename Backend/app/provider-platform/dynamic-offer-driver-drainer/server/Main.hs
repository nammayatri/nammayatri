-- {-# OPTIONS_GHC -Wno-unused-local-binds #-}
-- {-# OPTIONS_GHC -Wno-unused-matches     #-}

module Main where

-- import Prelude

-- main :: IO ()
-- main = pure ()

import Config.Config
import Config.Env as Env
import qualified Constants as C
import Control.Concurrent.Async (async, cancel)
import qualified DBSync.DBSync as DBSync
import qualified Data.HashSet as HS
import qualified "unordered-containers" Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Euler.WebService.Database.EulerDB as EulerDB
import qualified EulerHS.Interpreters as R
import EulerHS.Logger.Types
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import qualified Event.Event as Event
import qualified Events.Network as NW
import qualified System.Directory as SD
import Types.DBSync
import Utils.Logging as Logging
import Utils.Utils

main :: IO ()
main = do
  config <- Config.config'
  logFilepath <- Env.getLogFilePath
  logFile <- case logFilepath of
    Just path -> return path
    Nothing -> do
      _ <- SD.createDirectoryIfMissing True "app/logs/"
      return "app/logs/app.log"

  logToFile <- Env.getLogToFile
  logAsync <- Env.getLogAsync
  logToConsole <- Env.getLogToConsole
  logRawSql <- Env.getLogRawSql
  logAPI' <- Env.getLogAPI
  logLevel <- Env.getLogLevel
  logFormatter <- Env.getLogFormatter
  let shouldLogSql =
        if logRawSql
          then ET.UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
          else ET.SafelyOmitSqlLogs

  whitelistedLoggingKeysText <- Logging.getwhiteListedLoggingKeys
  let mWhitelistedLoggingKeys = decodeFromText whitelistedLoggingKeysText :: Maybe [Text]
  whitelistedRef <- newIORef (HS.fromList <$> mWhitelistedLoggingKeys)

  let formatter = Logging.drainerFlowFormatter whitelistedRef
  let loggingMask =
        ET.LogMaskingConfig
          { _maskKeys = HashSet.fromList ["accountNumber"],
            _maskText = Just "XXXXXXXXX",
            _keyType = ET.BlackListKey
          }
  let loggerCfg =
        ET.defaultLoggerConfig
          { ET._logToFile = logToFile,
            ET._logLevel = logLevel,
            ET._logFilePath = logFile,
            ET._isAsync = logAsync,
            ET._logRawSql = shouldLogSql,
            ET._logAPI = logAPI',
            ET._logMaskingConfig = Nothing,
            ET._logToConsole = logToConsole -- uncomment this for perf in production
          }

  bracket (async NW.runMetricServer) cancel $ \_ -> do
    R.withFlowRuntime (Just (R.createLoggerRuntime' Nothing (Just Logging.drainerRenderer) 4096 formatter Nothing loggerCfg)) $
      ( \_config flowRt' -> do
          putStrLn @String "Initializing DB and KV Connections..."
          let loggerRuntime = R._loggerRuntime $ R._coreRuntime flowRt'
          try (R.runFlow flowRt' EulerDB.prepareDBConnections) >>= \case
            Left (e :: SomeException) -> putStrLn @String ("Exception thrown while running dbConfig: " <> show e)
            Right _ -> do
              dbSyncMetric <- Event.mkDBSyncMetric
              let environment = Env (T.pack C.kvRedis) dbSyncMetric
              threadPerPodCount <- Env.getThreadPerPodCount
              -- handle graceful shutdown of threads
              DBSync.spawnDrainerThread threadPerPodCount flowRt' environment
              R.runFlow flowRt' (runReaderT DBSync.startDBSync environment)
      )
        config
