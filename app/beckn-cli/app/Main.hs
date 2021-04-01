{-# LANGUAGE TypeApplications #-}

module Main where

import Beckn.Utils.Logging
  ( LogLevel (DEBUG),
    LoggerConfig
      ( LoggerConfig,
        isAsync,
        level,
        logFilePath,
        logRawSql,
        logToConsole,
        logToFile
      ),
    getEulerLoggerRuntime,
  )
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    showDefault,
    strOption,
    value,
  )
import PrepareDataForLoadTest
  ( cleanupData,
    prepareDataForLoadTest,
    runK6Script,
  )

data Mode = GenerateRequestsForLoadTest !ByteString !Int deriving (Show, Eq)

main :: IO ()
main = do
  mode <- execParser opts
  case mode of
    GenerateRequestsForLoadTest privateKey requests ->
      runWithFlowRuntime $ do
        prepareDataForLoadTest privateKey requests
        L.logInfo @Text "GenerateRequestsForLoadTest" "Start K6 script..."
        result <- runK6Script
        L.logInfo @Text "GenerateRequestsForLoadTest" $ fromString result
        cleanupData
  exitSuccess
  where
    opts = info (mode <**> helper) fullDesc

mode :: Parser Mode
mode =
  GenerateRequestsForLoadTest <$> strOption privateKey <*> option auto requests
  where
    privateKey =
      long "private-key"
        <> metavar "PRIVATEKEY"
        <> help "Private key for signing requests"
    requests =
      long "requests"
        <> help "How many requests to generate"
        <> showDefault
        <> value 100
        <> metavar "INT"

runWithFlowRuntime :: L.Flow a -> IO a
runWithFlowRuntime flow = do
  let logRuntime =
        getEulerLoggerRuntime $
          LoggerConfig
            { isAsync = False,
              level = DEBUG,
              logToFile = False,
              logFilePath = "",
              logToConsole = True,
              logRawSql = False
            }
  R.withFlowRuntime (Just logRuntime) $ \flowRt -> I.runFlow flowRt flow
