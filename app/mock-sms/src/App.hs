{-# LANGUAGE QuantifiedConstraints #-}

module App
  ( runMockSms,
  )
where

import App.Routes (mockSmsAPI, mockSmsServer)
import App.Types
import Beckn.Prelude
import Beckn.Types.Logging
import Beckn.Utils.Servant.Server
import Servant

runMockSms :: (AppCfg -> AppCfg) -> IO ()
runMockSms configModifier = do
  appEnv <- buildAppEnv $ configModifier defaultConfig
  runServerService appEnv mockSmsAPI mockSmsServer identity identity EmptyContext releaseAppEnv pure

defaultConfig :: AppCfg
defaultConfig =
  AppCfg
    { port = 4343,
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/mock-sms.log",
            logToConsole = True,
            logRawSql = True,
            prettyPrinting = True
          },
      graceTerminationPeriod = 90
    }
