{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import qualified Data.Aeson as J
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GenerateKeyPair (generateKeyPair)
import Kernel.Types.Logging
  ( LogLevel (DEBUG),
    LoggerConfig (..),
  )
import Kernel.Utils.FlowLogging (getEulerLoggerRuntime)
import Kernel.Utils.Logging
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    flag',
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

data Mode
  = GenerateRequestsForLoadTest
      !ByteString
      !Int
      !Text
      !Text
  | GenerateKeyPair
  deriving (Show, Eq)

main :: IO ()
main = do
  mode <- execParser opts
  case mode of
    GenerateRequestsForLoadTest privateKey requests url filePath ->
      runWithFlowRuntime $ do
        prepareDataForLoadTest privateKey requests filePath
        L.logInfo @Text "GenerateRequestsForLoadTest" "Start K6 script..."
        result <- runK6Script url filePath requests
        L.logInfo @Text "GenerateRequestsForLoadTest" $ fromString result
        cleanupData filePath
    GenerateKeyPair ->
      runWithFlowRuntime $ do
        keyPairResponse <- generateKeyPair
        L.runIO $ putStrLn @Text $ decodeUtf8 $ J.encode keyPairResponse
  exitSuccess
  where
    opts = info (mode <**> helper) fullDesc

mode :: Parser Mode
mode =
  GenerateRequestsForLoadTest
    <$> strOption privateKey
      <*> option auto requests
      <*> strOption url
      <*> strOption filePath
    <|> flag' GenerateKeyPair (long "generate-key-pair" <> help "Generate public/private key pair.")
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
    url =
      long "url"
        <> metavar "URL"
        <> help "URL to test"
        <> showDefault
        <> value "http://127.0.0.1:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"
    filePath =
      long "file-path"
        <> metavar "FILEPATH"
        <> help "Path to file with generated data."
        <> showDefault
        <> value "/tmp/req-data.json"

runWithFlowRuntime :: L.Flow a -> IO a
runWithFlowRuntime flow = do
  let logRuntime =
        getEulerLoggerRuntime (Just "cli") $
          LoggerConfig
            { level = DEBUG,
              logToFile = False,
              logFilePath = "",
              logToConsole = True,
              logRawSql = False,
              prettyPrinting = False
            }
  R.withFlowRuntime (Just logRuntime) $ \flowRt -> I.runFlow flowRt flow
