module Config.Env where

import Constants
import Data.Aeson as A
import Data.ByteString.Lazy as DBL
import Data.Text as T
import Data.Text.Encoding as DTE
-- import Euler.WebService.Config.EnvVars as Env
import EulerHS.Prelude
import EulerHS.Types as ET
import Juspay.Extra.Config (lookupEnvT)
import System.Environment as SE

getNodeEnvironment :: IO [Char]
getNodeEnvironment = fromMaybe "development" <$> SE.lookupEnv nodeEnvironmentEnvKey

getDatabaseHost :: IO [Char]
getDatabaseHost = fromMaybe "localhost" <$> SE.lookupEnv dbHostEnvKey

getDatabasePort :: IO Word16
getDatabasePort = fromMaybe 5432 . (>>= readMaybe) <$> SE.lookupEnv dbPortEnvKey

getDatabaseUser :: IO [Char]
getDatabaseUser = fromMaybe "amazon_beta_usr_001" <$> SE.lookupEnv dbUserEnvKey

getDatabaseName :: IO [Char]
getDatabaseName = fromMaybe "axisbiz01" <$> SE.lookupEnv dbUserEnvKey

getDatabasePassword :: IO [Char]
getDatabasePassword = fromMaybe "nodefaultvalue" <$> SE.lookupEnv dbPasswordEnvKey

getDBSyncStream :: IO [Char]
getDBSyncStream = fromMaybe "rider-db-sync-stream" <$> SE.lookupEnv dbSyncStreamEnvKey

getDbPool :: IO Int
getDbPool = fromMaybe 1 . (>>= readMaybe) <$> SE.lookupEnv dbPoolEnvKey

getDbMaxIdleTimeout :: IO Int
getDbMaxIdleTimeout = fromMaybe 10 . (>>= readMaybe) <$> SE.lookupEnv dbMaxIdleTimeoutEnvKey

getDbMaxConnections :: IO Int
getDbMaxConnections = fromMaybe 50 . (>>= readMaybe) <$> SE.lookupEnv dbMaxConnectionsEnvKey

getRedisHost :: IO T.Text
getRedisHost = T.pack . fromMaybe "localhost" <$> SE.lookupEnv redisHostEnvKey

getRedisPort :: IO Word16
getRedisPort = fromMaybe 6379 . (>>= readMaybe) <$> SE.lookupEnv redisPortEnvKey

getRedisPassword :: IO (Maybe Text)
getRedisPassword = do
  isRedisPasswordReqEnabled <- getRedisPasswordReqEnabled
  case isRedisPasswordReqEnabled of
    "false" -> return Nothing
    _ -> Just . T.pack . fromMaybe "nodefaultvalue" <$> SE.lookupEnv redisAuthEnvKey

getRedisPasswordReqEnabled :: IO [Char]
getRedisPasswordReqEnabled = fromMaybe "true" <$> SE.lookupEnv redisPasswordReqEnvKey

getRedisDbValue :: IO Integer
getRedisDbValue = fromMaybe 0 . (>>= readMaybe) <$> SE.lookupEnv redisDbValueEnvKey

getRedisSocketKeepAlive :: IO Bool
getRedisSocketKeepAlive = fromMaybe True . (>>= (\val -> if (val == "false") || (val == "False") then Just False else Nothing)) <$> SE.lookupEnv redisSocketKeepAliveEnvKey

getMaskingEnabled :: IO Bool
getMaskingEnabled = do
  envRes <- (>>= readMaybe) <$> SE.lookupEnv maskingEnabledEnvKey
  defaultRes <- getDefaultMask <$> getNodeEnvironment
  return $ fromMaybe defaultRes envRes
  where
    getDefaultMask :: String -> Bool
    getDefaultMask env' =
      case env' of
        "uat" -> True
        "axis_uat" -> True
        "production" -> True
        _ -> False

getLogFilePath :: IO (Maybe String)
getLogFilePath = SE.lookupEnv logFileEnvKey

getLogToFile :: IO Bool
getLogToFile = fromMaybe True . (>>= readMaybe) <$> SE.lookupEnv logToFileEnvKey

getLogAsync :: IO Bool
getLogAsync = fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv logAsyncEnvKey

getLogToConsole :: IO Bool
getLogToConsole = fromMaybe True . (>>= readMaybe) <$> SE.lookupEnv logToConsoleEnvKey

getLogRawSql :: IO Bool
getLogRawSql = fromMaybe True . (>>= readMaybe) <$> SE.lookupEnv logRawSqlEnvKey

getLogAPI :: IO Bool
getLogAPI = fromMaybe False . (>>= readMaybe) <$> SE.lookupEnv logAPIEnvKey

getLogLevel :: IO LogLevel
getLogLevel = fromMaybe ET.Debug . (>>= readMaybe) <$> SE.lookupEnv logLevelEnvKey

getLogFormatter :: IO Bool
getLogFormatter = fromMaybe True . (>>= readMaybe) <$> SE.lookupEnv logFormatterEnvKey

gracefulShutDownPeriodInMs :: IO Int
gracefulShutDownPeriodInMs = (1000000 *) . fromMaybe defaultGracefulShutDownPeriodInMs . (>>= readMaybe) <$> SE.lookupEnv gracefulShutDownPeriodSecondsEnvKey

getStreamBlockTime :: IO (Maybe Integer)
getStreamBlockTime = (\ms -> if ms == 0 then Nothing else Just ms) <$> (fromMaybe 100 . (>>= readMaybe) <$> SE.lookupEnv streamBlockTimeEnvKey)

hostname :: Text
hostname = T.pack "" -- Env.getApplicationHostname

env :: Text
env = T.pack "" -- Env._getEnv

sourceCommit :: Text
sourceCommit = T.pack "" --Env.sourceCommit

getWhitelistedKeysFromEnv :: Text
getWhitelistedKeysFromEnv = fromMaybe (DTE.decodeUtf8 . DBL.toStrict . A.encode $ getWhitelistedLoggingKeys) $ lookupEnvT whitelistedLoggingKeys

getRetryDelay :: IO Int
getRetryDelay = fromMaybe 1000000 . (>>= readMaybe) <$> SE.lookupEnv dbFailureRetryDelayEnvKey

getDrainerRetryDelay :: IO Int
getDrainerRetryDelay = fromMaybe 5000000 . (>>= readMaybe) <$> SE.lookupEnv drainerFailureRetryDelayEnvKey

getMaxRetries :: IO Int
getMaxRetries = fromMaybe 3 . (>>= readMaybe) <$> SE.lookupEnv maxDbFailureRetries

getThreadPerPodCount :: IO Int
getThreadPerPodCount = fromMaybe 0 . (>>= readMaybe) <$> SE.lookupEnv threadPerPodCountEnvKey
