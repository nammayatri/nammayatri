module Config.Env where

import Constants
import Data.Text as T
import EulerHS.Prelude
import System.Environment as SE
import Types.DBSync (DBSyncConfig (..))

defaultDBSyncConfig :: DBSyncConfig
defaultDBSyncConfig =
  DBSyncConfig
    { _emptyRetry = 50,
      _rateLimitN = 200,
      _rateLimitWindow = 100,
      _streamReadCount = 200 -- 1000
    }

getDBSyncStream :: IO [Char]
getDBSyncStream = fromMaybe "rider-db-sync-stream" <$> SE.lookupEnv dbSyncStreamEnvKey

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

getRetryDelay :: IO Int
getRetryDelay = fromMaybe 1000000 . (>>= readMaybe) <$> SE.lookupEnv dbFailureRetryDelayEnvKey

getDrainerRetryDelay :: IO Int
getDrainerRetryDelay = fromMaybe 5000000 . (>>= readMaybe) <$> SE.lookupEnv drainerFailureRetryDelayEnvKey

getMaxRetries :: IO Int
getMaxRetries = fromMaybe 3 . (>>= readMaybe) <$> SE.lookupEnv maxDbFailureRetries

getDrainerExecutionDelay :: IO Int
getDrainerExecutionDelay = fromMaybe 20000 . (>>= readMaybe) <$> SE.lookupEnv drainerExecutionDelayEnvKey

getThreadPerPodCount :: IO Int
getThreadPerPodCount = fromMaybe 0 . (>>= readMaybe) <$> SE.lookupEnv threadPerPodCount
