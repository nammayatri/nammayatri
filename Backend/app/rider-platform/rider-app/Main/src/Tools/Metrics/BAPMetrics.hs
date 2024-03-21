{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.BAPMetrics
  ( module Tools.Metrics.BAPMetrics,
    module Reexport,
  )
where

import Data.Time (diffUTCTime)
import GHC.Records.Extra
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion)
import Kernel.Types.Common
import Prometheus as P
import Tools.Metrics.BAPMetrics.Types as Reexport

data ActionFRFS = SEARCH | INIT | CONFIRM | CANCEL

deriving instance Show ActionFRFS

startSearchMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => Text -> Text -> m ()
startSearchMetrics merchantName txnId = do
  bmContainer <- asks (.bapMetrics)
  version <- asks (.version)
  startSearchMetrics' bmContainer merchantName version txnId

finishSearchMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => Text -> Text -> m ()
finishSearchMetrics merchantName txnId = do
  bmContainer <- asks (.bapMetrics)
  version <- asks (.version)
  finishSearchMetrics' bmContainer merchantName version txnId

startMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => ActionFRFS -> Text -> Text -> Text -> m ()
startMetrics actionFRFS merchantName txnId merchantOperatingCityId = do
  bmContainer <- asks (.bapMetrics)
  version <- asks (.version)
  startMetrics' bmContainer actionFRFS merchantName version txnId merchantOperatingCityId

finishMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => ActionFRFS -> Text -> Text -> Text -> m ()
finishMetrics actionFRFS merchantName txnId merchantOperatingCityId = do
  bmContainer <- asks (.bapMetrics)
  version <- asks (.version)
  finishMetrics' bmContainer actionFRFS merchantName version txnId merchantOperatingCityId

incrementRideCreatedRequestCount :: HasBAPMetrics m r => Text -> Text -> Text -> m ()
incrementRideCreatedRequestCount merchantId merchantOperatingCityId category = do
  bmContainer <- asks (.bapMetrics)
  version <- asks (.version)
  incrementRideCreatedRequestCount' bmContainer merchantId merchantOperatingCityId version category

incrementRideCreatedRequestCount' :: MonadIO m => BAPMetricsContainer -> Text -> Text -> DeploymentVersion -> Text -> m ()
incrementRideCreatedRequestCount' bmContainer merchantId merchantOperatingCityId version category = do
  let rideCreatedCounter = bmContainer.rideCreatedCounter
  liftIO $ P.withLabel rideCreatedCounter (merchantId, version.getDeploymentVersion, category, merchantOperatingCityId) P.incCounter

incrementSearchRequestCount :: HasBAPMetrics m r => Text -> Text -> m ()
incrementSearchRequestCount merchantName merchantOperatingCityId = do
  bmContainer <- asks (.bapMetrics)
  version <- asks (.version)
  incrementSearchRequestCount' bmContainer merchantName merchantOperatingCityId version

incrementSearchRequestCount' :: MonadIO m => BAPMetricsContainer -> Text -> Text -> DeploymentVersion -> m ()
incrementSearchRequestCount' bmContainer merchantName merchantOperatingCityId version = do
  let searchRequestCounter = bmContainer.searchRequestCounter
  liftIO $ P.withLabel searchRequestCounter (merchantName, version.getDeploymentVersion, merchantOperatingCityId) P.incCounter

putSearchDuration :: MonadIO m => P.Vector P.Label2 P.Histogram -> Text -> DeploymentVersion -> Double -> m ()
putSearchDuration searchDurationHistogram merchantName version duration = liftIO $ P.withLabel searchDurationHistogram (merchantName, version.getDeploymentVersion) (`P.observe` duration)

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

putDuration :: MonadIO m => P.Vector P.Label3 P.Histogram -> Text -> DeploymentVersion -> Text -> Double -> m ()
putDuration durationHistogram merchantName version merchantOperatingCityId duration = liftIO $ P.withLabel durationHistogram (merchantName, version.getDeploymentVersion, merchantOperatingCityId) (`P.observe` duration)

durationKeyFRFS :: Text -> ActionFRFS -> Text
durationKeyFRFS txnId action = "beckn:" <> txnId <> ":on_" <> show action <> "_frfs:received"

durationLockKeyFRFS :: Text -> ActionFRFS -> Text
durationLockKeyFRFS txnId action = txnId <> ":on_" <> show action <> "_frfs"

startSearchMetrics' :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => BAPMetricsContainer -> Text -> DeploymentVersion -> Text -> m ()
startSearchMetrics' bmContainer merchantName version txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExp (searchDurationKey txnId) startTime (searchRedisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "Gateway Search Metrics" $ do
    liftIO $ threadDelay $ searchRedisExTime * 1000000
    Redis.whenWithLockRedis (searchDurationLockKey txnId) searchRedisExTime $ do
      Redis.get (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.del (searchDurationKey txnId)
          liftIO $ P.withLabel failureCounter (merchantName, version.getDeploymentVersion) P.incCounter
        Nothing -> return ()

finishSearchMetrics' :: (Redis.HedisFlow m r, MonadTime m, MonadMask m) => BAPMetricsContainer -> Text -> DeploymentVersion -> Text -> m ()
finishSearchMetrics' bmContainer merchantName version txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  Redis.whenWithLockRedis (searchDurationLockKey txnId) searchRedisExTime $ do
    Redis.get (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.del (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram merchantName version . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()

startMetrics' :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => BAPMetricsContainer -> ActionFRFS -> Text -> DeploymentVersion -> Text -> Text -> m ()
startMetrics' bmContainer actionFRFS merchantName version txnId merchantOperatingCityId = do
  let (_, failureCounter) = case actionFRFS of
        SEARCH -> bmContainer.searchDurationFRFS
        INIT -> bmContainer.initDurationFRFS
        CONFIRM -> bmContainer.confirmDurationFRFS
        CANCEL -> bmContainer.cancelDurationFRFS
      redisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExp (durationKeyFRFS txnId actionFRFS) startTime (redisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "Gateway Search Metrics" $ do
    liftIO $ threadDelay $ redisExTime * 1000000
    Redis.whenWithLockRedis (durationLockKeyFRFS txnId actionFRFS) redisExTime $ do
      Redis.get (durationKeyFRFS txnId actionFRFS) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.del (durationKeyFRFS txnId actionFRFS)
          liftIO $ P.withLabel failureCounter (merchantName, version.getDeploymentVersion, merchantOperatingCityId) P.incCounter
        Nothing -> return ()

finishMetrics' :: (Redis.HedisFlow m r, MonadTime m, MonadMask m) => BAPMetricsContainer -> ActionFRFS -> Text -> DeploymentVersion -> Text -> Text -> m ()
finishMetrics' bmContainer actionFRFS merchantName version txnId merchantOperatingCityId = do
  let (durationHistogram, _) = case actionFRFS of
        SEARCH -> bmContainer.searchDurationFRFS
        INIT -> bmContainer.initDurationFRFS
        CONFIRM -> bmContainer.confirmDurationFRFS
        CANCEL -> bmContainer.cancelDurationFRFS
      redisExTime = getSeconds bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  Redis.whenWithLockRedis (durationLockKeyFRFS txnId actionFRFS) redisExTime $ do
    Redis.get (durationKeyFRFS txnId actionFRFS) >>= \case
      Just startTime -> do
        void $ Redis.del (durationKeyFRFS txnId actionFRFS)
        putDuration durationHistogram merchantName version merchantOperatingCityId . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
