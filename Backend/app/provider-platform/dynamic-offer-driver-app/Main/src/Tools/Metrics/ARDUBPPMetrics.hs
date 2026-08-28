{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.ARDUBPPMetrics
  ( module Tools.Metrics.ARDUBPPMetrics,
    module Reexport,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Kernel.Types.Common
import Kernel.Utils.Time (getClockTimeInMs)
import Prometheus as P
import Tools.Metrics.ARDUBPPMetrics.Types as Reexport

putFareAndDistanceDeviations :: (MonadIO m, HasBPPMetrics m r) => Text -> Money -> Meters -> m ()
putFareAndDistanceDeviations agencyName fareDiff distanceDiff = do
  countingDeviationMetric <- asks (.bppMetrics.countingDeviation)
  version <- asks (.version)
  liftIO $ P.withLabel countingDeviationMetric.realFareDeviation (agencyName, version.getDeploymentVersion) (`P.observe` fromIntegral fareDiff)
  liftIO $ P.withLabel countingDeviationMetric.realDistanceDeviation (agencyName, version.getDeploymentVersion) (`P.observe` fromIntegral distanceDiff)

incrementSearchRequestCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> m ()
incrementSearchRequestCount merchantId merchantOpCityId distanceBucket = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.searchRequestCounter (merchantId, merchantOpCityId, distanceBucket, version.getDeploymentVersion) P.incCounter

incrementSearchTryCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> Text -> Text -> m ()
incrementSearchTryCount merchantId merchantOpCityId vehicleServiceTier searchRepeatType distanceBucket = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.searchTryCounter (merchantId, merchantOpCityId, vehicleServiceTier, searchRepeatType, distanceBucket, version.getDeploymentVersion) P.incCounter

addSearchRequestSentToDriverCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> (Text, Text, Text) -> Int -> m ()
addSearchRequestSentToDriverCount merchantId merchantOpCityId vehicleServiceTier (distanceBucket, poolingLogicV, poolingConfigV) count = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.searchRequestSentToDriverCounter (merchantId, merchantOpCityId, vehicleServiceTier, distanceBucket, poolingLogicV, poolingConfigV, version.getDeploymentVersion) (void . (`P.addCounter` fromIntegral count))

addSearchRequestExpiredCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> (Text, Text, Text) -> Int -> m ()
addSearchRequestExpiredCount merchantId merchantOpCityId vehicleServiceTier (distanceBucket, poolingLogicV, poolingConfigV) count = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.searchRequestExpiredCounter (merchantId, merchantOpCityId, vehicleServiceTier, distanceBucket, poolingLogicV, poolingConfigV, version.getDeploymentVersion) (void . (`P.addCounter` fromIntegral count))

incrementRiderAcceptanceCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> Text -> m ()
incrementRiderAcceptanceCount merchantId merchantOpCityId vehicleServiceTier distanceBucket = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.riderAcceptanceCounter (merchantId, merchantOpCityId, vehicleServiceTier, distanceBucket, version.getDeploymentVersion) P.incCounter

incrementBookingCreatedCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> Text -> m ()
incrementBookingCreatedCount merchantId merchantOpCityId vehicleServiceTier distanceBucket = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.bookingCreatedCounter (merchantId, merchantOpCityId, vehicleServiceTier, version.getDeploymentVersion) P.incCounter

incrementRideCreatedCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> m ()
incrementRideCreatedCount merchantId merchantOpCityId vehicleServiceTier = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.rideCreatedCounter (merchantId, merchantOpCityId, vehicleServiceTier, version.getDeploymentVersion) P.incCounter

incrementRideStartedCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> m ()
incrementRideStartedCount merchantId merchantOpCityId vehicleServiceTier = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.rideStartedCounter (merchantId, merchantOpCityId, vehicleServiceTier, version.getDeploymentVersion) P.incCounter

incrementRideCompletedCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> m ()
incrementRideCompletedCount merchantId merchantOpCityId vehicleServiceTier = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.rideCompletedCounter (merchantId, merchantOpCityId, vehicleServiceTier, version.getDeploymentVersion) P.incCounter

incrementRideCancelledCount :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> Text -> Text -> m ()
incrementRideCancelledCount merchantId merchantOpCityId vehicleServiceTier cancellationSource = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  liftIO $ P.withLabel bmContainer.rideCancelledCounter (merchantId, merchantOpCityId, vehicleServiceTier, cancellationSource, version.getDeploymentVersion) P.incCounter

type SearchMetricsMVar = MVar Milliseconds

startSearchMetrics :: HasBPPMetrics m r => Text -> m SearchMetricsMVar
startSearchMetrics agencyName = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  startSearchMetrics' agencyName version bmContainer

finishSearchMetrics :: HasBPPMetrics m r => Text -> SearchMetricsMVar -> m ()
finishSearchMetrics agencyName searchMetricsMVar = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  finishSearchMetrics' agencyName version bmContainer searchMetricsMVar

putSearchDuration :: L.MonadFlow m => Text -> DeploymentVersion -> P.Vector P.Label2 P.Histogram -> Double -> m ()
putSearchDuration agencyName version searchDurationHistogram duration =
  L.runIO $
    P.withLabel
      searchDurationHistogram
      (agencyName, version.getDeploymentVersion)
      (`P.observe` duration)

startSearchMetrics' :: MonadFlow m => Text -> DeploymentVersion -> BPPMetricsContainer -> m SearchMetricsMVar
startSearchMetrics' agencyName version bmContainer = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchDurationTimeout = getSeconds bmContainer.searchDurationTimeout
  startTime <- getClockTimeInMs
  searchMetricsMVar <- liftIO $ newMVar startTime
  fork "BPP Search Metrics" $ do
    liftIO $ threadDelay $ searchDurationTimeout * 1000000
    whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \_ -> do
      liftIO $ P.withLabel failureCounter (agencyName, version.getDeploymentVersion) P.incCounter
  return searchMetricsMVar

finishSearchMetrics' ::
  MonadFlow m =>
  Text ->
  DeploymentVersion ->
  BPPMetricsContainer ->
  SearchMetricsMVar ->
  m ()
finishSearchMetrics' agencyName version bmContainer searchMetricsMVar = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
  whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \startTime -> do
    endTime <- getClockTimeInMs
    putSearchDuration agencyName version searchDurationHistogram $ fromIntegral $ endTime - startTime
