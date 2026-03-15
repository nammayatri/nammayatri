{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.ARDUBPPMetrics.Types
  ( HasBPPMetrics,
    BPPMetricsContainer (..),
    module CoreMetrics,
    registerBPPMetricsContainer,
    incrementNotificationFailure,
    incrementNotificationSuccess,
    incrementStaleTokenCleanup,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Utils.Common
import Prometheus as P

type HasBPPMetrics m r = (HasFlowEnv m r ["bppMetrics" ::: BPPMetricsContainer, "version" ::: DeploymentVersion])

type SearchDurationMetric = (P.Vector P.Label2 P.Histogram, P.Vector P.Label2 P.Counter)

data BPPMetricsContainer = BPPMetricsContainer
  { searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric,
    countingDeviation :: CountingDeviationMetric,
    notificationDeliveryFailure :: P.Vector P.Label2 P.Counter,
    notificationDeliverySuccess :: P.Counter,
    staleTokenCleanup :: P.Counter
  }

data CountingDeviationMetric = CountingDeviationMetric
  { realFareDeviation :: P.Vector P.Label2 P.Histogram,
    realDistanceDeviation :: P.Vector P.Label2 P.Histogram
  }

registerBPPMetricsContainer :: Seconds -> IO BPPMetricsContainer
registerBPPMetricsContainer searchDurationTimeout = do
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  countingDeviation <- registerCountingDeviationMetric
  notificationDeliveryFailure <- registerNotificationFailureMetric
  notificationDeliverySuccess <- registerNotificationSuccessMetric
  staleTokenCleanup <- registerStaleTokenCleanupMetric
  return $ BPPMetricsContainer {..}

registerNotificationFailureMetric :: IO (P.Vector P.Label2 P.Counter)
registerNotificationFailureMetric =
  P.register $
    P.vector ("error_type", "notification_category") $
      P.counter $
        P.Info
          "notification_delivery_failure_total"
          "Total FCM notification delivery failures by error type and category"

registerNotificationSuccessMetric :: IO P.Counter
registerNotificationSuccessMetric =
  P.register $
    P.counter $
      P.Info
        "notification_delivery_success_total"
        "Total FCM notification deliveries attempted"

registerStaleTokenCleanupMetric :: IO P.Counter
registerStaleTokenCleanupMetric =
  P.register $
    P.counter $
      P.Info
        "fcm_stale_token_cleanup_total"
        "Total stale FCM device tokens cleaned up"

incrementNotificationFailure :: (MonadIO m, HasBPPMetrics m r) => Text -> Text -> m ()
incrementNotificationFailure errorType category = do
  bmContainer <- asks (.bppMetrics)
  liftIO $ P.withLabel bmContainer.notificationDeliveryFailure (errorType, category) P.incCounter

incrementNotificationSuccess :: (MonadIO m, HasBPPMetrics m r) => m ()
incrementNotificationSuccess = do
  bmContainer <- asks (.bppMetrics)
  liftIO $ P.incCounter bmContainer.notificationDeliverySuccess

incrementStaleTokenCleanup :: (MonadIO m, HasBPPMetrics m r) => m ()
incrementStaleTokenCleanup = do
  bmContainer <- asks (.bppMetrics)
  liftIO $ P.incCounter bmContainer.staleTokenCleanup

registerCountingDeviationMetric :: IO CountingDeviationMetric
registerCountingDeviationMetric =
  CountingDeviationMetric
    <$> (P.register . P.vector ("agency_name", "version") $ P.histogram fareDeviation $ aroundZero 10 5)
    <*> (P.register . P.vector ("agency_name", "version") $ P.histogram distanceDeviation $ aroundZero 10 6)
  where
    aroundZero factor b =
      let l = P.exponentialBuckets 1 factor b
       in reverse (map negate l) ++ l
    fareDeviation =
      P.Info
        "BPP_fare_deviation"
        "Difference between initially offered and recalculated fare of a ride"
    distanceDeviation =
      P.Info
        "BPP_distance_deviation"
        "Difference between estimated distance and real distance of a ride"

registerSearchDurationMetric :: Seconds -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  searchDurationHistogram <-
    P.register $
      P.vector ("agency_name", "version") $
        P.histogram
          infoSearchDuration
          buckets
  failureCounter <-
    P.register $
      P.vector ("agency_name", "version") $
        P.counter $ P.Info "BPP_search_failure_counter" ""

  pure (searchDurationHistogram, failureCounter)
  where
    infoSearchDuration =
      P.Info
        "BPP_search_time"
        ""
    buckets =
      P.linearBuckets
        0
        0.5
        searchDurationBucketCount
    searchDurationBucketCount = min 20 $ (getSeconds searchDurationTimeout + 1) * 2
