{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.CancellationMetrics
  ( module Tools.Metrics.CancellationMetrics,
    module Reexport,
  )
where

import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Tools.Metrics.CancellationMetrics.Types as Reexport

-- | Increment ride cancellation counter
-- cancelledBy: "rider" | "driver" | "system"
-- reason: "found_cheaper" | "long_wait" | "wrong_address" | "short_distance" | "wrong_direction" | "other"
incrementCancellation ::
  (MonadIO m, HasCancellationMetrics m r) =>
  Text -> -- city
  Text -> -- cancelled_by
  Text -> -- reason
  Text -> -- vehicle_type
  m ()
incrementCancellation city cancelledBy reason vehicleType = do
  container <- asks (.cancellationMetrics)
  liftIO $ P.withLabel container.cancellationCounter (city, cancelledBy, reason, vehicleType) P.incCounter

-- | Increment post-acceptance cancellation counter
-- minutesAfterBucket: "0-2" | "2-5" | "5-10" | "10+"
incrementPostAcceptanceCancellation ::
  (MonadIO m, HasCancellationMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- cancelled_by
  Text -> -- minutes_after_bucket
  m ()
incrementPostAcceptanceCancellation city zone cancelledBy minutesAfterBucket = do
  container <- asks (.cancellationMetrics)
  liftIO $ P.withLabel container.postAcceptanceCancellationCounter (city, zone, cancelledBy, minutesAfterBucket) P.incCounter

-- | Increment passive cancellation counter (driver made rider cancel)
incrementPassiveCancellation ::
  (MonadIO m, HasCancellationMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  m ()
incrementPassiveCancellation city zone = do
  container <- asks (.cancellationMetrics)
  liftIO $ P.withLabel container.passiveCancellationCounter (city, zone) P.incCounter

-- | Increment reallocation outcome counter
-- result: "matched" | "timeout" | "rider_cancelled"
incrementReallocationSuccess ::
  (MonadIO m, HasCancellationMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- result
  m ()
incrementReallocationSuccess city zone result = do
  container <- asks (.cancellationMetrics)
  liftIO $ P.withLabel container.reallocationSuccessCounter (city, zone, result) P.incCounter

-- | Observe reallocation duration in seconds
observeReallocationDuration ::
  (MonadIO m, HasCancellationMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Double -> -- duration in seconds
  m ()
observeReallocationDuration city zone duration = do
  container <- asks (.cancellationMetrics)
  liftIO $ P.withLabel container.reallocationDurationHist (city, zone) (`P.observe` duration)

-- | Increment cancellation penalty counter
-- penaltyType: "fee" | "block" | "warning"
incrementCancellationPenalty ::
  (MonadIO m, HasCancellationMetrics m r) =>
  Text -> -- city
  Text -> -- vehicle_type
  Text -> -- penalty_type
  m ()
incrementCancellationPenalty city vehicleType penaltyType = do
  container <- asks (.cancellationMetrics)
  liftIO $ P.withLabel container.cancellationPenaltyCounter (city, vehicleType, penaltyType) P.incCounter

-- | Compute the minutes-after-acceptance bucket from elapsed seconds
minutesAfterBucket :: Int -> Text
minutesAfterBucket elapsedSeconds
  | elapsedSeconds < 120 = "0-2"
  | elapsedSeconds < 300 = "2-5"
  | elapsedSeconds < 600 = "5-10"
  | otherwise = "10+"
