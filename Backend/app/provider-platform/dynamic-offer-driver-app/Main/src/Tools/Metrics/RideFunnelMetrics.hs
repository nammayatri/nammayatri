{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.RideFunnelMetrics
  ( module Tools.Metrics.RideFunnelMetrics,
    module Reexport,
  )
where

import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Tools.Metrics.RideFunnelMetrics.Types as Reexport

-- | Increment ride funnel stage counter
-- Stages: "search" -> "estimate" -> "offer" -> "accept" -> "ride_start" -> "ride_complete"
-- Results: "success" | "failure" | "drop_off"
incrementRideFunnelStage ::
  (MonadIO m, HasRideFunnelMetrics m r) =>
  Text -> -- city
  Text -> -- vehicle_type
  Text -> -- stage
  Text -> -- result
  m ()
incrementRideFunnelStage city vehicleType stage result = do
  container <- asks (.rideFunnelMetrics)
  version <- asks (.version)
  let _ = version -- version available for future use
  liftIO $ P.withLabel container.rideFunnelStageCounter (city, vehicleType, stage, result) P.incCounter

-- | Increment ride allocation request counter
-- Results: "matched" | "timeout" | "no_driver" | "cancelled"
incrementAllocationRequest ::
  (MonadIO m, HasRideFunnelMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- vehicle_type
  Text -> -- result
  m ()
incrementAllocationRequest city zone vehicleType result = do
  container <- asks (.rideFunnelMetrics)
  liftIO $ P.withLabel container.rideAllocationRequestCounter (city, zone, vehicleType, result) P.incCounter

-- | Observe ride allocation duration in seconds
observeAllocationDuration ::
  (MonadIO m, HasRideFunnelMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- vehicle_type
  Double -> -- duration in seconds
  m ()
observeAllocationDuration city zone vehicleType duration = do
  container <- asks (.rideFunnelMetrics)
  liftIO $ P.withLabel container.rideAllocationDurationHist (city, zone, vehicleType) (`P.observe` duration)

-- | Increment driver notification counter
-- Results: "sent" | "accepted" | "rejected" | "timeout"
incrementDriverNotification ::
  (MonadIO m, HasRideFunnelMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- result
  m ()
incrementDriverNotification city zone result = do
  container <- asks (.rideFunnelMetrics)
  liftIO $ P.withLabel container.driverNotificationCounter (city, zone, result) P.incCounter

-- | Observe number of drivers in an allocation batch
observeBatchDriverCount ::
  (MonadIO m, HasRideFunnelMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- batch_number (e.g. "1", "2", "3")
  Double -> -- driver count
  m ()
observeBatchDriverCount city zone batchNumber driverCount = do
  container <- asks (.rideFunnelMetrics)
  liftIO $ P.withLabel container.batchDriverCountHist (city, zone, batchNumber) (`P.observe` driverCount)

-- | Increment reallocation attempt counter
incrementReallocationAttempt ::
  (MonadIO m, HasRideFunnelMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- attempt_number (e.g. "1", "2", "3")
  m ()
incrementReallocationAttempt city zone attemptNumber = do
  container <- asks (.rideFunnelMetrics)
  liftIO $ P.withLabel container.reallocationAttemptCounter (city, zone, attemptNumber) P.incCounter

-- | Observe driver pool size at the time of search
observeDriverPoolSize ::
  (MonadIO m, HasRideFunnelMetrics m r) =>
  Text -> -- city
  Text -> -- zone
  Text -> -- vehicle_type
  Double -> -- pool size
  m ()
observeDriverPoolSize city zone vehicleType poolSize = do
  container <- asks (.rideFunnelMetrics)
  liftIO $ P.withLabel container.driverPoolSizeHist (city, zone, vehicleType) (`P.observe` poolSize)
