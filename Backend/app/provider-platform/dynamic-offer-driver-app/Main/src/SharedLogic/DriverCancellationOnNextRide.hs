{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverCancellationOnNextRide
  ( accumulateCancellationOnDeductionNextRideFee,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude
import Kernel.Prelude hiding (any, elem, map)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Ride as QRide
import Tools.Constants
import Tools.Error
import Tools.Metrics as Metrics
import TransactionLogs.Types

accumulateCancellationOnDeductionNextRideFee ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasShortDurationRetryCfg r c
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  [LYT.TagNameValue] ->
  DTC.TransporterConfig ->
  m ()
accumulateCancellationOnDeductionNextRideFee _booking ride rideTags transporterConfig = do
  when (validCancellationPenaltyApplicable `elem` rideTags && isJust transporterConfig.cancellationDeductionOnNextRideAmount) $ do
    case transporterConfig.cancellationDeductionOnNextRideAmount of
      Just deductionAmount -> do
        -- Get current driver information to read existing deduction amount
        driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound

        -- Calculate new accumulated amount with lock to prevent race conditions
        Redis.whenWithLockRedis (cancellationDeductionLockKey ride.driverId.getId) 10 $ do
          let currentAmount = fromMaybe 0 driverInfo.cancellationDeductionOnNextRideAmount
              uncappedAmount = currentAmount + deductionAmount
              maxAllowed = fromMaybe uncappedAmount transporterConfig.maxCancellationDeductionOnNextRideAmount
              newAmount = min uncappedAmount maxAllowed
              actualDeduction = newAmount - currentAmount -- The amount actually added in this ride

          -- Update driver information with new accumulated amount
          QDI.updateCancellationDeductionOnNextRideAmount (Just newAmount) ride.driverId

          -- Update ride with the deduction amount for this specific ride
          QRide.updateDriverCancellationDeductionOnNextRide (Just actualDeduction) ride.id

          logInfo $
            "Updated cancellation deduction for driver " <> ride.driverId.getId
              <> " from ₹"
              <> show currentAmount
              <> " to ₹"
              <> show newAmount
              <> " (added ₹"
              <> show actualDeduction
              <> " for ride "
              <> ride.id.getId
              <> ")"
      Nothing ->
        pure ()

cancellationDeductionLockKey :: Text -> Text
cancellationDeductionLockKey driverId = "Driver:Cancellation:Deduction:DriverId-" <> driverId
