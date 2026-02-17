{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.SpecialZonePayout where

import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Ride as Ride
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.Payout.Types as PT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.LocationUpdates
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import Lib.Scheduler
import SharedLogic.Allocator
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Payout as TP

sendSpecialZonePayout ::
  ( EncFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    RideEnd.EndRideFlow m r,
    LocationUpdateFlow m r c
  ) =>
  Job 'SpecialZonePayout ->
  m ExecutionResult
sendSpecialZonePayout Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let SpecialZonePayoutJobData {payoutRequestId} = jobInfo.jobData
  let lockKey = "payout:lock:" <> payoutRequestId.getId

  -- 1. Try to acquire Redis lock (5 min TTL)
  acquired <- Redis.tryLockRedis lockKey 300
  if not acquired
    then do
      logWarning $ "Could not acquire lock for payout: " <> show payoutRequestId
      pure Complete
    else do
      -- Process with lock, release on exit
      flip finally (Redis.unlockRedis lockKey) $ do
        -- 2. Fetch PayoutRequest record
        mbPayoutRequest <- PayoutRequest.getPayoutRequestById payoutRequestId

        case mbPayoutRequest of
          Nothing -> do
            logInfo $ "PayoutRequest record not found for id: " <> show payoutRequestId
            pure Complete
          Just payoutRequest ->
            executeSpecialZonePayout payoutRequest

executeSpecialZonePayout ::
  ( EncFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    RideEnd.EndRideFlow m r,
    LocationUpdateFlow m r c
  ) =>
  DPR.PayoutRequest ->
  m ExecutionResult
executeSpecialZonePayout payoutRequest = do
  let driverId = Id payoutRequest.beneficiaryId
  let merchantId = Id payoutRequest.merchantId
  let merchantOpCityId = Id payoutRequest.merchantOperatingCityId

  -- 1. Domain-specific: end ride if still in progress
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbRide <- QRide.findById (Id payoutRequest.entityId)

  whenJust mbRide $ \ride ->
    when (ride.status == Ride.INPROGRESS) $ do
      let driverReq =
            RideEnd.DriverEndRideReq
              { endRideOtp = Nothing,
                point = LatLong {lat = 0.0, lon = 0.0}, -- fix this
                requestor = person,
                uiDistanceCalculationWithAccuracy = Nothing,
                uiDistanceCalculationWithoutAccuracy = Nothing,
                odometer = Nothing,
                driverGpsTurnedOff = Nothing
              }
      shandle <- RideEnd.buildEndRideHandle merchantId merchantOpCityId (Just ride.id)
      void $ RideEnd.driverEndRide shandle ride.id driverReq

  -- 2. Execute payout — VPA/phone/email are already populated in PayoutRequest at creation time
  payoutServiceName <- TP.decidePayoutService (DEMSC.RidePayoutService PT.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  let payoutCall = TP.createPayoutOrder merchantId merchantOpCityId payoutServiceName (Just person.id.getId)
  mbPayoutOrder <- PayoutRequest.executePayoutRequest payoutRequest payoutCall
  whenJust mbPayoutOrder $ \payoutOrder ->
    logInfo $ "Special Zone Payout request submitted for id: " <> show payoutRequest.id <> " | orderId: " <> payoutOrder.id.getId
  pure Complete
