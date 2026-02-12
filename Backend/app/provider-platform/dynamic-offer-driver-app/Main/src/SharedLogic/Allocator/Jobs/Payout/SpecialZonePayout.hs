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
import Kernel.External.Encryption (decrypt)
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
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DLP
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Lib.Scheduler
import SharedLogic.Allocator
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as QDI
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
        mbPayoutRequest <- QPR.findById payoutRequestId

        case mbPayoutRequest of
          Nothing -> do
            logInfo $ "PayoutRequest record not found for id: " <> show payoutRequestId
            pure Complete
          Just payoutRequest -> do
            -- 3. Check status (idempotency + cancellation check)
            case payoutRequest.status of
              DPR.FAILED -> do
                logInfo $ "Payout was cancelled/failed, skipping: " <> show payoutRequestId
                pure Complete
              DPR.CREDITED -> do
                logInfo $ "Payout already processed: " <> show payoutRequestId
                pure Complete
              DPR.CASH_PAID -> do
                logInfo $ "Payout already marked as cash paid: " <> show payoutRequestId
                pure Complete
              DPR.CASH_PENDING -> do
                logInfo $ "Payout already marked as cash pending: " <> show payoutRequestId
                pure Complete
              DPR.PROCESSING -> do
                logInfo $ "Payout already being processed: " <> show payoutRequestId
                pure Complete
              DPR.AUTO_PAY_FAILED -> do
                logInfo $ "Payout auto-pay failed, needs admin retry: " <> show payoutRequestId
                pure Complete
              DPR.RETRYING -> do
                logInfo $ "Payout is being retried: " <> show payoutRequestId
                pure Complete
              DPR.CANCELLED -> do
                logInfo $ "Payout was cancelled, skipping: " <> show payoutRequestId
                pure Complete
              DPR.INITIATED -> do
                -- 4. Execute payout logic
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
  let merchantId = Id <$> payoutRequest.merchantId
  let merchantOpCityId = Id <$> payoutRequest.merchantOperatingCityId

  -- 1. Mark as PROCESSING
  PayoutRequest.updateStatusWithHistoryById DPR.PROCESSING (Just "Payment in progress") payoutRequest

  -- 2. Fetch driver info
  driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbRide <- QRide.findById (Id payoutRequest.entityId)

  case (mbRide, merchantId, merchantOpCityId) of
    (Just ride, Just mId, Just mocId) -> do
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
        shandle <- RideEnd.buildEndRideHandle mId mocId (Just ride.id)
        void $ RideEnd.driverEndRide shandle ride.id driverReq
    _ -> pure ()

  -- 3. Check if driver has VPA for payout
  case driverInfo.payoutVpa of
    Nothing -> do
      -- No VPA configured, mark as failed
      logWarning $ "Driver " <> driverId.getId <> " has no payout VPA configured"
      PayoutRequest.updateStatusWithHistoryById DPR.AUTO_PAY_FAILED (Just "Driver has no payout VPA configured") payoutRequest
      pure Complete
    Just vpa -> do
      case merchantOpCityId of
        Nothing -> do
          logWarning $ "No merchant operating city for payout: " <> show payoutRequest.id
          PayoutRequest.updateStatusWithHistoryById DPR.AUTO_PAY_FAILED (Just "Missing merchant operating city") payoutRequest
          pure Complete
        Just opCityId -> do
          -- 6. Get amount and create payout order
          let amount = fromMaybe 0 payoutRequest.amount
          if amount <= 0
            then do
              logWarning $ "Invalid payout amount: " <> show amount
              PayoutRequest.updateStatusWithHistoryById DPR.AUTO_PAY_FAILED (Just "Invalid payout amount") payoutRequest
              pure Complete
            else do
              -- 7. Create payout order
              uid <- generateGUID
              phoneNo <- mapM decrypt person.mobileNumber
              merchantOperatingCity <- CQMOC.findById opCityId >>= fromMaybeM (MerchantOperatingCityNotFound opCityId.getId)
              let entityName = DLP.SPECIAL_ZONE_PAYOUT
                  createPayoutOrderReq = Payout.mkCreatePayoutOrderReq uid amount phoneNo person.email driverId.getId "Payout for Airport Ride" (Just person.firstName) vpa "FULFILL_ONLY" False
              case merchantId of
                Nothing -> do
                  logWarning $ "No merchant ID for payout: " <> show payoutRequest.id
                  PayoutRequest.updateStatusWithHistoryById DPR.AUTO_PAY_FAILED (Just "Missing merchant ID") payoutRequest
                  pure Complete
                Just mId -> do
                  -- 8. Call payout service
                  logInfo $ "Calling payout service for driver: " <> driverId.getId <> " | amount: " <> show amount <> " | orderId: " <> uid
                  payoutServiceName <- TP.decidePayoutService (DEMSC.RidePayoutService PT.Juspay) person.clientSdkVersion person.merchantOperatingCityId
                  let createPayoutOrderCall = TP.createPayoutOrder mId opCityId payoutServiceName (Just person.id.getId)

                  result <- try $ Payout.createPayoutService (cast mId) (Just $ cast opCityId) (cast driverId) (Just [payoutRequest.id.getId]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall

                  case result of
                    Left (err :: SomeException) -> do
                      -- 9. Handle failure
                      logError $ "Payout service call failed: " <> show err
                      PayoutRequest.updateStatusWithHistoryById DPR.AUTO_PAY_FAILED (Just $ "Payout service error: " <> show err) payoutRequest
                      pure Complete
                    Right (mbPayoutResp, mbPayoutOrder) -> do
                      let payoutOrderId = maybe "unknown" (\po -> po.id.getId) mbPayoutOrder
                          -- Extract transactionRef from response if available
                          mbTransactionRef = do
                            resp <- mbPayoutResp
                            fulfillment <- listToMaybe =<< resp.fulfillments
                            txn <- listToMaybe =<< fulfillment.transactions
                            pure txn.transactionRef
                          transactionRef = fromMaybe payoutOrderId mbTransactionRef

                      -- Update scheduled payout with payout order ID -- we can do this when webhook arrive uske liye lookup
                      QPR.updatePayoutTransactionIdById (Just payoutOrderId) payoutRequest.id

                      -- Keep as PROCESSING - actual status will be updated by Juspay webhook
                      PayoutRequest.updateStatusWithHistoryById DPR.PROCESSING (Just $ "Payout request sent to Bank. OrderId: " <> payoutOrderId <> ", TxnRef: " <> transactionRef) payoutRequest
                      logInfo $ "Special Zone Payout request submitted for id: " <> show payoutRequest.id <> " | orderId: " <> payoutOrderId
                      pure Complete
