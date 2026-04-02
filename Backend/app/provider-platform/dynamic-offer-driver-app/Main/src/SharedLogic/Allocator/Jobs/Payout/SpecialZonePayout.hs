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
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
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
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Ride (getRcIdForRide)
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverRidePayoutBankAccount as QDRPB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.ScheduledPayout as QSP
import qualified Storage.Queries.ScheduledPayoutExtra as QSPE
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
  let jobData = jobInfo.jobData
  case jobData.payoutRequestId of
    Just prId -> handleNewFlow prId
    Nothing -> case jobData.scheduledPayoutId of
      Just spId -> handleOldFlow spId
      Nothing -> do
        logError "SpecialZonePayout job has neither payoutRequestId nor scheduledPayoutId"
        pure Complete

-- ---------------------------------------------------------------------------
-- New flow: PayoutRequest based (current main)
-- ---------------------------------------------------------------------------

handleNewFlow ::
  ( EncFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    RideEnd.EndRideFlow m r,
    LocationUpdateFlow m r c
  ) =>
  Id DPR.PayoutRequest ->
  m ExecutionResult
handleNewFlow payoutRequestId = do
  let lockKey = "payout:lock:" <> payoutRequestId.getId
  acquired <- Redis.tryLockRedis lockKey 300
  if not acquired
    then do
      logWarning $ "Could not acquire lock for payout: " <> show payoutRequestId
      pure Complete
    else do
      flip finally (Redis.unlockRedis lockKey) $ do
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

  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbRide <- QRide.findById (Id payoutRequest.entityId)

  whenJust mbRide $ \ride ->
    when (ride.status == Ride.INPROGRESS) $ do
      let driverReq =
            RideEnd.DriverEndRideReq
              { endRideOtp = Nothing,
                point = LatLong {lat = 0.0, lon = 0.0},
                requestor = person,
                uiDistanceCalculationWithAccuracy = Nothing,
                uiDistanceCalculationWithoutAccuracy = Nothing,
                odometer = Nothing,
                driverGpsTurnedOff = Nothing
              }
      shandle <- RideEnd.buildEndRideHandle merchantId merchantOpCityId (Just ride.id)
      void $ RideEnd.driverEndRide shandle ride.id driverReq

  payoutServiceName <- TP.decidePayoutService (DEMSC.RidePayoutService PT.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  let payoutCall = TP.createPayoutOrder merchantId merchantOpCityId payoutServiceName (Just person.id.getId)
  mbPayoutOrder <- PayoutRequest.executePayoutRequest payoutRequest payoutCall
  whenJust mbPayoutOrder $ \payoutOrder ->
    logInfo $ "Special Zone Payout request submitted for id: " <> show payoutRequest.id <> " | orderId: " <> payoutOrder.id.getId
  pure Complete

-- ---------------------------------------------------------------------------
-- Old flow: ScheduledPayout based (backward compat with prodHotPush-BPP)
-- Will be removed after all old jobs are processed post-release.
-- ---------------------------------------------------------------------------

handleOldFlow ::
  ( EncFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    RideEnd.EndRideFlow m r,
    LocationUpdateFlow m r c
  ) =>
  Id DSP.ScheduledPayout ->
  m ExecutionResult
handleOldFlow scheduledPayoutId = do
  let lockKey = "payout:lock:" <> scheduledPayoutId.getId
  acquired <- Redis.tryLockRedis lockKey 300
  if not acquired
    then do
      logWarning $ "Could not acquire lock for payout: " <> show scheduledPayoutId
      pure Complete
    else do
      flip finally (Redis.unlockRedis lockKey) $ do
        mbScheduledPayout <- QSP.findById scheduledPayoutId
        case mbScheduledPayout of
          Nothing -> do
            logInfo $ "ScheduledPayout record not found for id: " <> show scheduledPayoutId
            pure Complete
          Just scheduledPayout -> do
            case scheduledPayout.status of
              DSP.FAILED -> do
                logInfo $ "Payout was cancelled/failed, skipping: " <> show scheduledPayoutId
                pure Complete
              DSP.CREDITED -> do
                logInfo $ "Payout already processed: " <> show scheduledPayoutId
                pure Complete
              DSP.CASH_PAID -> do
                logInfo $ "Payout already marked as cash paid: " <> show scheduledPayoutId
                pure Complete
              DSP.CASH_PENDING -> do
                logInfo $ "Payout already marked as cash pending: " <> show scheduledPayoutId
                pure Complete
              DSP.PROCESSING -> do
                logInfo $ "Payout already being processed: " <> show scheduledPayoutId
                pure Complete
              DSP.AUTO_PAY_FAILED -> do
                logInfo $ "Payout auto-pay failed, needs admin retry: " <> show scheduledPayoutId
                pure Complete
              DSP.RETRYING -> do
                logInfo $ "Payout is being retried: " <> show scheduledPayoutId
                pure Complete
              DSP.CANCELLED -> do
                logInfo $ "Payout was cancelled, skipping: " <> show scheduledPayoutId
                pure Complete
              DSP.INITIATED -> do
                executeOldSpecialZonePayout scheduledPayout

executeOldSpecialZonePayout ::
  ( EncFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r,
    RideEnd.EndRideFlow m r,
    LocationUpdateFlow m r c
  ) =>
  DSP.ScheduledPayout ->
  m ExecutionResult
executeOldSpecialZonePayout scheduledPayout = do
  let driverId = Id scheduledPayout.driverId
  let merchantId = scheduledPayout.merchantId
  let merchantOpCityId = scheduledPayout.merchantOperatingCityId

  -- 1. Mark as PROCESSING
  QSPE.updateStatusWithHistoryById DSP.PROCESSING (Just "Payment in progress") scheduledPayout

  -- 2. End ride if still in progress
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbRide <- QRide.findById (Id scheduledPayout.rideId)

  case (mbRide, merchantId, merchantOpCityId) of
    (Just ride, Just mId, Just mocId) -> do
      when (ride.status == Ride.INPROGRESS) $ do
        let driverReq =
              RideEnd.DriverEndRideReq
                { endRideOtp = Nothing,
                  point = LatLong {lat = 0.0, lon = 0.0},
                  requestor = person,
                  uiDistanceCalculationWithAccuracy = Nothing,
                  uiDistanceCalculationWithoutAccuracy = Nothing,
                  odometer = Nothing,
                  driverGpsTurnedOff = Nothing
                }
        shandle <- RideEnd.buildEndRideHandle mId mocId (Just ride.id)
        void $ RideEnd.driverEndRide shandle ride.id driverReq
    _ -> pure ()

  -- 3. Get payout VPA from ScheduledPayout.rcId -> DriverRidePayoutBankAccount
  mbRcId <-
    case scheduledPayout.rcId of
      Just t -> pure (Just t)
      Nothing -> do
        logWarning $ "No rcId for scheduled payout, falling back to ride details (ride: " <> scheduledPayout.rideId <> ")"
        getRcIdForRide (Id scheduledPayout.rideId)
  mbVpa <-
    case mbRcId of
      Nothing -> do
        logWarning $ "No rcId for payout: scheduled_payout.rc_id is empty and fallback returned nothing for ride: " <> scheduledPayout.rideId
        pure Nothing
      Just rcIdText -> do
        mbBankAccount <- QDRPB.findByRcId (Id rcIdText :: Id DVRC.VehicleRegistrationCertificate)
        case mbBankAccount of
          Nothing -> do
            logWarning $ "No driver ride payout bank account for rcId: " <> rcIdText
            pure Nothing
          Just rcAccount -> do
            accountNumber <- decrypt `mapM` rcAccount.bankAccountNumber
            ifscCode <- decrypt `mapM` rcAccount.bankIfscCode
            pure $ (\a i -> a <> "@" <> i <> ".ifsc.npci") <$> accountNumber <*> ifscCode

  case mbVpa of
    Nothing -> do
      logWarning $ "No payout bank account for ride: " <> scheduledPayout.rideId
      QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "No payout bank account for this ride") scheduledPayout
      pure Complete
    Just vpa -> do
      case merchantOpCityId of
        Nothing -> do
          logWarning $ "No merchant operating city for payout: " <> show scheduledPayout.id
          QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Missing merchant operating city") scheduledPayout
          pure Complete
        Just opCityId -> do
          let amount = fromMaybe 0 scheduledPayout.amount
          if amount <= 0
            then do
              logWarning $ "Invalid payout amount: " <> show amount
              QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Invalid payout amount") scheduledPayout
              pure Complete
            else do
              uid <- generateGUID
              phoneNo <- mapM decrypt person.mobileNumber
              merchantOperatingCity <- CQMOC.findById opCityId >>= fromMaybeM (MerchantOperatingCityNotFound opCityId.getId)
              let entityName = DLP.SPECIAL_ZONE_PAYOUT
                  createPayoutOrderReq = Payout.mkCreatePayoutOrderReq uid amount phoneNo person.email driverId.getId "Payout for Airport Ride" (Just person.firstName) vpa "FULFILL_ONLY" False
              case merchantId of
                Nothing -> do
                  logWarning $ "No merchant ID for payout: " <> show scheduledPayout.id
                  QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just "Missing merchant ID") scheduledPayout
                  pure Complete
                Just mId -> do
                  logInfo $ "Calling payout service for driver: " <> driverId.getId <> " | amount: " <> show amount <> " | orderId: " <> uid
                  payoutServiceName <- TP.decidePayoutService (DEMSC.RidePayoutService PT.Juspay) person.clientSdkVersion person.merchantOperatingCityId
                  let createPayoutOrderCall = TP.createPayoutOrder mId opCityId payoutServiceName (Just person.id.getId)

                  result <- try $ Payout.createPayoutService (cast mId) (Just $ cast opCityId) (cast driverId) (Just [scheduledPayout.id.getId]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall Nothing

                  case result of
                    Left (err :: SomeException) -> do
                      logError $ "Payout service call failed: " <> show err
                      QSPE.updateStatusWithHistoryById DSP.AUTO_PAY_FAILED (Just $ "Payout service error: " <> show err) scheduledPayout
                      pure Complete
                    Right (mbPayoutResp, mbPayoutOrder) -> do
                      let payoutOrderId = maybe "unknown" (\po -> po.id.getId) mbPayoutOrder
                          mbTransactionRef = do
                            resp <- mbPayoutResp
                            fulfillment <- listToMaybe =<< resp.fulfillments
                            txn <- listToMaybe =<< fulfillment.transactions
                            pure txn.transactionRef
                          transactionRef = fromMaybe payoutOrderId mbTransactionRef

                      QSPE.updatePayoutTransactionId (Just payoutOrderId) scheduledPayout.id
                      QSPE.updateStatusWithHistoryById DSP.PROCESSING (Just $ "Payout request sent to Bank. OrderId: " <> payoutOrderId <> ", TxnRef: " <> transactionRef) scheduledPayout
                      logInfo $ "Special Zone Payout request submitted for id: " <> show scheduledPayout.id <> " | orderId: " <> payoutOrderId
                      pure Complete
