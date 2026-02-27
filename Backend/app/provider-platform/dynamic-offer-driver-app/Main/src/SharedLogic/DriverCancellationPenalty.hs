{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverCancellationPenalty
  ( mkCancellationPenaltyFee,
    accumulateCancellationPenalty,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import EulerHS.Prelude
import Kernel.Prelude hiding (any, elem, map)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import qualified Lib.Finance.Domain.Types.Invoice as Invoice
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Finance.Wallet
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.Ride as QRide
import Tools.Constants
import Tools.Error
import Tools.Metrics as Metrics
import TransactionLogs.Types

mkCancellationPenaltyFee ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  UTCTime ->
  Id DMerc.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  HighPrecMoney ->
  Currency ->
  DTC.TransporterConfig ->
  m (UTCTime, DF.DriverFee)
mkCancellationPenaltyFee now merchantId merchantOpCityId driverId penaltyAmount currency transporterConfig = do
  id' <- generateGUID
  let cycleDuration = secondsToNominalDiffTime $ fromMaybe 0 transporterConfig.cancellationFeeCycle
      startTime = now
      endTime = addUTCTime cycleDuration startTime
      disputeWindowSeconds = secondsToNominalDiffTime $ fromMaybe 0 transporterConfig.cancellationFeeDisputeWindow
      disputeWindowEndTime = addUTCTime disputeWindowSeconds endTime
      payByWindow = secondsToNominalDiffTime (Seconds 604800) -- adding 7 days buffer for payby window .. dont know why i am doing this .. sad life
      payBy = addUTCTime (disputeWindowSeconds + payByWindow) endTime
  return $
    ( disputeWindowEndTime,
      DF.DriverFee
        { id = id',
          merchantId = merchantId,
          driverId = cast driverId,
          status = DF.ONGOING,
          feeType = DF.CANCELLATION_PENALTY,
          serviceName = DPlan.YATRI_SUBSCRIPTION,
          cancellationPenaltyAmount = Just penaltyAmount,
          platformFee = DF.PlatformFee {fee = 0, cgst = 0, sgst = 0, currency = currency},
          govtCharges = 0,
          totalEarnings = 0,
          numRides = 1,
          specialZoneAmount = 0,
          specialZoneRideCount = 0,
          startTime = startTime,
          endTime = endTime,
          payBy = payBy,
          createdAt = now,
          updatedAt = now,
          collectedBy = Nothing,
          collectedAt = Nothing,
          offerId = Nothing,
          planOfferTitle = Nothing,
          autopayPaymentStage = Nothing,
          stageUpdatedAt = Nothing,
          billNumber = Nothing,
          schedulerTryCount = 0,
          feeWithoutDiscount = Nothing,
          overlaySent = False,
          amountPaidByCoin = Nothing,
          planId = Nothing,
          planMode = Nothing,
          notificationRetryCount = 0,
          badDebtDeclarationDate = Nothing,
          badDebtRecoveryDate = Nothing,
          vehicleNumber = Nothing,
          merchantOperatingCityId = merchantOpCityId,
          refundEntityId = Nothing,
          refundedAmount = Nothing,
          refundedAt = Nothing,
          refundedBy = Nothing,
          vehicleCategory = DVC.AUTO_CATEGORY,
          hasSibling = Just False,
          siblingFeeId = Nothing,
          splitOfDriverFeeId = Nothing,
          validDays = Nothing,
          currency = currency,
          addedToFeeId = Nothing,
          collectedAtVendorId = Nothing
        }
    )

accumulateCancellationPenalty ::
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
  Bool -> -- isWalletEnabled
  SRB.Booking ->
  DRide.Ride ->
  [LYT.TagNameValue] ->
  DTC.TransporterConfig ->
  DP.Person ->
  m ()
accumulateCancellationPenalty isWalletEnabled booking ride rideTags transporterConfig driver = do
  when (validCancellationPenaltyApplicable `elem` rideTags && isJust booking.fareParams.driverCancellationPenaltyAmount) $ do
    case booking.fareParams.driverCancellationPenaltyAmount of
      Just penaltyAmount ->
        if isWalletEnabled
          then do
            -- Wallet path: settle via ledger directly, skip DriverFee
            let merchantId = booking.providerId
                merchantOpCityId = booking.merchantOperatingCityId
                mid = merchantId.getId
                mocid = merchantOpCityId.getId
                (driverOrFleetCounterparty, driverOrFleetId) =
                  case ride.fleetOwnerId of
                    Just fleetOwnerId -> (FLEET_OWNER, fleetOwnerId.getId)
                    Nothing -> (DRIVER, ride.driverId.getId)
            driverOrFleetLiability <-
              ( if driverOrFleetCounterparty == FLEET_OWNER
                  then getOrCreateFleetOwnerLiabilityAccount booking.currency driverOrFleetId mid mocid
                  else getOrCreateDriverLiabilityAccount booking.currency driverOrFleetId mid mocid
                )
                >>= fromEitherM (\err -> InternalError ("Driver/FleetOwner liability account not found: " <> show err))
            driverOrFleetExpense <-
              ( if driverOrFleetCounterparty == FLEET_OWNER
                  then getOrCreateFleetOwnerExpenseAccount booking.currency driverOrFleetId mid mocid
                  else getOrCreateDriverExpenseAccount booking.currency driverOrFleetId mid mocid
                )
                >>= fromEitherM (\err -> InternalError ("Driver/FleetOwner expense account not found: " <> show err))
            penaltyEntryId <-
              createLedgerTransfer driverOrFleetLiability driverOrFleetExpense penaltyAmount walletReferenceDriverCancellationCharges booking.id.getId
                >>= fromEitherM (\err -> InternalError ("Failed to create DriverCancellationCharges ledger entry: " <> show err))
            -- Create invoice for driver cancellation penalty
            let entryIds = catMaybes [penaltyEntryId]
            createWalletInvoice
              booking
              ride
              (Just driver)
              WalletInvoiceParams
                { invoiceType = Invoice.RideCancellation,
                  issuedToType = "DRIVER",
                  issuedToId = driverOrFleetId,
                  issuedToName = Nothing,
                  issuedToAddress = Nothing,
                  gstBreakdown = Nothing, -- no GST on driver cancellation penalty
                  lineItems =
                    [ InvoiceLineItem {description = "Driver Cancellation Penalty", quantity = 1, unitPrice = penaltyAmount, lineTotal = penaltyAmount, isExternalCharge = False}
                    ]
                }
              entryIds
            logInfo $
              "Created DriverCancellationCharges ledger entry for ₹"
                <> show penaltyAmount
                <> " bookingId: "
                <> booking.id.getId
            QRide.updateDriverCancellationPenalty Nothing (Just penaltyAmount) ride.id
          else do
            -- Legacy path: create/update DriverFee
            now <- getCurrentTime
            existingCancellationFee <-
              QDF.findOngoingCancellationPenaltyFeeByDriverIdAndServiceName
                (cast ride.driverId)
                DPlan.YATRI_SUBSCRIPTION
                booking.providerId
                booking.merchantOperatingCityId
                now
            (feeId, penAmt) <- case existingCancellationFee of
              Just existingFee -> do
                Redis.whenWithLockRedis (cancellationPenaltyLockKey existingFee.id.getId) 10 $ do
                  let currentAmount = fromMaybe 0 existingFee.cancellationPenaltyAmount
                      newAmount = currentAmount + penaltyAmount
                      newNumRides = existingFee.numRides + 1
                  QDF.updateCancellationPenaltyAmountAndNumRides existingFee.id newAmount newNumRides now
                return (existingFee.id, penaltyAmount)
              Nothing -> do
                (_, newFee) <-
                  mkCancellationPenaltyFee
                    now
                    booking.providerId
                    booking.merchantOperatingCityId
                    ride.driverId
                    penaltyAmount
                    booking.currency
                    transporterConfig
                QDF.create newFee
                logInfo $
                  "Created new CANCELLATION_PENALTY DriverFee " <> newFee.id.getId
                    <> " for ₹"
                    <> show penaltyAmount
                return (newFee.id, penaltyAmount)
            QRide.updateDriverCancellationPenalty (Just feeId.getId) (Just penAmt) ride.id
      Nothing ->
        logError $
          "Penalty tag present but driverCancellationPenaltyAmount is Nothing for ride "
            <> ride.id.getId

cancellationPenaltyLockKey :: Text -> Text
cancellationPenaltyLockKey id' = "Driver:Cancellation:Penalty:DriverFeeId-" <> id'
