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
    chargeDriverPenaltyFee,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverFee as DF
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..))
import qualified "beckn-spec" Domain.Types.Invoice as InvType
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import EulerHS.Prelude hiding (whenJust)
import Kernel.Prelude hiding (any, elem, map)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import qualified Lib.Finance.Core.Types as Finance
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Finance.Wallet
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.Ride as QRide
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
  ( EncFlow m r,
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
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasShortDurationRetryCfg r c,
    Finance.HasActorInfo m r
  ) =>
  Bool -> -- isWalletEnabled
  SRB.Booking ->
  DRide.Ride ->
  -- penalty amount from the CancellationConsequenceMatrix row's driverDeduction (MONEY
  -- variant); Nothing means no penalty. Replaces the CancellationPenaltyApplicable tag
  -- gate and farePolicy.driverCancellationPenaltyAmount.
  Maybe HighPrecMoney ->
  DTC.TransporterConfig ->
  DP.Person ->
  m ()
accumulateCancellationPenalty isWalletEnabled booking ride mbPenaltyAmount transporterConfig driver = do
  whenJust mbPenaltyAmount $ \signedAmount -> do
    -- Signed amount from the consequence ADAPTER (the matrix itself stores positive
    -- amounts with direction in the MoneyDeduction/MoneyAddition constructor;
    -- CancellationConsequence.driverMoneyDeduction emits + for a penalty, − for an
    -- addition). Positive = penalty (fee/wallet debit), negative = compensation, which
    -- rides the wallet only — the legacy DriverFee rail cannot pay out.
    when (signedAmount < 0) $
      if isWalletEnabled
        then do
          mbPanCard <- QPanCard.findByDriverId ride.driverId
          mbDriverInfo <- QDI.findById (cast ride.driverId)
          ctx <- buildFinanceCtx booking ride (Just driver) mbPanCard mbDriverInfo transporterConfig True
          creditResult <- runFinance ctx $ void $ transfer OwnerExpense OwnerLiability (abs signedAmount) walletReferenceDriverCancellationCharges Nothing
          case creditResult of
            Left err -> logError $ "Failed to credit driver cancellation compensation: " <> show err <> " bookingId: " <> booking.id.getId
            Right _ -> logInfo $ "Credited driver cancellation compensation ₹" <> show (abs signedAmount) <> " bookingId: " <> booking.id.getId
        else logError $ "Driver cancellation compensation (negative matrix amount) requires the wallet; skipped for ride " <> ride.id.getId
    when (signedAmount > 0) $ do
      let penaltyAmount = signedAmount
      if isWalletEnabled
        then do
          mbPanCard <- QPanCard.findByDriverId ride.driverId
          mbDriverInfo <- QDI.findById (cast ride.driverId)
          ctx <- buildFinanceCtx booking ride (Just driver) mbPanCard mbDriverInfo transporterConfig True
          result <- runFinance ctx $ do
            _ <- transfer OwnerLiability OwnerExpense penaltyAmount walletReferenceDriverCancellationCharges Nothing
            invoice
              InvoiceConfig
                { invoiceType = RideCancellation,
                  issuedToType = InvType.DRIVER,
                  issuedToId = maybe ride.driverId.getId (.getId) ride.fleetOwnerId,
                  issuedToName = Nothing,
                  issuedToAddress = Nothing,
                  referenceId = Just booking.id.getId,
                  gstBreakdown = Nothing,
                  lineItems =
                    [ InvoiceLineItem {description = "Driver Cancellation Penalty", descriptionType = Just DriverCancellationPenalty, quantity = 1, unitPrice = penaltyAmount, lineTotal = penaltyAmount, isExternalCharge = False, groupId = Just "g-penalty", itemType = Just Fare}
                    ],
                  isVat = False,
                  issuedToTaxNo = Nothing,
                  issuedByTaxNo = Nothing,
                  paymentMode = Nothing,
                  periodStart = Nothing,
                  periodEnd = Nothing
                }
          case result of
            Left err -> fromEitherM (\e -> InternalError ("Failed to create DriverCancellationCharges: " <> show e)) (Left err)
            Right _ -> pure ()
          logInfo $
            "Created DriverCancellationCharges ledger entry for ₹"
              <> show penaltyAmount
              <> " bookingId: "
              <> booking.id.getId
          QRide.updateDriverCancellationPenalty Nothing (Just penaltyAmount) ride.id
        else do
          -- Legacy path: create/update DriverFee
          feeId <- chargeDriverPenaltyFee booking.providerId booking.merchantOperatingCityId ride.driverId penaltyAmount booking.currency transporterConfig
          QRide.updateDriverCancellationPenalty (Just feeId.getId) (Just penaltyAmount) ride.id

-- | Create or top up the driver's ongoing CANCELLATION_PENALTY fee. Shared by the
-- per-ride cancellation penalty (accumulateCancellationPenalty) and the behavior
-- engine's CHARGE_FEE consequence (ConsequenceDispatcher) — both ride the same
-- DriverFee collection and dashboard waiver rails.
chargeDriverPenaltyFee ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMerc.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  HighPrecMoney ->
  Currency ->
  DTC.TransporterConfig ->
  m (Id DF.DriverFee)
chargeDriverPenaltyFee merchantId merchantOpCityId driverId penaltyAmount currency transporterConfig = do
  now <- getCurrentTime
  existingCancellationFee <-
    QDF.findOngoingCancellationPenaltyFeeByDriverIdAndServiceName
      (cast driverId)
      DPlan.YATRI_SUBSCRIPTION
      merchantId
      merchantOpCityId
      now
  case existingCancellationFee of
    Just existingFee -> do
      Redis.whenWithLockRedis (cancellationPenaltyLockKey existingFee.id.getId) 10 $ do
        let currentAmount = fromMaybe 0 existingFee.cancellationPenaltyAmount
            newAmount = currentAmount + penaltyAmount
            newNumRides = existingFee.numRides + 1
        QDF.updateCancellationPenaltyAmountAndNumRides existingFee.id newAmount newNumRides now
      return existingFee.id
    Nothing -> do
      (_, newFee) <-
        mkCancellationPenaltyFee
          now
          merchantId
          merchantOpCityId
          driverId
          penaltyAmount
          currency
          transporterConfig
      QDF.create newFee
      logInfo $
        "Created new CANCELLATION_PENALTY DriverFee " <> newFee.id.getId
          <> " for ₹"
          <> show penaltyAmount
      return newFee.id

cancellationPenaltyLockKey :: Text -> Text
cancellationPenaltyLockKey id' = "Driver:Cancellation:Penalty:DriverFeeId-" <> id'
