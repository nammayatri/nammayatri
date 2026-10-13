{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Autopay only recharges the wallet (wallet-subscription cities). A recharge is a WALLET_TOPUP
--   driver_fee on the driver's existing mandate; the existing PDN and execution jobs charge it on
--   the next daily cycle, and 'processPayment' credits the wallet on success.
module SharedLogic.Finance.WalletAutopay
  ( ensureAutopayRecharge,
    findOpenAutopayRecharge,
  )
where

import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Mandate as DMandate
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as DV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance as Finance
import SharedLogic.DriverFee (getStartTimeAndEndTimeRange, roundToHalf)
import SharedLogic.Finance.WalletAccount (isWalletSubscriptionCity, tierAutopayTrigger)
import SharedLogic.Finance.WalletSubscription (resolveWalletOwner)
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Mandate as QMandate

-- | How long past its pay-by date a pending recharge is still considered in flight.
staleRechargeGrace :: NominalDiffTime
staleRechargeGrace = 24 * 3600

-- | The driver's pending Autopay recharge, if one is in flight.
findOpenAutopayRecharge ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  m (Maybe DDF.DriverFee)
findOpenAutopayRecharge driverId =
  listToMaybe <$> QDF.findAllFeeByTypeServiceStatusAndDriver DPlan.YATRI_SUBSCRIPTION (cast driverId) [DDF.WALLET_TOPUP] [DDF.PAYMENT_PENDING]

-- | Schedule one Autopay recharge when the driver's own wallet is at or below the trigger level
--   (the tier minimum, or 0 when the tier has none). At most one recharge is in flight per driver,
--   so calling this from every debit is idempotent. The amount is the configured recharge, raised to
--   close the gap to the trigger level, capped by the mandate's max amount (existing mandates are
--   reused as they are). Returns the pending recharge, new or existing.
ensureAutopayRecharge ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  DTC.TransporterConfig ->
  DP.Person ->
  Maybe DV.VehicleVariant ->
  HighPrecMoney ->
  m (Maybe DDF.DriverFee)
ensureAutopayRecharge transporterConfig person mbVariant balance = do
  let dwc = transporterConfig.driverWalletConfig
  case dwc.autopayRechargeAmount of
    Just rechargeAmount | isWalletSubscriptionCity dwc && rechargeAmount > 0 -> do
      tiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing
      let triggerLevel = fromMaybe 0 (mbVariant >>= tierAutopayTrigger tiers)
      (walletOwnerType, _) <- resolveWalletOwner person.id
      -- A driver's mandate only ever recharges the driver's own wallet.
      if balance > triggerLevel || walletOwnerType /= Finance.DRIVER
        then pure Nothing
        else do
          mbDriverPlan <- QDPlan.findByDriverIdWithServiceName person.id DPlan.YATRI_SUBSCRIPTION
          case (mbDriverPlan, mbDriverPlan >>= (.mandateId)) of
            (Just driverPlan, Just mandateId) | driverPlan.autoPayStatus == Just DI.ACTIVE -> do
              mbMandate <- QMandate.findById mandateId
              case mbMandate of
                Just mandate | mandate.status == DMandate.ACTIVE ->
                  Redis.withWaitAndLockRedis (autopayRechargeLockKey person.id) 10 1000 $ do
                    now <- getCurrentTime
                    mbOpen <- findOpenAutopayRecharge person.id
                    -- A recharge whose charge never ran (jobs down, notification lost) must not block every later
                    -- one. Past its pay-by date plus a day it is retired; if the gateway did charge it after
                    -- all, the paid order still credits the wallet (see creditWalletForPaidDriverFees).
                    mbLive <- case mbOpen of
                      Just open | now > addUTCTime staleRechargeGrace open.payBy -> do
                        logWarning $ "Retiring stale Autopay wallet recharge " <> open.id.getId <> " of driver " <> person.id.getId
                        QDF.updateStatus DDF.INACTIVE open.id now
                        QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [open.id] Nothing
                        pure Nothing
                      _ -> pure mbOpen
                    case mbLive of
                      Just open -> pure (Just open)
                      Nothing -> do
                        let amount = roundToHalf transporterConfig.currency $ min mandate.maxAmount (max rechargeAmount (triggerLevel - balance))
                        -- A mandate with a zero cap would otherwise leave a zero-amount fee pending forever,
                        -- which then blocks every later recharge as "already open".
                        if amount <= 0
                          then do
                            logWarning $ "Skipping Autopay wallet recharge for driver " <> person.id.getId <> ": mandate cap leaves nothing to charge"
                            pure Nothing
                          else do
                            fee <- buildRechargeFee driverPlan amount
                            QDF.create fee
                            QINV.create =<< buildAutopayInvoice fee
                            logInfo $ "Scheduled Autopay wallet recharge of " <> show amount <> " for driver " <> person.id.getId
                            pure (Just fee)
                _ -> pure Nothing
            _ -> pure Nothing
    _ -> pure Nothing
  where
    buildRechargeFee driverPlan amount = do
      feeId <- generateGUID
      now <- getCurrentTime
      (startTime, endTime) <- getStartTimeAndEndTimeRange person.merchantOperatingCityId person.id (Just transporterConfig)
      let payBy = addUTCTime (transporterConfig.driverAutoPayNotificationTime + transporterConfig.driverAutoPayExecutionTime) endTime
      pure
        DDF.DriverFee
          { id = feeId,
            merchantId = person.merchantId,
            merchantOperatingCityId = person.merchantOperatingCityId,
            driverId = cast person.id,
            serviceName = DPlan.YATRI_SUBSCRIPTION,
            feeType = DDF.WALLET_TOPUP,
            status = DDF.PAYMENT_PENDING,
            platformFee = DDF.PlatformFee {fee = amount, cgst = 0, sgst = 0, currency = transporterConfig.currency},
            govtCharges = 0,
            totalEarnings = 0,
            numRides = 0,
            startTime,
            endTime,
            payBy,
            autopayPaymentStage = Just DDF.NOTIFICATION_SCHEDULED,
            stageUpdatedAt = Just now,
            planId = Just driverPlan.planId,
            planMode = Just DPlan.AUTOPAY,
            vehicleCategory = fromMaybe DVC.AUTO_CATEGORY driverPlan.vehicleCategory,
            currency = transporterConfig.currency,
            collectedBy = Nothing,
            collectedAt = Nothing,
            collectedAtVendorId = Nothing,
            offerId = Nothing,
            planOfferTitle = Nothing,
            billNumber = Nothing,
            schedulerTryCount = 0,
            feeWithoutDiscount = Nothing,
            overlaySent = False,
            amountPaidByCoin = Nothing,
            notificationRetryCount = 0,
            badDebtDeclarationDate = Nothing,
            badDebtRecoveryDate = Nothing,
            vehicleNumber = Nothing,
            refundEntityId = Nothing,
            refundedAmount = Nothing,
            refundedAt = Nothing,
            refundedBy = Nothing,
            hasSibling = Just False,
            siblingFeeId = Nothing,
            splitOfDriverFeeId = Nothing,
            validDays = Nothing,
            cancellationPenaltyAmount = Nothing,
            addedToFeeId = Nothing,
            specialZoneRideCount = 0,
            specialZoneAmount = 0,
            specialZoneRideCharges = Nothing,
            createdAt = now,
            updatedAt = now
          }
    buildAutopayInvoice fee = do
      invoiceId <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      pure
        INV.Invoice
          { id = invoiceId,
            invoiceShortId = shortId.getShortId,
            driverFeeId = fee.id,
            invoiceStatus = INV.ACTIVE_INVOICE,
            paymentMode = INV.AUTOPAY_INVOICE,
            bankErrorCode = Nothing,
            bankErrorMessage = Nothing,
            bankErrorUpdatedAt = Nothing,
            maxMandateAmount = Nothing,
            driverId = person.id,
            lastStatusCheckedAt = Nothing,
            serviceName = fee.serviceName,
            merchantId = Just fee.merchantId,
            merchantOperatingCityId = fee.merchantOperatingCityId,
            updatedAt = now,
            createdAt = now
          }

autopayRechargeLockKey :: Id DP.Person -> Text
autopayRechargeLockKey driverId = "WalletAutopayRecharge:" <> driverId.getId
