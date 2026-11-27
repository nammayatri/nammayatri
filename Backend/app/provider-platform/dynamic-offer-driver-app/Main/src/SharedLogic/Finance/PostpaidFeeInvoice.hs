{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Finance.PostpaidFeeInvoice
  ( PostpaidFeePaymentMode (..),
    createPostpaidFeeInvoice,
  )
where

import Control.Applicative ((<|>))
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverGstin as Fields
import qualified Domain.Types.DriverPanCard as Fields
import qualified Domain.Types.FleetOwnerInformation as Fields
import "beckn-spec" Domain.Types.Invoice (InvoiceType (SubscriptionFee), IssuedToType (..))
import qualified Domain.Types.Merchant as Fields
import qualified Domain.Types.MerchantOperatingCity as Fields
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.External.Encryption (EncFlow, decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (HasRequestId, addUTCTime, fork, fromMaybeM, secondsToNominalDiffTime, withTryCatch)
import Kernel.Utils.Logging (logError, logInfo, logWarning)
import Lib.Finance
  ( IndirectTaxInput (..),
    InvoiceInput (..),
    InvoiceLineItem (..),
    InvoiceNumberLayout (..),
    ItemType (..),
    LineItemDescription (..),
    createIndirectTaxEntry,
    createInvoiceWithLayout,
    updateInvoiceStatus,
  )
import qualified Lib.Finance.Core.Types as FinanceCore
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import SharedLogic.DriverFee (roundToHalf)
import qualified SharedLogic.Finance.EInvoice as EInvoice
import SharedLogic.Finance.GstBreakdown (computeGstBreakdownForPerson, computeGstBreakdownGSTIN)
import SharedLogic.Finance.RideInvoice (resolveInvoiceIssuedToAddress)
import SharedLogic.Finance.WalletAccount (computeTdsRateReason)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverGstin as QDG
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import Tools.Error

data PostpaidFeePaymentMode = PostpaidOnline | PostpaidCash | PostpaidWallet
  deriving (Eq, Show)

paymentModeText :: PostpaidFeePaymentMode -> Text
paymentModeText = \case
  PostpaidOnline -> "ONLINE"
  PostpaidCash -> "CASH"
  PostpaidWallet -> "WALLET"

-- | Create (at most once per @referenceId@) the SubscriptionFee invoice for fees paid together.
createPostpaidFeeInvoice ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    Metrics.CoreMetrics m,
    HasRequestId r,
    FinanceCore.HasActorInfo m r
  ) =>
  DTC.TransporterConfig ->
  DP.Person -> -- fee owner (driver / fleet owner)
  Text -> -- idempotency key, stored as invoice.referenceId
  Maybe Text -> -- payment-order id when paid through the gateway (invoice.entityReferenceId)
  PostpaidFeePaymentMode ->
  [DDF.DriverFee] ->
  m ()
createPostpaidFeeInvoice transporterConfig person referenceId mbPaymentOrderId mode fees = do
  let enabled = fromMaybe False (transporterConfig.invoiceConfig >>= (.enablePostpaidFeeInvoice))
      invoiceableFees = filter isInvoiceable fees
  whenJust (listToMaybe invoiceableFees) $ \firstFee -> when (enabled && sum (map feeTotal invoiceableFees) > 0) $ do
    res <-
      withTryCatch "createPostpaidFeeInvoice" $
        Redis.withWaitAndLockRedis (lockKey referenceId) 30 5000 $ do
          existing <- QFinanceInvoiceExtra.findByReferenceIdWithOptions referenceId (Just SubscriptionFee) Nothing [] (Just 1) (Just 0)
          if null existing
            then createInvoice firstFee.currency invoiceableFees
            else logInfo $ "Postpaid fee invoice already exists for reference " <> referenceId
    case res of
      Left err -> logError $ "Postpaid fee invoice failed for reference " <> referenceId <> ": " <> show err
      Right () -> pure ()
  where
    lockKey ref = "PostpaidFeeInvoice:Lock:" <> ref

    isInvoiceable fee =
      fee.serviceName == DPlan.YATRI_SUBSCRIPTION
        && fee.feeType `elem` [DDF.RECURRING_INVOICE, DDF.RECURRING_EXECUTION_INVOICE, DDF.CANCELLATION_PENALTY, DDF.MANDATE_REGISTRATION]
        && fee.status `notElem` [DDF.EXEMPTED, DDF.CLEARED_BY_YATRI_COINS]
        && not (paidElsewhere fee)

    paidElsewhere fee = mode == PostpaidOnline && isJust fee.collectedBy

    feeBase fee = fee.platformFee.fee
    feePenalty fee = fromMaybe 0 fee.cancellationPenaltyAmount
    penaltyBase fee = roundTo2 (feePenalty fee / 1.18)
    penaltyGst fee = feePenalty fee - penaltyBase fee
    roundTo2 :: HighPrecMoney -> HighPrecMoney
    roundTo2 x = fromIntegral (round (x * 100) :: Integer) / 100
    money2Text :: HighPrecMoney -> Text
    money2Text x =
      let paiseTotal = round (abs x * 100) :: Integer
          (rupees, paise) = paiseTotal `divMod` 100
       in show rupees <> "." <> (if paise < 10 then "0" else "") <> show paise
    feeGst fee = fee.platformFee.cgst + fee.platformFee.sgst
    feeGovtCharges fee = fee.govtCharges
    feeTotal fee = feeBase fee + feePenalty fee + feeGst fee + feeGovtCharges fee
    feeRoundOff fee = if mode == PostpaidOnline then roundToHalf fee.currency (feeTotal fee) - feeTotal fee else 0

    localDate :: UTCTime -> Text
    localDate t = T.pack $ formatTime defaultTimeLocale "%d %b %Y" (addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) t)

    feeLabel fee
      | fee.feeType == DDF.MANDATE_REGISTRATION = "Autopay mandate registration " <> localDate fee.startTime <> (if feeCoins fee > 0 then " (" <> T.drop 3 (coinsNote fee) <> ")" else "")
      | isJust fee.splitOfDriverFeeId =
        "Subscription fee "
          <> localDate fee.startTime
          <> " – split for Autopay limit"
          <> maybe "" (\v -> " (" <> v <> ")") fee.vehicleNumber
      | otherwise =
        "Subscription fee "
          <> localDate fee.startTime
          <> " ("
          <> show fee.numRides
          <> " rides"
          <> maybe "" (" · " <>) fee.vehicleNumber
          <> coinsNote fee
          <> ")"

    mkLine description descriptionType itemType groupId amount =
      InvoiceLineItem
        { description = description,
          descriptionType = Just descriptionType,
          quantity = 1,
          unitPrice = amount,
          lineTotal = amount,
          isExternalCharge = False,
          groupId = Just groupId,
          itemType = Just itemType
        }

    feeCoins fee = if isNothing fee.splitOfDriverFeeId then max 0 (fromMaybe 0 fee.amountPaidByCoin) else 0
    coinsNote fee = if feeCoins fee > 0 then " · ₹" <> money2Text (feeCoins fee) <> " Yatri coins applied" else ""

    feeLineItems :: (Int, DDF.DriverFee) -> [InvoiceLineItem]
    feeLineItems (idx, fee) =
      let groupId = "g-fee-" <> show idx
       in catMaybes
            [ positive (feeBase fee) $ mkLine (feeLabel fee) SubscriptionPlanFee Fare groupId,
              positive (penaltyBase fee) $ mkLine ("Cancellation penalty " <> localDate fee.startTime) DriverCancellationPenalty Fare (groupId <> "-penalty"),
              positive (penaltyGst fee) $ mkLine "GST" Gst Tax (groupId <> "-penalty"),
              positive (feeGst fee) $ mkLine "GST" Gst Tax groupId,
              positive (feeGovtCharges fee) $ \amt ->
                InvoiceLineItem
                  { description = "Government charges",
                    descriptionType = Just GovernmentCharges,
                    quantity = 1,
                    unitPrice = amt,
                    lineTotal = amt,
                    isExternalCharge = True,
                    groupId = Just (groupId <> "-govt"),
                    itemType = Just Fare
                  },
              nonZero (feeRoundOff fee) $ \amt ->
                InvoiceLineItem
                  { description = "Round off",
                    descriptionType = Just RoundOff,
                    quantity = 1,
                    unitPrice = amt,
                    lineTotal = amt,
                    isExternalCharge = True,
                    groupId = Just (groupId <> "-roundoff"),
                    itemType = Just Fare
                  }
            ]

    positive :: HighPrecMoney -> (HighPrecMoney -> a) -> Maybe a
    positive amount f = if amount > 0 then Just (f amount) else Nothing

    nonZero :: HighPrecMoney -> (HighPrecMoney -> a) -> Maybe a
    nonZero amount f = if amount /= 0 then Just (f amount) else Nothing

    createInvoice currency invoiceableFees = do
      let merchantId = person.merchantId
          isFleetOwner = person.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]
          totalGst = sum (map (\f -> feeGst f + penaltyGst f) invoiceableFees)
          totalPenalty = sum (map feePenalty invoiceableFees)
          totalGovtCharges = sum (map feeGovtCharges invoiceableFees)
          totalRoundOff = sum (map feeRoundOff invoiceableFees)
          totalExternal = totalGovtCharges + totalRoundOff
          taxableValue = sum (map (\f -> feeBase f + penaltyBase f) invoiceableFees)
      merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist person.merchantOperatingCityId.getId)
      mbFleetInfo <- if isFleetOwner then QFOI.findByPrimaryKey person.id else pure Nothing
      mbBuyerGstinRow <- QDG.findByDriverId person.id
      fleetGstin <- maybe (pure Nothing) (mapM decrypt . (.gstNumber)) mbFleetInfo
      buyerGstin <- traverse (decrypt . (.gstin)) mbBuyerGstinRow
      let gstinOfParty = fleetGstin <|> buyerGstin
      mbPanCard <- QPanCard.findByDriverId person.id
      panDecrypted <- traverse (decrypt . (.panCardNumber)) mbPanCard
      let sellerGstin = merchantOperatingCity.gstin <|> merchant.gstin
          issuedByAddress = Just $ show merchant.city <> ", " <> show merchant.state <> ", " <> show merchant.country
          opCityAddress = show merchantOperatingCity.city <> ", " <> show merchantOperatingCity.state <> ", " <> show merchantOperatingCity.country
          issuedToAddress = Just $ resolveInvoiceIssuedToAddress mbBuyerGstinRow mbFleetInfo opCityAddress
          issuedToName = Just $ fromMaybe person.firstName (mbFleetInfo >>= (.fleetName))
          subscriptionGst = transporterConfig.taxConfig.subscriptionGst
      gstBreakdown <-
        if totalGst <= 0
          then pure Nothing
          else
            if person.role == DP.FLEET_BUSINESS && isJust sellerGstin && isJust buyerGstin
              then pure $ computeGstBreakdownGSTIN subscriptionGst sellerGstin buyerGstin totalGst
              else computeGstBreakdownForPerson subscriptionGst merchantOperatingCity person.id isFleetOwner mbFleetInfo Nothing totalGst
      eInvoiceEnabled <- EInvoice.isEInvoiceEnabledForCity person.merchantOperatingCityId.getId
      let invoiceInput =
            InvoiceInput
              { invoiceType = SubscriptionFee,
                entityReferenceId = mbPaymentOrderId,
                referenceInvoiceNumber = Nothing,
                issuedToType = if isFleetOwner then FLEET_OWNER else DRIVER,
                issuedToId = person.id.getId,
                issuedToName = issuedToName,
                issuedToAddress = issuedToAddress,
                issuedByType = "SELLER",
                issuedById = merchantId.getId,
                issuedByName = Just merchant.name,
                issuedByAddress = issuedByAddress,
                supplierName = Nothing,
                supplierAddress = Nothing,
                supplierGSTIN = sellerGstin,
                supplierTaxNo = Nothing,
                supplierId = Nothing,
                merchantGstin = sellerGstin,
                referenceId = Just referenceId,
                gstinOfParty = gstinOfParty,
                panOfParty = panDecrypted,
                panType = mbPanCard >>= (fmap show . (.docType)),
                counterpartyId = person.id.getId,
                tdsRateReason = computeTdsRateReason mbPanCard False,
                tanOfDeductee = Nothing,
                lineItems = concatMap feeLineItems (zip [1 ..] (sortOn (\f -> (f.startTime, isNothing f.splitOfDriverFeeId)) invoiceableFees)),
                gstBreakdown = gstBreakdown,
                currency = currency,
                dueAt = Nothing,
                merchantId = merchantId.getId,
                merchantOperatingCityId = person.merchantOperatingCityId.getId,
                merchantShortId = getShortId merchant.shortId,
                isVat = False,
                issuedToTaxNo = gstinOfParty,
                issuedByTaxNo = sellerGstin,
                paymentMode = Just (paymentModeText mode),
                periodStart = Just (minimum (map (.startTime) invoiceableFees)),
                periodEnd = Just (maximum (map (.endTime) invoiceableFees))
              }
      invoiceResult <- createInvoiceWithLayout (if eInvoiceEnabled then PurposeDateSeq else DatePurposeSeq) invoiceInput []
      case invoiceResult of
        Left err -> logError $ "Postpaid fee invoice creation failed for reference " <> referenceId <> ": " <> show err
        Right inv -> do
          -- Fees are already paid when we get here.
          updateInvoiceStatus inv.id FInvoice.Paid
            `catch` (\(e :: SomeException) -> logError $ "Failed to mark postpaid fee invoice Paid: " <> show e)
          when (totalGst > 0) $
            void
              ( createIndirectTaxEntry
                  IndirectTaxInput
                    { transactionType = IndirectTax.Subscription,
                      referenceId = referenceId,
                      taxableValue = taxableValue,
                      totalTaxAmount = totalGst,
                      gstBreakdown = gstBreakdown,
                      taxCreditType = IndirectTax.Output,
                      counterpartyId = person.id.getId,
                      gstinOfParty = gstinOfParty,
                      sacCode = Just "998314", -- same SAC as prepaid subscription (sacCodeForTransactionType Subscription)
                      -- Government charges + round off, matching the invoice's external lines.
                      externalCharges = if totalExternal /= 0 then Just totalExternal else Nothing,
                      invoiceNumber = Just inv.invoiceNumber,
                      merchantId = merchantId.getId,
                      merchantOperatingCityId = person.merchantOperatingCityId.getId,
                      isVat = False,
                      issuedToTaxNo = gstinOfParty,
                      issuedByTaxNo = sellerGstin
                    }
              )
              `catch` (\(e :: SomeException) -> logError $ "Failed to create GST entry for postpaid fee invoice " <> inv.invoiceNumber <> ": " <> show e)
          logInfo $ "Created postpaid fee invoice " <> inv.invoiceNumber <> " for reference " <> referenceId
          if totalPenalty + totalGovtCharges > 0 || totalRoundOff /= 0
            then logWarning $ "GSTEInvoice: skipped for postpaid fee invoice " <> inv.invoiceNumber <> " (has penalty/government charges/round off, not representable in the IRP payload)"
            else
              fork "generate B2B e-invoice (postpaid fee)" $
                EInvoice.generateEInvoiceForInvoice inv
                  `catch` (\(e :: SomeException) -> logError $ "GSTEInvoice: background fork failed: " <> show e)
