{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.PopulateTipAmount where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Data.Time
import Domain.Types.Ride
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance (AccountRole (..), FinanceCtx, runFinance, transfer)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Invoice.Interface as InvoiceI
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import qualified Lib.Finance.Ledger.Service as LedgerSvc
import qualified SharedLogic.Finance.InvoiceRegeneration as InvoiceRegen
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

populateTipAmount :: Id Ride -> HighPrecMoney -> Maybe Text -> Flow APISuccess
populateTipAmount rideId tipAmount apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantId = fromMaybe booking.providerId ride.merchantId
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

  QRide.updateTipAmountField (Just tipAmount) ride.id
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  mbDailyStats <- QDailyStats.findByDriverIdAndDate ride.driverId (utctDay localTime)
  case mbDailyStats of
    Just stats -> QDailyStats.updateTipAmountByDriverId (stats.tipAmount + tipAmount) ride.driverId (utctDay localTime)
    Nothing -> logError $ "DailyStats not found during updation of tip amount for driverId : " <> show ride.driverId

  -- Void prior invoice document (only); preserve original ledger entries.
  -- Append Tips ledger entry; mint a new invoice that re-links original entries + the new Tips entry.
  when (transporterConfig.driverWalletConfig.enableDriverWallet && tipAmount > 0) $ do
    mbPriorCtx <- InvoiceRegen.voidPriorRideInvoice booking.id.getId
    case mbPriorCtx of
      Nothing -> logInfo $ "Skipping invoice regeneration — no prior invoice found for booking " <> booking.id.getId
      Just priorCtx -> do
        mbDriver <- QPerson.findById ride.driverId
        mbDriverInfo <- QDI.findById (cast ride.driverId)
        case mbDriverInfo of
          Nothing -> logError $ "Cannot regenerate invoice for tip: missing driverInfo for ride " <> ride.id.getId
          Just _driverInfo -> do
            mbPanCard <- pure Nothing -- panCard lookup not strictly required for invoice regeneration
            -- Tip payment is always platform-mediated via the rider's card → isOnline=True.
            ctx <- Wallet.buildFinanceCtx booking ride mbDriver mbPanCard mbDriverInfo transporterConfig True
            -- Step 1a: reverse any prior Tips entries (handles multi-tip-update edge case)
            let priorTipsEntries = List.filter (\e -> e.referenceType == Wallet.walletReferenceTips) priorCtx.priorEntries
            forM_ priorTipsEntries $ \e -> do
              rRes <- LedgerSvc.createReversal e.id "tip-update: superseded by new tip amount"
              case rRes of
                Left err -> logError $ "Failed to reverse prior Tips entry " <> e.id.getId <> ": " <> show err
                Right _ -> pure ()
            -- Step 1b: append the new Tips ledger entry (BuyerAsset -> OwnerLiability)
            tipsResult <- runFinance ctx $ do
              void $ transfer BuyerAsset OwnerLiability tipAmount Wallet.walletReferenceTips
            case tipsResult of
              Left err -> logError $ "Failed to create Tips ledger entry: " <> show err
              Right (_mbInvId, newEntryIds) -> do
                -- Step 2: build a new InvoiceInput cloned from the prior invoice + tip line item
                let priorInv = priorCtx.priorInvoice
                    priorLineItems = parseLineItems priorInv.lineItems
                    tipLineItem =
                      InvoiceI.InvoiceLineItem
                        { description = "Tip",
                          quantity = 1,
                          unitPrice = tipAmount,
                          lineTotal = tipAmount,
                          isExternalCharge = False
                        }
                    -- Avoid duplicate Tip rows if this is run multiple times
                    nonTipLineItems = List.filter (\li -> li.description /= "Tip") priorLineItems
                    newLineItems = nonTipLineItems <> [tipLineItem]
                    newInvoiceInput = invoiceToInput priorInv ctx newLineItems
                    -- Exclude any prior Tips entries — the new Tips entry supersedes them.
                    -- (Handles the case where a pre-ride Tip entry was created at ride end.)
                    priorEntryIds = map (.id) $ List.filter (\e -> e.referenceType /= Wallet.walletReferenceTips) priorCtx.priorEntries
                createRes <- InvoiceSvc.createInvoice newInvoiceInput (priorEntryIds <> newEntryIds)
                case createRes of
                  Left err -> logError $ "Failed to mint regenerated invoice with tip: " <> show err
                  Right newInv ->
                    logInfo $ "Regenerated invoice " <> newInv.id.getId <> " (replacing voided " <> priorInv.id.getId <> ") with tip " <> show tipAmount

  pure Success
  where
    parseLineItems :: Aeson.Value -> [InvoiceI.InvoiceLineItem]
    parseLineItems v = case Aeson.fromJSON v of
      Aeson.Success xs -> xs
      Aeson.Error _ -> []

    invoiceToInput :: FInvoice.Invoice -> FinanceCtx -> [InvoiceI.InvoiceLineItem] -> InvoiceI.InvoiceInput
    invoiceToInput inv ctx lineItems =
      InvoiceI.InvoiceInput
        { invoiceType = inv.invoiceType,
          paymentOrderId = inv.paymentOrderId,
          issuedToType = inv.issuedToType,
          issuedToId = inv.issuedToId,
          issuedToName = inv.issuedToName,
          issuedToAddress = inv.issuedToAddress,
          issuedByType = inv.issuedByType,
          issuedById = inv.issuedById,
          issuedByName = inv.issuedByName,
          issuedByAddress = inv.issuedByAddress,
          supplierName = inv.supplierName,
          supplierAddress = inv.supplierAddress,
          supplierGSTIN = inv.supplierGSTIN,
          supplierTaxNo = inv.supplierTaxNo,
          supplierId = inv.supplierId,
          -- Tax/compliance fields (not on Invoice domain type — pulled from FinanceCtx)
          gstinOfParty = Nothing,
          panOfParty = Nothing,
          panType = Nothing,
          counterpartyId = ctx.counterpartyId,
          tdsRateReason = Nothing,
          tanOfDeductee = Nothing,
          lineItems = lineItems,
          gstBreakdown = Nothing, -- regeneration: tax already booked at original ride end
          currency = inv.currency,
          dueAt = inv.dueAt,
          merchantId = inv.merchantId,
          merchantOperatingCityId = inv.merchantOperatingCityId,
          merchantShortId = fromMaybe ctx.merchantId ctx.merchantShortId,
          isVat = False, -- regeneration path: tax records unchanged, no need to re-create indirect_tax_transaction
          issuedToTaxNo = Nothing,
          issuedByTaxNo = Nothing
        }
