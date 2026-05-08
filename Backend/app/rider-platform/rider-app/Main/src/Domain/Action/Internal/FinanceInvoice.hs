module Domain.Action.Internal.FinanceInvoice
  ( getFinanceInvoicePdfByBppBookingId,
    getFinanceInvoiceListBySupplier,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import Domain.Types.Booking (BPPBooking)
import Domain.Types.Invoice (InvoiceType)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (last, listToMaybe, showBaseUrl)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QInvoiceLedgerLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedgerEntry
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Booking as QBooking
import Tools.Error
import "beckn-services" Tools.InvoicePdf (generateFinanceInvoicePdf)

getFinanceInvoicePdfByBppBookingId ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe DateOrTime ->
  Maybe DateOrTime ->
  Maybe InvoiceType ->
  Flow API.FinanceInvoicePdfResp
getFinanceInvoicePdfByBppBookingId mbToken mbBppBookingId mbInvoiceId mbFrom mbTo mbInvoiceType = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (mbToken == Just internalAPIKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

  case mbInvoiceId of
    Just invoiceId -> fetchByInvoiceId invoiceId
    Nothing -> fetchByBppBookingId
  where
    fetchByInvoiceId invoiceId = do
      inv <- QInvoice.findById (Id invoiceId) >>= fromMaybeM (InvalidRequest $ "Invoice not found: " <> invoiceId)
      merchantOpCity <-
        CQMOC.findById (Id inv.merchantOperatingCityId)
          >>= fromMaybeM (MerchantOperatingCityNotFound inv.merchantOperatingCityId)
      mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = inv.merchantOperatingCityId})
      renderPdf merchantOpCity mbRiderConfig [inv]

    fetchByBppBookingId = do
      bppBookingId <- mbBppBookingId & fromMaybeM (InvalidRequest "bppBookingId or invoiceId is required")
      booking <- QBooking.findByBPPBookingId (Id bppBookingId :: Id BPPBooking) >>= fromMaybeM (BookingDoesNotExist bppBookingId)
      merchantOpCity <-
        CQMOC.findById booking.merchantOperatingCityId
          >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
      mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId})
      let fromTime = toUTCTimeFrom <$> mbFrom
          toTime = toUTCTimeTo <$> mbTo
      invoices <-
        QInvoiceExtra.findByMerchantOpCityIdAndDateRange
          booking.merchantOperatingCityId.getId
          fromTime
          toTime
          mbInvoiceType
          Nothing
          (Just booking.riderId.getId)
          Nothing
          (Just 10)
          (Just 0)
      renderPdf merchantOpCity mbRiderConfig invoices

    renderPdf merchantOpCity mbRiderConfig invoices = do
      when (null invoices) $
        throwError $ InvalidRequest "No invoices found for the given criteria"

      let locale = countryToLocale merchantOpCity.country
          tz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRiderConfig
          mbInvCfg = mbRiderConfig >>= (.invoiceConfig)
          cfg =
            InvoicePdfConfig
              { locale,
                timezone = tz,
                logoUrl = mbInvCfg >>= (.logoUrl) <&> showBaseUrl,
                cfgSupplierName = mbInvCfg >>= (.supplierName),
                cfgSupplierAddress = mbInvCfg >>= (.supplierAddress),
                cfgSupplierVatNumber = mbInvCfg >>= (.supplierVatNumber)
              }

      pdfDatas <- forM invoices $ \inv -> do
        let items = parseLineItems inv.lineItems
        taxTxns <- QIndirectTaxExtra.findByInvoiceNumber inv.invoiceNumber
        let mbTaxTxn = listToMaybe taxTxns
        (mbPayType, mbBrand, mbLast4) <- case inv.paymentOrderId of
          Just orderId -> do
            txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
            let mbTxn = listToMaybe txns
            pure (mbTxn >>= (.paymentMethodType), mbTxn >>= (.cardBrand), mbTxn >>= (.cardLastFourDigits))
          Nothing -> pure (Nothing, Nothing, Nothing)
        pure $ buildInvoicePdfData inv items mbTaxTxn mbPayType mbBrand mbLast4

      let lastInv = last invoices
          html = case pdfDatas of
            [single] -> renderInvoiceHtml cfg single
            batch -> renderBatchInvoiceHtml cfg batch

      pdfBase64 <- generateFinanceInvoicePdf lastInv.invoiceNumber html

      pure $
        API.FinanceInvoicePdfResp
          { pdfBase64 = pdfBase64,
            invoiceNumber = lastInv.invoiceNumber
          }

getFinanceInvoiceListBySupplier ::
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Flow API.FinanceInvoiceListResp
getFinanceInvoiceListBySupplier mbToken mbBppBookingId mbFrom mbTo mbInvoiceType mbLimit mbOffset = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (mbToken == Just internalAPIKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

  bppBookingId <- mbBppBookingId & fromMaybeM (InvalidRequest "bppBookingId is required")
  booking <- QBooking.findByBPPBookingId (Id bppBookingId :: Id BPPBooking) >>= fromMaybeM (BookingDoesNotExist bppBookingId)

  invoices <-
    QInvoiceExtra.findByIssuedToAndType
      booking.riderId.getId
      mbInvoiceType
      mbFrom
      mbTo
      (mbLimit <|> Just 10)
      (mbOffset <|> Just 0)

  items <- mapM buildInvoiceListItem invoices

  pure $
    API.FinanceInvoiceListResp
      { invoices = items,
        totalItems = length items
      }
  where
    buildInvoiceListItem :: FinanceInvoice.Invoice -> Flow API.FinanceInvoiceListItem
    buildInvoiceListItem invoice = do
      indirectTaxTxns <- QIndirectTaxExtra.findByInvoiceNumber invoice.invoiceNumber

      let (taxableValue, gstRate, gstAmount, cgstAmount, sgstAmount, gstinOfParty, sacCode, mbTaxRate, mbIssuedToTaxNo, mbIssuedByTaxNo) = case indirectTaxTxns of
            (txn : _) ->
              ( Just txn.taxableValue,
                Just txn.gstRate,
                Just txn.totalGstAmount,
                Just txn.cgstAmount,
                Just txn.sgstAmount,
                txn.gstinOfParty,
                txn.sacCode,
                txn.taxRate,
                txn.issuedToTaxNo,
                txn.issuedByTaxNo
              )
            _ -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

      ledgerLinks <- QInvoiceLedgerLink.findByInvoice invoice.id
      ledgerEntries <- mapM (QLedgerEntry.findById . (.ledgerEntryId)) ledgerLinks

      let (rideIds, subscriptionIds) = foldr extractIds ([], []) (catMaybes ledgerEntries)

      mbPaymentMethod <- case invoice.paymentOrderId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing

      pure $
        API.FinanceInvoiceListItem
          { invoiceId = invoice.id.getId,
            invoiceNumber = invoice.invoiceNumber,
            invoiceType = invoice.invoiceType,
            invoiceDate = invoice.issuedAt,
            invoiceStatus = invoice.status,
            counterpartyType = invoice.issuedToType,
            counterpartyId = invoice.issuedToId,
            taxableValue = taxableValue,
            gstRate = gstRate,
            gstAmount = gstAmount,
            cgstAmount = cgstAmount,
            sgstAmount = sgstAmount,
            totalInvoiceValue = invoice.totalAmount,
            taxableValueOfServiceSupplied = Just invoice.subtotal,
            tdsReference = Nothing,
            irn = invoice.irn,
            rideId = listToMaybe rideIds,
            subscriptionId = listToMaybe subscriptionIds,
            supplierName = invoice.supplierName,
            supplierAddress = invoice.supplierAddress,
            supplierGstin = invoice.supplierGSTIN,
            supplierTaxNo = invoice.supplierTaxNo,
            issuedToName = invoice.issuedToName,
            issuedToAddress = invoice.issuedToAddress,
            issuedByName = invoice.issuedByName,
            issuedByAddress = invoice.issuedByAddress,
            gstinOfParty = gstinOfParty,
            sacCode = sacCode,
            paymentMethod = mbPaymentMethod,
            lineItems = invoice.lineItems,
            generatedAt = invoice.createdAt,
            taxRate = mbTaxRate,
            issuedToTaxNo = mbIssuedToTaxNo,
            issuedByTaxNo = mbIssuedByTaxNo
          }

    extractIds :: LedgerEntry.LedgerEntry -> ([Text], [Text]) -> ([Text], [Text])
    extractIds entry (rides, subs) =
      case entry.referenceType of
        "Ride" -> (entry.referenceId : rides, subs)
        "SubscriptionPurchase" -> (rides, entry.referenceId : subs)
        _ -> (rides, subs)
