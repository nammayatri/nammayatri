module Domain.Action.UI.FinanceInvoice
  ( getSubscriptionInvoices,
    getFinanceInvoicePdf,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (head, listToMaybe)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import Storage.Beam.Payment ()
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SubscriptionPurchase as QSubscriptionPurchase
import Tools.Error
import "beckn-services" Tools.InvoicePdf (generateFinanceInvoicePdf)

-- | List finance invoices for the authenticated driver/fleet owner.
getSubscriptionInvoices ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Maybe UTCTime ->
  Maybe FinanceInvoice.InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Flow API.FinanceInvoiceListRes
getSubscriptionInvoices (mbDriverId, _, _) mbFrom mbInvoiceType mbLimit mbOffset mbTo = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  now <- getCurrentTime
  let fromDate = mbFrom
      toDate = mbTo <|> Just now
      limit = min 20 . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset

  let driverIdText = driverId.getId

  invoices <- case mbInvoiceType of
    Just FinanceInvoice.Ride ->
      QFinanceInvoiceExtra.findBySupplierAndType
        driverIdText
        (Just FinanceInvoice.Ride)
        fromDate
        toDate
        (Just limit)
        (Just offset)
    _ ->
      QFinanceInvoiceExtra.findByIssuedToAndType
        driverIdText
        mbInvoiceType
        fromDate
        toDate
        (Just limit)
        (Just offset)

  items <- mapM buildInvoiceItem invoices

  pure $
    API.FinanceInvoiceListRes
      { invoices = items,
        totalItems = length items
      }
  where
    buildInvoiceItem :: FinanceInvoice.Invoice -> Flow API.FinanceInvoiceItem
    buildInvoiceItem invoice = do
      indirectTaxTxns <- QIndirectTaxExtra.findByInvoiceNumber invoice.invoiceNumber
      let mbTaxTxn = Kernel.Prelude.listToMaybe indirectTaxTxns

      mbPaymentMethod <- case invoice.paymentOrderId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ Kernel.Prelude.listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing

      mbTotalCredit <- case invoice.invoiceType of
        FinanceInvoice.SubscriptionPurchase -> do
          mbSub <- QSubscriptionPurchase.findByFinanceInvoiceId invoice.id
          pure $ (.planRideCredit) <$> mbSub
        _ -> pure Nothing

      let taxAmount = invoice.totalAmount - invoice.subtotal

      pure $
        API.FinanceInvoiceItem
          { invoiceNumber = invoice.invoiceNumber,
            invoiceType = invoice.invoiceType,
            invoiceDate = invoice.issuedAt,
            taxAmount = taxAmount,
            taxableValue = invoice.subtotal,
            totalAmountPayable = invoice.totalAmount,
            gstRate = (.gstRate) <$> mbTaxTxn,
            cgstRate = mbTaxTxn >>= mkComponentRate (Just . (.cgstAmount)),
            sgstRate = mbTaxTxn >>= mkComponentRate (Just . (.sgstAmount)),
            igstRate = mbTaxTxn >>= mkComponentRate (Just . (.igstAmount)),
            sgstAmount = (.sgstAmount) <$> mbTaxTxn,
            cgstAmount = (.cgstAmount) <$> mbTaxTxn,
            igstAmount = (.igstAmount) <$> mbTaxTxn,
            totalGstAmount = (.totalGstAmount) <$> mbTaxTxn,
            paymentMethod = mbPaymentMethod,
            issuedToName = invoice.issuedToName,
            issuedToAddress = invoice.issuedToAddress,
            issuedByName = invoice.issuedByName,
            issuedByAddress = invoice.issuedByAddress,
            supplierAddress = invoice.supplierAddress,
            supplierName = invoice.supplierName,
            supplierGSTIN = invoice.supplierGSTIN,
            supplierTaxNo = invoice.supplierTaxNo,
            gstinOfParty = mbTaxTxn >>= (.gstinOfParty),
            sacCode = mbTaxTxn >>= (.sacCode),
            lineItems = Just invoice.lineItems,
            totalCredit = mbTotalCredit,
            taxRate = mbTaxTxn >>= (.taxRate),
            issuedToTaxNo = mbTaxTxn >>= (.issuedToTaxNo),
            issuedByTaxNo = mbTaxTxn >>= (.issuedByTaxNo)
          }

    mkComponentRate getAmount txn = do
      componentAmount <- getAmount txn
      guard (txn.taxableValue > 0)
      pure $ realToFrac (componentAmount / txn.taxableValue) * 100.0

-- | Generate a PDF for a single invoice.
getFinanceInvoicePdf ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Maybe DateOrTime ->
  Maybe FinanceInvoice.InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe DateOrTime ->
  Flow API.FinanceInvoicePdfResp
getFinanceInvoicePdf (mbDriverId, _, merchantOpCityId) mbFrom mbInvoiceType mbLimit mbOffset _mbReferenceId mbTo = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  merchantOpCity <-
    CQMOC.findById merchantOpCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  mbTransporterConfig <- findByMerchantOpCityId merchantOpCityId Nothing

  let fromTime = toUTCTimeFrom <$> mbFrom
      toTime = toUTCTimeTo <$> mbTo

  invoices <-
    QFinanceInvoiceExtra.findByMerchantOpCityIdAndDateRange
      merchantOpCityId.getId
      fromTime
      toTime
      mbInvoiceType
      Nothing
      (Just driverId.getId)
      Nothing
      (mbLimit <|> Just 10)
      (mbOffset <|> Just 0)

  when (null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let inv = head invoices
      items = parseLineItems inv.lineItems

  taxTxns <- QIndirectTaxExtra.findByInvoiceNumber inv.invoiceNumber
  let mbTaxTxn = Kernel.Prelude.listToMaybe taxTxns

  (mbPayType, mbBrand, mbLast4) <- case inv.paymentOrderId of
    Just orderId -> do
      txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
      let mbTxn = Kernel.Prelude.listToMaybe txns
      pure (mbTxn >>= (.paymentMethodType), mbTxn >>= (.cardBrand), mbTxn >>= (.cardLastFourDigits))
    Nothing -> pure (Nothing, Nothing, Nothing)

  let locale = countryToLocale merchantOpCity.country
      tz = countryToTimezone merchantOpCity.country
      cfg = InvoicePdfConfig {locale, timezone = tz, logoUrl = mbTransporterConfig >>= (.invoiceConfig) >>= (.logoUrl)}
      pdfData = buildInvoicePdfData inv items mbTaxTxn mbPayType mbBrand mbLast4
      html = renderInvoiceHtml cfg pdfData

  pdfBase64 <- generateFinanceInvoicePdf inv.invoiceNumber html

  pure $
    API.FinanceInvoicePdfResp
      { pdfBase64 = pdfBase64,
        invoiceNumber = inv.invoiceNumber
      }

countryToLocale :: Context.Country -> InvoiceLocale
countryToLocale Context.Finland = FI
countryToLocale Context.Netherlands = NL
countryToLocale _ = EN

countryToTimezone :: Context.Country -> DT.TimeZone
countryToTimezone Context.Finland = DT.TimeZone 180 False "EET"
countryToTimezone Context.Netherlands = DT.TimeZone 120 False "CEST"
countryToTimezone _ = DT.utc
