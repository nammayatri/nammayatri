module Domain.Action.UI.FinanceInvoice
  ( getSubscriptionInvoices,
    getFinanceInvoicePdf,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import qualified AWS.S3 as S3
import qualified Data.Text as T
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..))
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person (Person)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (head, listToMaybe)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import qualified SharedLogic.Finance.InvoiceDocument as InvoiceDocument
import Storage.Beam.Payment ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SubscriptionPurchase as QSubscriptionPurchase
import Tools.Error

-- | List finance invoices for the authenticated driver/fleet owner.
getSubscriptionInvoices ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Maybe UTCTime ->
  Maybe InvoiceType ->
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

  invoicesAll <- case mbInvoiceType of
    Just Ride ->
      QFinanceInvoiceExtra.findBySupplierAndType
        driverIdText
        (Just Ride)
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

  -- Hide Voided/Cancelled (incl. 0-amount AggregatedCommission markers).
  let invoices = filter (\i -> i.status `notElem` [FinanceInvoice.Voided, FinanceInvoice.Cancelled]) invoicesAll

  items <- mapM buildInvoiceItem invoices

  pure $
    API.FinanceInvoiceListRes
      { invoices = items,
        totalItems = length items
      }
  where
    buildInvoiceItem :: FinanceInvoice.Invoice -> Flow API.FinanceInvoiceItem
    buildInvoiceItem invoice = do
      indirectTaxTxns <- QIndirectTaxExtra.findByInvoiceNumber (Just invoice.invoiceNumber)
      let mbTaxTxn = Kernel.Prelude.listToMaybe indirectTaxTxns

      mbPaymentMethod <- case invoice.entityReferenceId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ Kernel.Prelude.listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing

      mbTotalCredit <- case invoice.invoiceType of
        SubscriptionPurchase -> do
          mbSub <- QSubscriptionPurchase.findByFinanceInvoiceId invoice.id
          pure $ (.planRideCredit) <$> mbSub
        _ -> pure Nothing

      let taxAmount = invoice.totalAmount - invoice.subtotal

      mbPdfUrl <- InvoiceDocument.mkInvoicePdfUrl invoice

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
            merchantGstin = invoice.merchantGstin,
            gstinOfParty = mbTaxTxn >>= (.gstinOfParty),
            sacCode = mbTaxTxn >>= (.sacCode),
            lineItems = Just invoice.lineItems,
            totalCredit = mbTotalCredit,
            taxRate = mbTaxTxn >>= (.taxRate),
            issuedToTaxNo = mbTaxTxn >>= (.issuedToTaxNo),
            issuedByTaxNo = mbTaxTxn >>= (.issuedByTaxNo),
            pdfUrl = mbPdfUrl -- pre-signed S3 URL; Nothing until the PDF is materialised
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
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe DateOrTime ->
  Flow API.FinanceInvoicePdfResp
getFinanceInvoicePdf (mbDriverId, _, merchantOpCityId) mbFrom mbInvoiceType mbLimit mbOffset _mbReferenceId mbTo = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing

  let fromTime = toUTCTimeFrom <$> mbFrom
      toTime = toUTCTimeTo <$> mbTo

  invoicesAll <-
    QFinanceInvoiceExtra.findByMerchantOpCityIdAndDateRange
      merchantOpCityId.getId
      fromTime
      toTime
      mbInvoiceType
      Nothing
      (Just driverId.getId)
      Nothing
      Nothing
      []
      []
      (mbLimit <|> Just 10)
      (mbOffset <|> Just 0)

  -- Hide Voided/Cancelled (incl. 0-amount AggregatedCommission markers).
  let invoices = filter (\i -> i.status `notElem` [FinanceInvoice.Voided, FinanceInvoice.Cancelled]) invoicesAll

  when (null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let inv = head invoices
  -- Read path: return the stored artifact if present, else render on demand.
  -- Write-through: when the merchant opts into PDF storage, persist the freshly
  -- rendered PDF so subsequent reads (and ONDC sharing) are served from S3.
  pdfBase64 <- case inv.pdfS3Path of
    Just path -> S3.get (T.unpack path)
    Nothing -> do
      pdf <- InvoiceDocument.renderInvoicePdfBase64 inv
      when (fromMaybe False (mbTransporterConfig >>= (.invoiceConfig) >>= (.enableInvoicePdfS3Storage))) $
        InvoiceDocument.storeInvoicePdf inv pdf
      pure pdf

  pure $
    API.FinanceInvoicePdfResp
      { pdfBase64 = pdfBase64,
        invoiceNumber = inv.invoiceNumber
      }
