module Domain.Action.UI.FinanceInvoice
  ( getSubscriptionInvoices,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import Data.Time (UTCTime)
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (listToMaybe)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import Storage.Beam.Payment ()
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SubscriptionPurchase as QSubscriptionPurchase

-- | List finance invoices for the authenticated driver/fleet owner.
-- Enriches each invoice with GST details from IndirectTaxTransaction
-- and payment method from PaymentTransaction via point queries.
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

  -- Determine the identifier: for Ride invoices, driver is the supplier;
  -- for SubscriptionPurchase/RideCancellation, driver is the issuedTo party.
  let driverIdText = driverId.getId

  -- Fetch invoices based on type.
  -- Ride type uses supplierId filter; others use issuedToId filter.
  -- If no type is specified, we query by issuedToId (covers Subscription + Cancellation).
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

  -- Enrich each invoice with GST details and payment method via point queries
  items <- mapM buildInvoiceItem invoices

  pure $
    API.FinanceInvoiceListRes
      { invoices = items,
        totalItems = length items
      }
  where
    buildInvoiceItem :: FinanceInvoice.Invoice -> Flow API.FinanceInvoiceItem
    buildInvoiceItem invoice = do
      -- Point query: IndirectTaxTransaction by invoice_number
      indirectTaxTxns <- QIndirectTaxExtra.findByInvoiceNumber invoice.invoiceNumber
      let mbTaxTxn = listToMaybe indirectTaxTxns

      -- Point query: PaymentTransaction by payment_order_id (if present)
      mbPaymentMethod <- case invoice.paymentOrderId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing

      -- For SubscriptionPurchase invoices, look up totalCredit from SubscriptionPurchase
      mbTotalCredit <- case invoice.invoiceType of
        FinanceInvoice.SubscriptionPurchase -> do
          mbSub <- QSubscriptionPurchase.findByFinanceInvoiceId invoice.id
          pure $ (.planRideCredit) <$> mbSub
        _ -> pure Nothing

      -- Compute derived values
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
            -- VAT integration: new generic tax fields from indirect_tax_transaction
            taxRate = mbTaxTxn >>= (.taxRate),
            issuedToTaxNo = mbTaxTxn >>= (.issuedToTaxNo),
            issuedByTaxNo = mbTaxTxn >>= (.issuedByTaxNo)
          }

    mkComponentRate getAmount txn = do
      componentAmount <- getAmount txn
      guard (txn.taxableValue > 0)
      pure $ realToFrac (componentAmount / txn.taxableValue) * 100.0
