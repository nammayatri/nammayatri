module Domain.Action.UI.FinanceInvoice
  ( getRidePaymentInvoice,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Ride (Ride)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (listToMaybe)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import Storage.Beam.Payment ()
import qualified Storage.Queries.Person as QPerson

getRidePaymentInvoice ::
  ( Maybe (Id Person),
    Id Merchant
  ) ->
  Id Ride ->
  Flow API.FinanceInvoiceItem
getRidePaymentInvoice (mbPersonId, _) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  _person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  indirectTaxTxns <- QIndirectTaxExtra.findByReferenceId rideId.getId
  mbInvoiceNumber <- pure $ listToMaybe indirectTaxTxns >>= (.invoiceNumber)
  invoiceNumber <- mbInvoiceNumber & fromMaybeM (InvalidRequest $ "Ride payment invoice not found for rideId: " <> rideId.getId)
  invoice <- QFinanceInvoice.findByNumber invoiceNumber >>= fromMaybeM (InvalidRequest $ "Invoice not found for invoice number: " <> invoiceNumber)
  when (invoice.issuedToId /= personId.getId) $
    throwError $ InvalidRequest "This invoice does not belong to the rider."
  buildInvoiceItem invoice
  where
    buildInvoiceItem :: FinanceInvoice.Invoice -> Flow API.FinanceInvoiceItem
    buildInvoiceItem invoice = do
      indirectTaxTxns <- QIndirectTaxExtra.findByInvoiceNumber invoice.invoiceNumber
      let mbTaxTxn = listToMaybe indirectTaxTxns
      mbPaymentMethod <- case invoice.paymentOrderId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing
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
            totalCredit = Nothing,
            taxRate = mbTaxTxn >>= (.taxRate),
            issuedToTaxNo = mbTaxTxn >>= (.issuedToTaxNo),
            issuedByTaxNo = mbTaxTxn >>= (.issuedByTaxNo)
          }

    mkComponentRate getAmount txn = do
      componentAmount <- getAmount txn
      guard (txn.taxableValue > 0)
      pure $ realToFrac (componentAmount / txn.taxableValue) * 100.0
