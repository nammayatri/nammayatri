module Domain.Action.Internal.FinanceInvoice (getFinanceInvoicePdfByBppBookingId) where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import Domain.Types.Booking (BPPBooking)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (last, listToMaybe)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Invoice (InvoiceType)
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra
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
  Maybe DateOrTime ->
  Maybe DateOrTime ->
  Maybe InvoiceType ->
  Flow API.FinanceInvoicePdfResp
getFinanceInvoicePdfByBppBookingId mbToken mbBppBookingId mbFrom mbTo mbInvoiceType = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (mbToken == Just internalAPIKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

  bppBookingId <- mbBppBookingId & fromMaybeM (InvalidRequest "bppBookingId is required")

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

  when (null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let locale = countryToLocale merchantOpCity.country
      tz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRiderConfig
      cfg = InvoicePdfConfig {locale, timezone = tz, logoUrl = mbRiderConfig >>= (.invoiceConfig) >>= (.logoUrl)}

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
  where
    countryToLocale Context.Finland = FI
    countryToLocale Context.Netherlands = NL
    countryToLocale _ = EN

