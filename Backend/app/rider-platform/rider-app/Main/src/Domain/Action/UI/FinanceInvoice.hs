module Domain.Action.UI.FinanceInvoice (getFinanceInvoicePdf) where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import Domain.Types.Invoice (InvoiceType (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (last, listToMaybe, showBaseUrl)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import Storage.Beam.Payment ()
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import "beckn-services" Tools.InvoicePdf (generateFinanceInvoicePdf)

getFinanceInvoicePdf ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant
  ) ->
  Maybe DateOrTime ->
  Maybe Text ->
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe DateOrTime ->
  Flow API.FinanceInvoicePdfResp
getFinanceInvoicePdf (mbPersonId, _) mbFrom mbInvoiceId mbInvoiceType mbLimit mbOffset mbReferenceId mbTo = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId})

  let fromTime = toUTCTimeFrom <$> mbFrom
      toTime = toUTCTimeTo <$> mbTo

  invoices <- case mbInvoiceId of
    Just invoiceId -> do
      inv <- QFinanceInvoice.findById (Id invoiceId) >>= fromMaybeM (InvalidRequest $ "Invoice not found: " <> invoiceId)
      unless (inv.issuedToId == personId.getId) $
        throwError $ InvalidRequest "Invoice does not belong to this rider"
      pure [inv]
    Nothing -> case (mbInvoiceType, mbReferenceId) of
      (Just Ride, Just rideId) -> fetchInvoicesByRideId rideId personId
      (Just RideCancellation, Just rideId) -> fetchInvoicesByRideId rideId personId
      (Just Ride, Nothing) -> throwError $ InvalidRequest "referenceId (rideId) is required for invoiceType=Ride"
      (Just RideCancellation, Nothing) -> throwError $ InvalidRequest "referenceId (rideId) is required for invoiceType=RideCancellation"
      _ ->
        QInvoiceExtra.findByMerchantOpCityIdAndDateRange
          person.merchantOperatingCityId.getId
          fromTime
          toTime
          mbInvoiceType
          Nothing
          (Just personId.getId)
          Nothing
          Nothing
          (if isJust mbFrom || isJust mbTo then Nothing else mbLimit <|> Just 1)
          (mbOffset <|> Just 0)

  when (null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let locale = languageToLocale person.language
      tz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRiderConfig
      cfg = InvoicePdfConfig {locale, timezone = tz, logoUrl = mbRiderConfig >>= (.invoiceConfig) >>= (.logoUrl) <&> showBaseUrl}

  pdfDatas <- forM invoices $ \inv -> do
    let items = parseLineItems inv.lineItems
    taxTxns <- QIndirectTaxExtra.findByInvoiceNumber inv.invoiceNumber
    let mbTaxTxn = Kernel.Prelude.listToMaybe taxTxns
    (mbPayType, mbBrand, mbLast4) <- case inv.paymentOrderId of
      Just orderId -> do
        txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
        let mbTxn = Kernel.Prelude.listToMaybe txns
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

fetchInvoicesByRideId :: Text -> Id DP.Person -> Flow [FInvoice.Invoice]
fetchInvoicesByRideId rideId personId = do
  ride <- QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (booking.riderId == personId) $
    throwError $ InvalidRequest "Ride does not belong to this rider"
  QFinanceInvoice.findByReferenceId (Just rideId)
