module Domain.Action.UI.FinanceInvoice (getFinanceInvoicePdf) where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import Domain.Types.Invoice (InvoiceType (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (head, listToMaybe, showBaseUrl)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Invoice (InvoiceStatus (..))
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
      (Just Ride, Just rideId) -> fetchInvoicesByRideId rideId personId (Just Ride)
      (Just RideCancellation, Just rideId) -> fetchInvoicesByRideId rideId personId (Just RideCancellation)
      (Just Ride, Nothing) -> throwError $ InvalidRequest "referenceId (rideId) is required for invoiceType=Ride"
      (Just RideCancellation, Nothing) -> throwError $ InvalidRequest "referenceId (rideId) is required for invoiceType=RideCancellation"
      _ ->
        let hasDateRange = isJust mbFrom || isJust mbTo
            statusFilter = if hasDateRange then [] else [Draft, Issued, Paid]
            limitArg = if hasDateRange then mbLimit else Just 1
         in QInvoiceExtra.findByMerchantOpCityIdAndDateRange
              person.merchantOperatingCityId.getId
              fromTime
              toTime
              mbInvoiceType
              Nothing
              (Just personId.getId)
              Nothing
              Nothing
              statusFilter
              limitArg
              (mbOffset <|> Just 0)

  when (null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let locale = languageToLocale person.language
      tz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRiderConfig
      cfg = InvoicePdfConfig {locale, timezone = tz, logoUrl = mbRiderConfig >>= (.invoiceConfig) >>= (.logoUrl) <&> showBaseUrl}

  -- Per-invoice isolation: parseLineItems throws on legacy/unmigrated rows;
  -- skip those individually so one bad row doesn't kill the whole list response.
  results <- forM invoices $ \inv -> do
    res <- withTryCatch ("renderInvoice:" <> inv.invoiceNumber) $ do
      let items = parseLineItems inv.lineItems
      taxTxns <- QIndirectTaxExtra.findByInvoiceNumber inv.invoiceNumber
      let mbTaxTxn = Kernel.Prelude.listToMaybe taxTxns
      (mbPayType, mbBrand, mbLast4) <- case inv.paymentOrderId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          let mbTxn = Kernel.Prelude.listToMaybe txns
          pure (mbTxn >>= (.paymentMethodType), mbTxn >>= (.cardBrand), mbTxn >>= (.cardLastFourDigits))
        Nothing -> pure (Nothing, Nothing, Nothing)
      pure $ buildInvoicePdfData inv items mbTaxTxn mbPayType mbBrand mbLast4 Nothing Nothing Nothing
    case res of
      Right pdfData -> pure (Just pdfData)
      Left err -> do
        logError $ "Skipping invoice " <> inv.invoiceNumber <> " (render failed): " <> show err
        pure Nothing
  let pdfDatas = catMaybes results
  when (null pdfDatas) $
    throwError $ InvalidRequest "No invoices could be rendered (all failed)"

  -- Always render a single invoice PDF; never aggregate. If multiple invoices
  -- match the filters, the first (newest by issuedAt DESC) is rendered.
  let chosenPdfData = head pdfDatas
      chosenInv = chosenPdfData.financeInvoice
      html = renderInvoiceHtml cfg chosenPdfData

  pdfBase64 <- generateFinanceInvoicePdf chosenInv.invoiceNumber html

  pure $
    API.FinanceInvoicePdfResp
      { pdfBase64 = pdfBase64,
        invoiceNumber = chosenInv.invoiceNumber
      }

fetchInvoicesByRideId :: Text -> Id DP.Person -> Maybe InvoiceType -> Flow [FInvoice.Invoice]
fetchInvoicesByRideId rideId personId mbInvoiceType = do
  ride <- QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (booking.riderId == personId) $
    throwError $ InvalidRequest "Ride does not belong to this rider"
  QInvoiceExtra.findByReferenceIdWithOptions rideId mbInvoiceType [Draft, Issued, Paid] (Just 1) (Just 0)
