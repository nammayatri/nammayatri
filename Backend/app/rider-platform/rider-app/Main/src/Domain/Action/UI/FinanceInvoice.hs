module Domain.Action.UI.FinanceInvoice (getFinanceInvoicePdf) where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.Invoice (InvoiceType (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OfferEntity as DOfferEntity
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude (head, listToMaybe, showBaseUrl)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Finance.Domain.Types.Invoice (InvoiceStatus (..))
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Invoice.RenderTemplate as FRT
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import SharedLogic.Booking (getfareBreakups)
import qualified SharedLogic.Finance.RideReceipt as RideReceipt
import qualified SharedLogic.RenderInvoiceFromTemplate as RIFT
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.OfferEntity as QOfferEntity
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
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
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId person.merchantOperatingCityId))

  let lang = fromMaybe ENGLISH person.language
      tz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRiderConfig
      tmplLogoUrl = mbRiderConfig >>= (.invoiceConfig) >>= (.logoUrl) <&> showBaseUrl

      -- Itemized ride receipt (RiderInvoiceConfig.useFareBreakupLineItems): line items
      -- from fareBreakup display tags; the row only supplies invoiceNumber/name, so
      -- pre-finance_invoice rides render too. tz/logo from the render city's config.
      itemizedRideReceiptPdf mbRenderCfg invType ride booking = do
        let renderTz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRenderCfg
            renderLogoUrl = mbRenderCfg >>= (.invoiceConfig) >>= (.logoUrl) <&> showBaseUrl
        breakups <- case invType of
          RideCancellation -> QFareBreakup.findAllByEntityIdAndEntityType ride.id.getId DFareBreakup.RIDE
          _ -> fst <$> getfareBreakups booking (Just ride)
        fareItems <- do
          let base = RideReceipt.mkItemizedRideLineItems (RideReceipt.isSpecialZoneBooking booking) breakups
              hasOfferDiscountRow = any ((== "OFFER_DISCOUNT") . (.description)) breakups
          -- Explicit offer-discount row so the Ride list sums to the paid amount; skipped
          -- when the breakup already carries OFFER_DISCOUNT (it would double-count).
          case (invType, booking.selectedOfferId) of
            (Ride, Just _) | not hasOfferDiscountRow -> do
              mbRideOffer <- QOfferEntity.findByEntityIdAndEntityType ride.id.getId DOfferEntity.RIDE
              mbOffer <- maybe (QOfferEntity.findByEntityIdAndEntityType booking.id.getId DOfferEntity.BOOKING) (pure . Just) mbRideOffer
              pure $ base <> maybe [] (\oe -> RideReceipt.mkOfferDiscountItem oe.offerTitle oe.discountAmount) mbOffer
            _ -> pure base
        let items = fareItems
        when (null items) $
          logWarning $ "Itemized receipt: no displayable fareBreakup rows for ride " <> ride.id.getId
        mbRow <- listToMaybe <$> QInvoiceExtra.findByReferenceIdWithOptions ride.id.getId (Just invType) Nothing [Draft, Issued, Paid] (Just 1) (Just 0)
        let mbName = (mbRow >>= (.issuedToName)) <|> RideReceipt.riderDisplayName person
            invoice = maybe (RideReceipt.synthFinanceInvoice invType ride booking mbName items) (RideReceipt.overrideIssuedToName mbName) mbRow
            fields = RideReceipt.mkRideReceiptFields renderTz ride booking
            ctx =
              FRT.buildInvoiceContext
                FRT.BuildInvoiceContextInput
                  { language = lang,
                    logoUrl = renderLogoUrl,
                    sellerTradeName = Nothing,
                    appName = Nothing,
                    invoice = invoice,
                    lineItems = items,
                    mbTaxTxn = Nothing,
                    mbPaymentMode = Nothing,
                    mbCardBrand = Nothing,
                    mbCardLastFour = Nothing,
                    mbRecipientBusinessId = Nothing,
                    mbSellerBusinessId = Nothing,
                    mbSellerVatNumber = Nothing,
                    rideShortId = fields.rideShortId,
                    driverName = fields.driverName,
                    vehicleNumber = fields.vehicleNumber,
                    pickupAddress = fields.pickupAddress,
                    dropAddress = fields.dropAddress,
                    rideDate = fields.rideDate,
                    rideStartTime = fields.rideStartTime,
                    rideEndTime = fields.rideEndTime
                  }
        html <- RIFT.renderHtml (Id invoice.merchantOperatingCityId) Nothing lang renderTz ctx
        pdfBase64 <- generateFinanceInvoicePdf invoice.invoiceNumber html
        pure $
          API.FinanceInvoicePdfResp
            { pdfBase64 = pdfBase64,
              invoiceNumber = invoice.invoiceNumber
            }

  -- The per-city flag is read from the BOOKING's city (the city being rendered) —
  -- the rider's profile city can differ and must not decide this render.
  -- Lite reads suffice here (riderId + merchantOperatingCityId) for ownership check and config flag lookup.
  mbInvoiceRequestWithRideId <- case (mbInvoiceId, mbInvoiceType, mbReferenceId) of
    (Nothing, Just invType, Just rideId)
      | invType `elem` [Ride, RideCancellation] -> do
        rideLite <- QRideLite.findByIdLite (Id rideId) >>= fromMaybeM (RideNotFound rideId)
        bookingLite <- QBookingLite.findByIdLite rideLite.bookingId >>= fromMaybeM (BookingDoesNotExist rideLite.bookingId.getId)
        unless (bookingLite.riderId == personId) $
          throwError $ InvalidRequest "Ride does not belong to this rider"
        mbRenderCfg <-
          if bookingLite.merchantOperatingCityId == person.merchantOperatingCityId
            then pure mbRiderConfig
            else getConfig (RiderConfigDimensions {merchantOperatingCityId = bookingLite.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId bookingLite.merchantOperatingCityId))
        let flagOn = (mbRenderCfg >>= (.invoiceConfig) >>= (.useFareBreakupLineItems)) == Just True
        pure $ Just (invType, rideId, mbRenderCfg, flagOn)
    _ -> pure Nothing

  case mbInvoiceRequestWithRideId of
    Just (invType, rideId, mbRenderCfg, True) -> do
      ride <- QRide.findById (Id rideId) >>= fromMaybeM (RideNotFound rideId)
      booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
      itemizedRideReceiptPdf mbRenderCfg invType ride booking
    _ -> do
      let fromTime = toUTCTimeFrom <$> mbFrom
          toTime = toUTCTimeTo <$> mbTo

      invoices <- case mbInvoiceId of
        Just invoiceId -> do
          inv <- QFinanceInvoice.findById (Id invoiceId) >>= fromMaybeM (InvalidRequest $ "Invoice not found: " <> invoiceId)
          unless (inv.issuedToId == personId.getId) $
            throwError $ InvalidRequest "Invoice does not belong to this rider"
          pure [inv]
        Nothing -> case mbInvoiceRequestWithRideId of
          Just (invType, rideId, _, _) -> QInvoiceExtra.findByReferenceIdWithOptions rideId (Just invType) Nothing [Draft, Issued, Paid] (Just 1) (Just 0)
          Nothing -> case (mbInvoiceType, mbReferenceId) of
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
                    []
                    statusFilter
                    limitArg
                    (mbOffset <|> Just 0)

      when (null invoices) $
        throwError $ InvalidRequest "No invoices found for the given criteria"

      -- AggregatedCommission isn't rendered for riders — these BPP-side fields aren't on RiderConfig.
      let tmplSellerTradeName = Nothing :: Maybe Text
          tmplAppName = Nothing :: Maybe Text

      -- Per-invoice isolation: parseLineItems throws on legacy/unmigrated rows;
      -- skip those individually so one bad row doesn't kill the whole list response.
      results <- forM invoices $ \inv -> do
        res <- withTryCatch ("renderInvoice:" <> inv.invoiceNumber) $ do
          let items = parseLineItems inv.lineItems
          taxTxns <- QIndirectTaxExtra.findByInvoiceNumber (Just inv.invoiceNumber)
          let mbTaxTxn = Kernel.Prelude.listToMaybe taxTxns
          (mbPayType, mbBrand, mbLast4) <- case inv.entityReferenceId of
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
          ctx =
            FRT.buildInvoiceContext
              FRT.BuildInvoiceContextInput
                { language = lang,
                  logoUrl = tmplLogoUrl,
                  sellerTradeName = tmplSellerTradeName,
                  appName = tmplAppName,
                  invoice = chosenInv,
                  lineItems = chosenPdfData.parsedLineItems,
                  mbTaxTxn = chosenPdfData.mbTaxTxn,
                  mbPaymentMode = chosenPdfData.mbPaymentMethodType,
                  mbCardBrand = chosenPdfData.mbCardBrand,
                  mbCardLastFour = chosenPdfData.mbCardLastFour,
                  mbRecipientBusinessId = chosenPdfData.mbRecipientBusinessId,
                  mbSellerBusinessId = chosenPdfData.mbSellerBusinessId,
                  mbSellerVatNumber = chosenPdfData.mbSellerVatNumber,
                  rideShortId = Nothing,
                  driverName = Nothing,
                  vehicleNumber = Nothing,
                  pickupAddress = Nothing,
                  dropAddress = Nothing,
                  rideDate = Nothing,
                  rideStartTime = Nothing,
                  rideEndTime = Nothing
                }
          mbInvType = case chosenInv.invoiceType of
            AggregatedCommission -> Just AggregatedCommission
            _ -> Nothing
      html <- RIFT.renderHtml (Id chosenInv.merchantOperatingCityId) mbInvType lang tz ctx
      pdfBase64 <- generateFinanceInvoicePdf chosenInv.invoiceNumber html

      pure $
        API.FinanceInvoicePdfResp
          { pdfBase64 = pdfBase64,
            invoiceNumber = chosenInv.invoiceNumber
          }
