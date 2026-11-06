module SharedLogic.InvoiceEmail
  ( InvoiceEmailFlow,
    findEmailTemplate,
    findInvoiceForEmail,
    resolveInvoiceRecipient,
    sendInvoiceEmail,
    invoiceEmailSentKey,
    invoiceEmailLockKey,
  )
where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Base64 as B64
import Data.Char (toUpper)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as DT
import Domain.Action.UI.FinanceInvoice (renderFinanceInvoicePdf)
import qualified Domain.Types.Booking.API as DBAPI
import qualified Domain.Types.EmailTemplate as DET
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Extra.Ride as DRideExtra
import Domain.Types.Invoice (InvoiceType (..))
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Trip as DTrip
import qualified Email.Flow as Email
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (Language (ENGLISH), ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (valueToMaybe)
import Lib.Finance.Domain.Types.Invoice (InvoiceStatus)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Invoice.Email as FEmail
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as YudhishthiraBeamFlow
import qualified SharedLogic.Type as SLT
import qualified Storage.CachedQueries.InvoiceTemplate as CQIT
import qualified Storage.Queries.EmailTemplate as QET
import Tools.Error
import Tools.InvoicePDF (getToLocationFromBooking)
import Tools.Metrics (CoreMetrics)

type InvoiceEmailFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    ClickhouseFlow m r,
    YudhishthiraBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r,
    HasField "emailServiceConfig" r Email.EmailServiceConfig
  )

invoiceEmailSentKey :: DET.EmailTemplateKey -> Id DRide.Ride -> Maybe Text -> Text
invoiceEmailSentKey templateKey rideId refundRequestId = "InvoiceEmail:Sent:" <> invoiceEmailKeySuffix templateKey rideId refundRequestId

invoiceEmailLockKey :: DET.EmailTemplateKey -> Id DRide.Ride -> Maybe Text -> Text
invoiceEmailLockKey templateKey rideId refundRequestId = "InvoiceEmail:Lock:" <> invoiceEmailKeySuffix templateKey rideId refundRequestId

invoiceEmailKeySuffix :: DET.EmailTemplateKey -> Id DRide.Ride -> Maybe Text -> Text
invoiceEmailKeySuffix templateKey rideId refundRequestId = show templateKey <> ":" <> rideId.getId <> maybe "" (":" <>) refundRequestId

findInvoiceForEmail :: FinanceBeamFlow.BeamFlow m r => DET.EmailTemplateKey -> Id DRide.Ride -> Maybe Text -> [InvoiceStatus] -> m (Maybe FInvoice.Invoice)
findInvoiceForEmail templateKey rideId refundRequestId statuses = case templateKey of
  DET.RIDE_INVOICE -> latestOfType Ride
  DET.RIDE_CANCELLATION_INVOICE -> latestOfType RideCancellation
  DET.REFUND_INVOICE ->
    find (\invoice -> isJust refundRequestId && invoice.entityReferenceId == refundRequestId)
      <$> QInvoiceExtra.findByReferenceIdWithOptions rideId.getId (Just Refund) Nothing statuses Nothing Nothing
  where
    latestOfType invoiceType = listToMaybe <$> QInvoiceExtra.findByReferenceIdWithOptions rideId.getId (Just invoiceType) Nothing statuses (Just 1) (Just 0)

findEmailTemplate :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> DET.EmailTemplateKey -> Language -> m (Maybe DET.EmailTemplate)
findEmailTemplate merchantOperatingCityId templateKey language =
  QET.findByMerchantOperatingCityIdAndTemplateKeyAndLanguage merchantOperatingCityId templateKey language
    >>= maybe (QET.findByMerchantOperatingCityIdAndTemplateKeyAndLanguage merchantOperatingCityId templateKey ENGLISH) (pure . Just)

resolveInvoiceRecipient :: EncFlow m r => DP.Person -> SLT.BillingCategory -> m (Maybe Text)
resolveInvoiceRecipient person billingCategory = do
  mbBusinessEmail <-
    if billingCategory == SLT.BUSINESS && person.businessProfileVerified == Just True
      then mapM decrypt person.businessEmail
      else pure Nothing
  mbPersonalEmail <- mapM decrypt person.email
  pure $ nonEmpty' mbBusinessEmail <|> nonEmpty' mbPersonalEmail
  where
    nonEmpty' mbEmail = mfilter (not . T.null) (T.strip <$> mbEmail)

sendInvoiceEmail ::
  InvoiceEmailFlow m r =>
  DET.EmailTemplate ->
  DRC.RiderConfig ->
  DP.Person ->
  DBAPI.BookingAPIEntity ->
  FInvoice.Invoice ->
  Text ->
  m ()
sendInvoiceEmail template riderConfig person booking invoice recipient = do
  ride <- listToMaybe booking.rideList & fromMaybeM (InvalidRequest "No ride found for invoice email")
  emailConfig <- valueToMaybe @FEmail.InvoiceEmailConfig template.config & fromMaybeM (InvalidRequest $ "Invoice email config is invalid for template " <> template.id.getId)
  let language = fromMaybe ENGLISH person.language
      mocId = booking.merchantOperatingCityId
  mbPdfTemplate <- CQIT.findByMerchantOpCityIdInvoiceTypeAndLanguage mocId Nothing language Nothing
  (pdfBase64, invoiceNumber) <- renderFinanceInvoicePdf mocId.getId (if isJust mbPdfTemplate then language else ENGLISH) [invoice]
  pdfBytes <- either (\err -> throwError $ InternalError ("Invoice PDF decode failed: " <> T.pack err)) pure (B64.decode (TE.encodeUtf8 (stripDataUri pdfBase64)))
  let emailData = buildInvoiceEmailData emailConfig riderConfig booking ride invoice invoiceNumber
      rendered = FEmail.renderInvoiceEmail template.subjectTemplate template.htmlTemplate emailData
      attachment =
        Email.EmailAttachment
          { content = pdfBytes,
            filename = emailConfig.attachmentNamePrefix <> "_" <> invoiceNumber <> ".pdf",
            contentType = "application/pdf"
          }
  unless (null rendered.missingKeys) $
    logWarning $ "Invoice email template has unfilled keys: " <> T.intercalate "," rendered.missingKeys
  emailServiceConfig <- asks (.emailServiceConfig)
  liftIO $ Email.sendEncodedEmailWithAttachments emailServiceConfig emailConfig.fromEmail [recipient] rendered.subject rendered.html Email.HtmlText [attachment]
  logInfo $ "Invoice email sent for ride " <> ride.id.getId <> " invoice " <> invoiceNumber

stripDataUri :: Text -> Text
stripDataUri encoded = case T.breakOn "base64," encoded of
  (_, rest) | not (T.null rest) -> T.drop (T.length "base64,") rest
  _ -> encoded

buildInvoiceEmailData ::
  FEmail.InvoiceEmailConfig ->
  DRC.RiderConfig ->
  DBAPI.BookingAPIEntity ->
  DRideExtra.RideAPIEntity ->
  FInvoice.Invoice ->
  Text ->
  FEmail.InvoiceEmailData
buildInvoiceEmailData cfg riderConfig booking ride invoice invoiceNumber =
  let tz = DT.minutesToTimeZone (riderConfig.timeDiffFromUtc.getSeconds `div` 60)
      toLocal = DT.utcToLocalTime tz
      locale = FEmail.mkInvoiceEmailLocale cfg
      dateLabel = FEmail.formatEmailDate locale . toLocal
      clockLabel = FEmail.formatEmailClock cfg.use24HourClock . toLocal
      stopLabel t = clockLabel t <> " • " <> dateLabel t
      scheduled = booking.rideScheduledTime
      startTime = ride.rideStartTime
      endTime = ride.rideEndTime
      dropLocation = getToLocationFromBooking booking
      paymentTime = (booking.cardInfo >>= (.paymentInitiatedAt)) <|> endTime <|> Just booking.updatedAt
   in FEmail.InvoiceEmailData
        { vehicleIconUrl = mfilter (not . T.null) booking.vehicleIconUrl,
          bookingDate = dateLabel scheduled,
          bookingTime = clockLabel scheduled,
          serviceTierLabel = serviceTierLabel booking,
          price = FEmail.formatEmailMoney invoice.currency (abs invoice.totalAmount),
          payment = buildPayment cfg booking ((\t -> FEmail.formatEmailStampDate (toLocal t) <> " • " <> clockLabel t) <$> paymentTime),
          vehicleModel = nonBlank ride.vehicleModel,
          driverName = nonBlank ride.driverName,
          rideDistance = FEmail.formatEmailDistance <$> ride.chargeableRideDistance,
          estimatedDistance = FEmail.formatEmailDistance <$> booking.estimatedDistance,
          rideDuration = (\s e -> FEmail.formatEmailDuration (round (DT.diffUTCTime e s))) <$> startTime <*> endTime,
          estimatedDuration = FEmail.formatEmailDuration . (.getSeconds) <$> booking.estimatedDuration,
          rideShortId = ride.shortRideId.getShortId,
          pickup = FEmail.InvoiceEmailStop {timeLabel = maybe (stopLabel scheduled) stopLabel startTime, address = formatAddress booking.fromLocation},
          dropoff = (\loc -> FEmail.InvoiceEmailStop {timeLabel = maybe "" stopLabel endTime, address = formatAddress loc}) <$> dropLocation,
          invoiceNumber = invoiceNumber,
          referenceInvoiceNumber = invoice.referenceInvoiceNumber
        }
  where
    nonBlank value = mfilter (not . T.null) (Just (T.strip value))

serviceTierLabel :: DBAPI.BookingAPIEntity -> Text
serviceTierLabel booking =
  let tierName = fromMaybe (show booking.vehicleServiceTierType) booking.serviceTierName
   in maybe tierName (\category -> tierName <> " - " <> tripCategoryLabel category) booking.tripCategory

tripCategoryLabel :: DTrip.TripCategory -> Text
tripCategoryLabel = \case
  DTrip.OneWay _ -> "OneWay"
  DTrip.Rental _ -> "Rental"
  DTrip.RideShare _ -> "RideShare"
  DTrip.InterCity _ _ -> "InterCity"
  DTrip.CrossCity _ _ -> "CrossCity"
  DTrip.Ambulance _ -> "Ambulance"
  DTrip.Delivery _ -> "Delivery"
  DTrip.EasyBooking _ -> "EasyBooking"
  DTrip.IntercityRental _ _ -> "IntercityRental"

buildPayment :: FEmail.InvoiceEmailConfig -> DBAPI.BookingAPIEntity -> Maybe Text -> Maybe FEmail.InvoiceEmailPayment
buildPayment cfg booking stamp = case booking.paymentInstrument of
  Nothing -> Nothing
  Just DMPM.Cash -> Just FEmail.InvoiceEmailPayment {iconUrl = Just cfg.cashIconUrl, isCash = True, cardLabel = "", stamp = stamp}
  Just instrument ->
    let brand = fromMaybe (instrumentName instrument) (mfilter (not . T.null) (booking.cardInfo >>= (.cardBrand)))
        lastFour = mfilter (not . T.null) (booking.cardInfo >>= (.cardLastFourDigits))
        label = T.strip (titleCase brand <> maybe "" (" ..." <>) lastFour)
        icon = Map.lookup (T.toLower brand) cfg.cardIconUrls <|> cfg.defaultCardIconUrl <|> Just cfg.cashIconUrl
     in Just FEmail.InvoiceEmailPayment {iconUrl = icon, isCash = False, cardLabel = label, stamp = stamp}
  where
    instrumentName = \case
      DMPM.Card _ -> "Card"
      DMPM.Wallet _ -> "Wallet"
      DMPM.UPI -> "UPI"
      DMPM.NetBanking -> "NetBanking"
      DMPM.Cash -> "Cash"
      DMPM.BoothOnline -> "BoothOnline"
    titleCase word = case T.uncons word of
      Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
      Nothing -> word

formatAddress :: DLoc.LocationAPIEntity -> Text
formatAddress loc =
  T.intercalate ", " $
    filter (not . T.null) $
      map T.strip $
        catMaybes [loc.door, loc.building, loc.street, loc.area, loc.city, loc.state, loc.country]
