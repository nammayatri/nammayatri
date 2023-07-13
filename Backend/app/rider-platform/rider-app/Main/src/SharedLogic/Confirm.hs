module SharedLogic.Confirm where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RentalSlab
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSRLoc
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error

data DConfirmReq = DConfirmReq
  { personId :: Id DP.Person,
    quoteId :: Id DQuote.Quote,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod)
  }

data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    fromLoc :: DBL.BookingLocation,
    toLoc :: Maybe DBL.BookingLocation,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    booking :: DRB.Booking,
    searchRequestId :: Id DSReq.SearchRequest,
    city :: Text,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmRentalDetails RentalSlabAPIEntity
  | ConfirmAutoDetails Text (Maybe Text)
  | ConfirmOneWaySpecialZoneDetails Text
  deriving (Show, Generic)

confirm :: (EsqDBFlow m r, CacheFlow m r) => DConfirmReq -> m DConfirmRes
confirm DConfirmReq {..} = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  case quote.quoteDetails of
    DQuote.OneWayDetails _ -> pure ()
    DQuote.RentalDetails _ -> pure ()
    DQuote.DriverOfferDetails driverOffer -> do
      estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
      when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
      when (driverOffer.validTill < now) $
        throwError $ QuoteExpired quote.id.getId
    DQuote.OneWaySpecialZoneDetails _ -> pure ()
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  activeBooking <- QRideB.findByRiderIdAndStatus personId DRB.activeBookingStatus
  unless (null activeBooking) $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
      driverId = getDriverId quote.quoteDetails
  bFromLocation <- buildBookingLocation now fromLocation
  mbBToLocation <- traverse (buildBookingLocation now) mbToLocation
  exophone <- findRandomExophone searchRequest.merchantId
  booking <- buildBooking searchRequest quote bFromLocation mbBToLocation exophone now Nothing paymentMethodId driverId
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  details <- mkConfirmQuoteDetails quote.quoteDetails
  paymentMethod <- forM paymentMethodId $ \paymentMethodId' -> do
    paymentMethod <-
      CQMPM.findByIdAndMerchantId paymentMethodId' searchRequest.merchantId
        >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId'.getId)
    unless (paymentMethodId' `elem` searchRequest.availablePaymentMethods) $
      throwError (InvalidRequest "Payment method not allowed")
    pure paymentMethod

  DB.runTransaction $ do
    QRideB.create booking
    QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill}
    QEstimate.updateStatusByRequestId quote.requestId DEstimate.COMPLETED
  QPFS.clearCache searchRequest.riderId
  return $
    DConfirmRes
      { booking,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        itemId = quote.itemId,
        fromLoc = bFromLocation,
        toLoc = mbBToLocation,
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = details,
        searchRequestId = searchRequest.id,
        city = merchant.city,
        maxEstimatedDistance = searchRequest.maxDistance,
        paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> paymentMethod
      }
  where
    mkConfirmQuoteDetails quoteDetails = do
      case quoteDetails of
        DQuote.OneWayDetails _ -> pure ConfirmOneWayDetails
        DQuote.RentalDetails RentalSlab {..} -> pure $ ConfirmRentalDetails $ RentalSlabAPIEntity {..}
        DQuote.DriverOfferDetails driverOffer -> do
          estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
          pure $ ConfirmAutoDetails estimate.bppEstimateId.getId driverOffer.driverId
        DQuote.OneWaySpecialZoneDetails details -> pure $ ConfirmOneWaySpecialZoneDetails details.quoteId
    getDriverId :: DQuote.QuoteDetails -> Maybe Text
    getDriverId = \case
      DQuote.DriverOfferDetails driverOffer -> driverOffer.driverId
      _ -> Nothing

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  DQuote.Quote ->
  DBL.BookingLocation ->
  Maybe DBL.BookingLocation ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe Text ->
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  Maybe Text ->
  m DRB.Booking
buildBooking searchRequest quote fromLoc mbToLoc exophone now otpCode paymentMethodId driverId = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    DRB.Booking
      { id = Id id,
        transactionId = searchRequest.id.getId,
        bppBookingId = Nothing,
        driverId,
        fulfillmentId = mkFulfillmentId,
        quoteId = Just quote.id,
        paymentMethodId,
        paymentUrl = Nothing,
        status = DRB.NEW,
        providerId = quote.providerId,
        primaryExophone = exophone.primaryPhone,
        providerUrl = quote.providerUrl,
        providerName = quote.providerName,
        itemId = quote.itemId,
        providerMobileNumber = quote.providerMobileNumber,
        startTime = searchRequest.startTime,
        riderId = searchRequest.riderId,
        fromLocation = fromLoc,
        estimatedFare = quote.estimatedFare,
        discount = quote.discount,
        estimatedTotalFare = quote.estimatedTotalFare,
        vehicleVariant = quote.vehicleVariant,
        bookingDetails,
        tripTerms = quote.tripTerms,
        merchantId = searchRequest.merchantId,
        specialLocationTag = quote.specialLocationTag,
        createdAt = now,
        updatedAt = now
      }
  where
    mkFulfillmentId = case quote.quoteDetails of
      DQuote.DriverOfferDetails driverOffer -> Just driverOffer.estimateId.getId
      DQuote.OneWaySpecialZoneDetails details -> Just details.quoteId
      _ -> Nothing
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.RentalDetails rentalSlab -> pure $ DRB.RentalDetails rentalSlab
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
      DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}
    buildOneWaySpecialZoneDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWaySpecialZoneBookingDetails {..}

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  exophones <- CQExophone.findAllByMerchantId merchantId
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones

buildBookingLocation :: MonadGuid m => UTCTime -> DSRLoc.SearchReqLocation -> m DBL.BookingLocation
buildBookingLocation now DSRLoc.SearchReqLocation {..} = do
  locId <- generateGUID
  return
    DBL.BookingLocation
      { id = locId,
        lat,
        lon,
        address,
        createdAt = now,
        updatedAt = now
      }
