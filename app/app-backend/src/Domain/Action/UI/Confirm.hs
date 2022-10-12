module Domain.Action.UI.Confirm
  ( confirm,
    cancelBooking,
    ConfirmRes (..),
    ConfirmQuoteDetails (..),
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RentalSlab
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSRLoc
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

-- domain types

data ConfirmRes = ConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    fromLoc :: LatLong,
    toLoc :: Maybe LatLong,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    startTime :: UTCTime,
    booking :: DRB.Booking
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmRentalDetails RentalSlabAPIEntity
  | ConfirmAutoDetails (Id DQuote.BPPQuote)
  deriving (Show, Generic)

confirm :: EsqDBFlow m r => Id DP.Person -> Id DQuote.Quote -> m ConfirmRes
confirm personId quoteId = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  activeBooking <- QRideB.findByRiderIdAndStatus personId DRB.activeBookingStatus
  unless (null activeBooking) $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
  case quote.quoteDetails of
    DQuote.OneWayDetails _ -> pure ()
    DQuote.RentalDetails _ -> pure ()
    DQuote.DriverOfferDetails driverOffer -> do
      when (driverOffer.validTill < now) $
        throwError $ QuoteExpired quote.id.getId
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
  bFromLocation <- buildBookingLocation now fromLocation
  mbBToLocation <- traverse (buildBookingLocation now) mbToLocation
  booking <- buildBooking searchRequest quote bFromLocation mbBToLocation now
  let details = mkConfirmQuoteDetails quote.quoteDetails
  DB.runTransaction $ do
    QRideB.create booking
  return $
    ConfirmRes
      { booking,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        fromLoc = LatLong {lat = fromLocation.lat, lon = fromLocation.lon},
        toLoc = mbToLocation <&> \toLocation -> LatLong {lat = toLocation.lat, lon = toLocation.lon},
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = details,
        startTime = searchRequest.startTime
      }
  where
    mkConfirmQuoteDetails :: DQuote.QuoteDetails -> ConfirmQuoteDetails
    mkConfirmQuoteDetails = \case
      DQuote.OneWayDetails _ -> ConfirmOneWayDetails
      DQuote.RentalDetails RentalSlab {..} -> ConfirmRentalDetails $ RentalSlabAPIEntity {..}
      DQuote.DriverOfferDetails driverOffer -> ConfirmAutoDetails driverOffer.bppQuoteId

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

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  DQuote.Quote ->
  DBL.BookingLocation ->
  Maybe DBL.BookingLocation ->
  UTCTime ->
  m DRB.Booking
buildBooking searchRequest quote fromLoc mbToLoc now = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    DRB.Booking
      { id = Id id,
        bppBookingId = Nothing,
        status = DRB.NEW,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        providerName = quote.providerName,
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
        createdAt = now,
        updatedAt = now
      }
  where
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.RentalDetails rentalSlab -> pure $ DRB.RentalDetails rentalSlab
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}

-- cancel booking when QUOTE_EXPIRED on bpp side, or other EXTERNAL_API_CALL_ERROR catched
cancelBooking :: (EsqDBFlow m r, HedisFlow m r, CoreMetrics m) => DRB.Booking -> m ()
cancelBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  bookingCancellationReason <- buildBookingCancellationReason booking.id
  DB.runTransaction $ do
    QRideB.updateStatus booking.id DRB.CANCELLED
    QBCR.create bookingCancellationReason
  Notify.notifyOnBookingCancelled booking DBCR.ByApplication

buildBookingCancellationReason ::
  MonadFlow m =>
  Id DRB.Booking ->
  m DBCR.BookingCancellationReason
buildBookingCancellationReason bookingId = do
  guid <- generateGUID
  return
    DBCR.BookingCancellationReason
      { id = guid,
        bookingId = bookingId,
        rideId = Nothing,
        source = DBCR.ByApplication,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing
      }
