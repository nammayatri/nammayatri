module Domain.Action.UI.Confirm
  ( confirm,
    ConfirmAPIReq (..),
    ConfirmLocationAPIEntity (..),
    ConfirmRes (..),
    ConfirmQuoteDetails (..),
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as SQuote
import Domain.Types.RentalSlab
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSRLoc
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import Types.Error
import Utils.Common

-- API types

data ConfirmAPIReq = ConfirmAPIReq
  { fromLocation :: ConfirmLocationAPIEntity,
    toLocation :: Maybe ConfirmLocationAPIEntity
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ConfirmLocationAPIEntity = ConfirmLocationAPIEntity
  { street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- domain types

data ConfirmRes = ConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    fromLoc :: LatLong,
    toLoc :: Maybe LatLong,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    startTime :: UTCTime,
    bookingId :: Id SRB.Booking
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmRentalDetails RentalSlabAPIEntity
  | ConfirmAutoDetails (Id SQuote.BPPQuote)
  deriving (Show, Generic)

confirm :: EsqDBFlow m r => Id SP.Person -> Id SQuote.Quote -> ConfirmAPIReq -> m ConfirmRes
confirm personId quoteId req = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
  bFromLocation <- buildBookingLocation now (fromLocation, req.fromLocation)
  mbBToLocation <- buildBookingLocation now `mapM` ((,) <$> mbToLocation <*> req.toLocation)
  booking <- buildBooking searchRequest quote bFromLocation mbBToLocation now
  let details = mkConfirmQuoteDetails quote.quoteDetails
  DB.runTransaction $ do
    QRideB.create booking
  return $
    ConfirmRes
      { bookingId = booking.id,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        fromLoc = LatLong {lat = fromLocation.lat, lon = fromLocation.lon},
        toLoc = mbToLocation <&> \toLocation -> LatLong {lat = toLocation.lat, lon = toLocation.lon},
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = details,
        startTime = searchRequest.startTime
      }
  where
    mkConfirmQuoteDetails :: SQuote.QuoteDetails -> ConfirmQuoteDetails
    mkConfirmQuoteDetails = \case
      SQuote.OneWayDetails _ -> ConfirmOneWayDetails
      SQuote.RentalDetails RentalSlab {..} -> ConfirmRentalDetails $ RentalSlabAPIEntity {..}
      SQuote.DriverOfferDetails driverOffer -> ConfirmAutoDetails driverOffer.bppQuoteId

buildBookingLocation :: MonadGuid m => UTCTime -> (DSRLoc.SearchReqLocation, ConfirmLocationAPIEntity) -> m DBL.BookingLocation
buildBookingLocation now (searchReqLocation, ConfirmLocationAPIEntity {..}) = do
  locId <- generateGUID
  let address = DBL.LocationAddress {..}
  return
    DBL.BookingLocation
      { id = locId,
        lat = searchReqLocation.lat,
        lon = searchReqLocation.lon,
        address,
        createdAt = now,
        updatedAt = now
      }

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  SQuote.Quote ->
  DBL.BookingLocation ->
  Maybe DBL.BookingLocation ->
  UTCTime ->
  m SRB.Booking
buildBooking searchRequest quote fromLoc mbToLoc now = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    SRB.Booking
      { id = Id id,
        bppBookingId = Nothing,
        status = SRB.NEW,
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
      SQuote.OneWayDetails _ -> buildOneWayDetails
      SQuote.RentalDetails rentalSlab -> pure $ SRB.RentalDetails rentalSlab
      SQuote.DriverOfferDetails _ -> buildOneWayDetails -- do we need DriverOfferDetails for Booking?
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure . SRB.OneWayDetails $ SRB.OneWayBookingDetails {..}
