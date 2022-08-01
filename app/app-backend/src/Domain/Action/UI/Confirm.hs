module Domain.Action.UI.Confirm
  ( confirm,
    ConfirmReq (..),
    ConfirmLocationReq (..),
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
import Domain.Types.BookingLocation
import qualified Domain.Types.BookingLocation as DBL
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as SQuote
import Domain.Types.RentalSlab
import qualified Domain.Types.SearchReqLocation as DSRLoc
import qualified Domain.Types.SearchRequest as DSReq
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchReqLocation as QSRLoc
import qualified Storage.Queries.SearchRequest as QSReq
import Types.Error
import Utils.Common

data ConfirmReq = ConfirmReq
  { fromLocation :: ConfirmLocationReq,
    toLocation :: Maybe ConfirmLocationReq
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ConfirmLocationReq = ConfirmLocationReq
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

confirm :: EsqDBFlow m r => Id SP.Person -> Id SQuote.Quote -> ConfirmReq -> m ConfirmRes
confirm personId quoteId req = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  fromLocation <- QSRLoc.findById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
  mbToLocation <- searchRequest.toLocationId `forM` (QSRLoc.findById >=> fromMaybeM LocationNotFound)
  bFromLocation <- buildBookingLocation now (fromLocation, req.fromLocation)
  mbBToLocation <- buildBookingLocation now `mapM` ((,) <$> mbToLocation <*> req.toLocation)
  booking <- buildBooking searchRequest quote bFromLocation.id (mbBToLocation <&> (.id)) now
  let details = mkConfirmQuoteDetails quote.quoteDetails
  DB.runTransaction $ do
    QBLoc.create bFromLocation
    whenJust mbBToLocation QBLoc.create
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

buildBookingLocation :: MonadGuid m => UTCTime -> (DSRLoc.SearchReqLocation, ConfirmLocationReq) -> m DBL.BookingLocation
buildBookingLocation now (searchReqLocation, ConfirmLocationReq {..}) = do
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
  Id DBL.BookingLocation ->
  Maybe (Id DBL.BookingLocation) ->
  UTCTime ->
  m SRB.Booking
buildBooking searchRequest quote fromLocId mbToLocId now = do
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
        fromLocationId = fromLocId,
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
      toLocationId <- mbToLocId & fromMaybeM (InternalError "distance is null for rental search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for rental search request")
      pure . SRB.OneWayDetails $ SRB.OneWayBookingDetails {..}
