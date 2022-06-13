module Domain.Action.UI.Init where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Domain.Types.BookingLocation as DLoc
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.RideBooking as SRB
import Domain.Types.SearchReqLocation (SearchReqLocation (..))
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRideB
import qualified Storage.Queries.SearchReqLocation as QSRLoc
import qualified Storage.Queries.SearchRequest as QSReq
import Types.Error
import Utils.Common

newtype InitReq = InitReq
  { quoteId :: Id SQuote.Quote
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data InitRes = InitRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    fromLoc :: LatLong,
    toLoc :: Maybe LatLong,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: SQuote.QuoteDetails,
    startTime :: UTCTime,
    bookingId :: Id SRB.RideBooking
  }
  deriving (Show, Generic)

init :: EsqDBFlow m r => Id SP.Person -> InitReq -> m InitRes
init personId req = do
  now <- getCurrentTime
  quote <- QQuote.findById req.quoteId >>= fromMaybeM (QuoteDoesNotExist req.quoteId.getId)
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  fromLocation <- QSRLoc.findById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
  mbToLocation :: Maybe SearchReqLocation <- searchRequest.toLocationId `forM` (QSRLoc.findById >=> fromMaybeM LocationNotFound)
  bFromLocation <- buildBLoc fromLocation now
  mbBToLocation <- (`buildBLoc` now) `mapM` mbToLocation
  booking <- buildRideBooking searchRequest quote bFromLocation.id (mbBToLocation <&> (.id)) now
  DB.runTransaction $ do
    QBLoc.create bFromLocation
    whenJust mbBToLocation $ \loc -> QBLoc.create loc
    QRideB.create booking
  return $
    InitRes
      { bookingId = booking.id,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        fromLoc = LatLong {lat = fromLocation.lat, lon = fromLocation.lon},
        toLoc = mbToLocation <&> \toLocation -> LatLong {lat = toLocation.lat, lon = toLocation.lon},
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = quote.quoteDetails,
        startTime = searchRequest.startTime
      }
  where
    buildBLoc searchReqLocation now = do
      locId <- generateGUID
      let address =
            DLoc.LocationAddress
              { street = Nothing,
                door = Nothing,
                city = Nothing,
                state = Nothing,
                country = Nothing,
                building = Nothing,
                areaCode = Nothing,
                area = Nothing
              }
      return
        DLoc.BookingLocation
          { id = locId,
            lat = searchReqLocation.lat,
            lon = searchReqLocation.lon,
            address,
            createdAt = now,
            updatedAt = now
          }
    buildRideBooking searchRequest quote fromLocId toLocId now = do
      id <- generateGUID
      rideBookingDetails <- case quote.quoteDetails of
        SQuote.OneWayDetails _ -> do
          -- we need to throw errors here because of some redundancy of our domain model
          toLocationId <- toLocId & fromMaybeM (InternalError "distance is null for rental search request")
          distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for rental search request")
          pure . SRB.OneWayDetails $ SRB.OneWayRideBookingDetails {..}
        SQuote.RentalDetails rentalSlab ->
          pure $ SRB.RentalDetails rentalSlab
      return $
        SRB.RideBooking
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
            rideBookingDetails,
            tripTerms = quote.tripTerms,
            createdAt = now,
            updatedAt = now
          }
