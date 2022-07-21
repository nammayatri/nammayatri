module Domain.Action.UI.Init where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Domain.Types.Booking as SRB
import Domain.Types.BookingLocation
import qualified Domain.Types.BookingLocation as DLoc
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as SQuote
import Domain.Types.RentalSlab
import Domain.Types.SearchReqLocation (SearchReqLocation (..))
import Domain.Types.SearchRequest
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchReqLocation as QSRLoc
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SelectedQuote as QSQuote
import Types.Error
import Utils.Common

data InitReq = InitReq
  { quoteId :: Id SQuote.Quote,
    selected :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data InitRes = InitRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    fromLoc :: LatLong,
    toLoc :: Maybe LatLong,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: InitQuoteDetails,
    startTime :: UTCTime,
    bookingId :: Id SRB.Booking,
    bppQuoteId :: Maybe Text
  }
  deriving (Show, Generic)

data InitQuoteDetails = InitOneWayDetails | InitRentalDetails RentalSlabAPIEntity | InitAutoDetails
  deriving (Show, Generic)

data InitHandler q m = InitHandler
  { hBuildBooking :: SearchRequest -> q -> Id BookingLocation -> Maybe (Id BookingLocation) -> UTCTime -> m SRB.Booking,
    hGetBPPQuoteId :: q -> Maybe (Id SQuote.BPPQuote),
    hGetInitQuoteDetails :: q -> m InitQuoteDetails
  }

init :: forall m r. EsqDBFlow m r => Id SP.Person -> InitReq -> m InitRes
init personId_ req_ = do
  if not req_.selected
    then do
      quote_ <- QQuote.findById req_.quoteId >>= fromMaybeM (QuoteDoesNotExist req_.quoteId.getId)

      init' quote_.requestId
        personId_
        quote_
        InitHandler
          { hBuildBooking = buildBookingStatic,
            hGetBPPQuoteId = const Nothing,
            hGetInitQuoteDetails = \q -> case q.quoteDetails of
              SQuote.OneWayDetails _ -> pure InitOneWayDetails
              SQuote.RentalDetails RentalSlab {..} -> pure $ InitRentalDetails $ RentalSlabAPIEntity {..}
              SQuote.AutoDetails -> throwError $ InvalidRequest "Quote should be selected before init"
          }
    else do
      selQuote_ <- QSQuote.findById (cast req_.quoteId) >>= fromMaybeM (SelectedQuoteNotFound req_.quoteId.getId)
      quote_ <- QQuote.findById selQuote_.quoteId >>= fromMaybeM (QuoteDoesNotExist selQuote_.quoteId.getId)
      init' quote_.requestId
        personId_
        selQuote_
        InitHandler
          { hBuildBooking = buildBookingDynamic,
            hGetBPPQuoteId = Just . (.bppQuoteId),
            hGetInitQuoteDetails = \_ -> pure InitAutoDetails
          }
  where
    init' requestId personId quote h = do
      now <- getCurrentTime
      searchRequest <- QSReq.findById requestId >>= fromMaybeM (SearchRequestNotFound requestId.getId)
      when ((searchRequest.validTill) < now) $
        throwError SearchRequestExpired
      unless (searchRequest.riderId == personId) $ throwError AccessDenied
      fromLocation <- QSRLoc.findById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
      mbToLocation :: Maybe SearchReqLocation <- searchRequest.toLocationId `forM` (QSRLoc.findById >=> fromMaybeM LocationNotFound)
      bFromLocation <- buildBLoc fromLocation now
      mbBToLocation <- (`buildBLoc` now) `mapM` mbToLocation
      booking <- h.hBuildBooking searchRequest quote bFromLocation.id (mbBToLocation <&> (.id)) now
      let bppQuoteId = (.getId) <$> h.hGetBPPQuoteId quote
      details <- hGetInitQuoteDetails h quote
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
            quoteDetails = details,
            startTime = searchRequest.startTime,
            bppQuoteId
          }
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
    buildOneWayDetails mbToLocId searchRequest = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocationId <- mbToLocId & fromMaybeM (InternalError "distance is null for rental search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for rental search request")
      pure . SRB.OneWayDetails $ SRB.OneWayBookingDetails {..}

    buildBookingStatic searchRequest quote fromLocId toLocId now =
      buildBooking searchRequest quote fromLocId now $ \q -> case q.quoteDetails of
        SQuote.OneWayDetails _ ->
          buildOneWayDetails toLocId searchRequest
        SQuote.RentalDetails rentalSlab ->
          pure $ SRB.RentalDetails rentalSlab
        SQuote.AutoDetails ->
          throwError $ InvalidRequest "init is not supported for auto trips"

    buildBookingDynamic searchRequest quote fromLocId toLocId now =
      buildBooking searchRequest quote fromLocId now $ \_ ->
        buildOneWayDetails toLocId searchRequest

    buildBooking searchRequest quote fromLocId now buildFunc = do
      id <- generateGUID
      bookingDetails <- buildFunc quote
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
