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

data ConfirmReq = ConfirmReq
  { selected :: Bool,
    fromLocation :: ConfirmLocationReq,
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
    bookingId :: Id SRB.Booking,
    bppQuoteId :: Maybe Text
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails = ConfirmOneWayDetails | ConfirmRentalDetails RentalSlabAPIEntity | ConfirmAutoDetails
  deriving (Show, Generic)

data ConfirmHandler q m = ConfirmHandler
  { hBuildBooking :: SearchRequest -> q -> Id BookingLocation -> Maybe (Id BookingLocation) -> UTCTime -> m SRB.Booking,
    hGetBPPQuoteId :: q -> Maybe (Id SQuote.BPPQuote),
    hGetConfirmQuoteDetails :: q -> m ConfirmQuoteDetails
  }

confirm :: forall m r. (EsqDBFlow m r) => Id SP.Person -> Id SQuote.Quote -> ConfirmReq -> m ConfirmRes
confirm personId_ quoteId req_ = do
  if not req_.selected
    then do
      quote_ <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)

      confirm' quote_.requestId
        personId_
        quote_
        ConfirmHandler
          { hBuildBooking = buildBookingStatic,
            hGetBPPQuoteId = const Nothing,
            hGetConfirmQuoteDetails = \q -> case q.quoteDetails of
              SQuote.OneWayDetails _ -> pure ConfirmOneWayDetails
              SQuote.RentalDetails RentalSlab {..} -> pure $ ConfirmRentalDetails $ RentalSlabAPIEntity {..}
              SQuote.AutoDetails -> throwError $ InvalidRequest "Quote should be selected before confirm"
          }
    else do
      selQuote_ <- QSQuote.findById (cast quoteId) >>= fromMaybeM (SelectedQuoteNotFound quoteId.getId)
      quote_ <- QQuote.findById selQuote_.quoteId >>= fromMaybeM (QuoteDoesNotExist selQuote_.quoteId.getId)
      confirm' quote_.requestId
        personId_
        selQuote_
        ConfirmHandler
          { hBuildBooking = buildBookingDynamic,
            hGetBPPQuoteId = Just . (.bppQuoteId),
            hGetConfirmQuoteDetails = \_ -> pure ConfirmAutoDetails
          }
  where
    confirm' ::
      ( EsqDBFlow m r,
        HasField "providerId" r2 Text,
        HasField "providerUrl" r2 BaseUrl,
        HasField "vehicleVariant" r2 VehicleVariant
      ) =>
      Id SearchRequest ->
      Id SP.Person ->
      r2 ->
      ConfirmHandler r2 m ->
      m ConfirmRes
    confirm' requestId personId quote h = do
      now <- getCurrentTime
      searchRequest <- QSReq.findById requestId >>= fromMaybeM (SearchRequestNotFound requestId.getId)
      when ((searchRequest.validTill) < now) $
        throwError SearchRequestExpired
      unless (searchRequest.riderId == personId) $ throwError AccessDenied
      fromLocation <- QSRLoc.findById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
      mbToLocation :: Maybe SearchReqLocation <- searchRequest.toLocationId `forM` (QSRLoc.findById >=> fromMaybeM LocationNotFound)
      bFromLocation <- buildBLoc (fromLocation, req_.fromLocation) now
      mbBToLocation <- (`buildBLoc` now) `mapM` ((,) <$> mbToLocation <*> req_.toLocation)
      booking <- h.hBuildBooking searchRequest quote bFromLocation.id (mbBToLocation <&> (.id)) now
      let bppQuoteId = (.getId) <$> h.hGetBPPQuoteId quote
      details <- hGetConfirmQuoteDetails h quote
      DB.runTransaction $ do
        QBLoc.create bFromLocation
        whenJust mbBToLocation $ \loc -> QBLoc.create loc
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
            startTime = searchRequest.startTime,
            bppQuoteId
          }
    buildBLoc (searchReqLocation, ConfirmLocationReq {..}) now = do
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
          throwError $ InvalidRequest "confirm is not supported for auto trips"

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
