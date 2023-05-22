module SharedLogic.SimulatedFlow.Booking where

import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Booking
import SharedLogic.Types.Booking.API
import qualified SharedLogic.Types.Booking.API as SRB
import qualified SharedLogic.Types.Booking.BookingLocation as DBL
import qualified SharedLogic.Types.Booking.BookingLocation as SLoc
import SharedLogic.Types.Booking.Type
import qualified SharedLogic.Types.Booking.Type as SRB
import qualified SharedLogic.Types.Person as Person
import qualified SharedLogic.Types.Ride as Ride
import qualified SharedLogic.Types.SearchRequest.SearchReqLocation as SSRL
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.SimulatedFlow.Driver as CSD
import Storage.CachedQueries.SimulatedFlow.SearchRequest
import qualified Storage.CachedQueries.SimulatedFlow.SearchRequest as CSR
import Tools.Error

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatusSimulator ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Bool -> Id Booking -> Id Person.Person -> m BookingAPIEntity -> m BookingAPIEntity
bookingStatusSimulator isSimulated bookingId personId nonSimulatedAction =
  if isSimulated
    then do
      now <- getCurrentTime
      CSR.linkBookingWithPerson bookingId personId
      createSimuatedReponse now bookingId
    else nonSimulatedAction

bookingListSimulator :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Bool -> Maybe BookingStatus -> Id Person.Person -> m BookingListRes -> m BookingListRes
bookingListSimulator isSimulated mbBookingStatus personId nonSimulatedAction =
  if isSimulated
    then do
      createBookingList mbBookingStatus personId
    else nonSimulatedAction

mkSimulatedBookingAPIDetails :: SSRL.SearchReqLocation -> HighPrecMeters -> BookingAPIDetails
mkSimulatedBookingAPIDetails toLocation distance = OneWayAPIDetails $ do
  OneWayBookingAPIDetails
    { toLocation = SLoc.makeSimulatedBookingLocationAPIEntity toLocation,
      estimatedDistance = distance
    }

createSimuatedReponse :: (MonadFlow m, SimluatedCacheFlow m r) => UTCTime -> Id Booking -> m BookingAPIEntity
createSimuatedReponse now bookingId = do
  status <- getLinkBookingStatusBooking bookingId
  let bookingLocation =
        DBL.BookingLocationAPIEntity
          { lat = 0,
            lon = 0,
            street = Nothing,
            door = Nothing,
            city = Nothing,
            state = Nothing,
            country = Nothing,
            building = Nothing,
            areaCode = Nothing,
            area = Nothing,
            ward = Nothing,
            placeId = Nothing
          }
      bookingDetails =
        DriverOfferAPIDetails $
          OneWayBookingAPIDetails
            { toLocation = bookingLocation,
              estimatedDistance = HighPrecMeters 0
            }
  return
    BookingAPIEntity
      { id = bookingId,
        status = if isNothing status then TRIP_ASSIGNED else CANCELLED,
        agencyName = "",
        agencyNumber = "",
        estimatedFare = Money 0,
        discount = Nothing,
        estimatedTotalFare = Money 0,
        fromLocation = bookingLocation,
        rideList = [],
        tripTerms = [],
        fareBreakup = [],
        bookingDetails = bookingDetails,
        rideStartTime = Nothing,
        rideEndTime = Nothing,
        duration = Nothing,
        merchantExoPhone = "0000000000",
        createdAt = now,
        updatedAt = now
      }

createBookingList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Maybe SRB.BookingStatus -> Id Person.Person -> m BookingListRes
createBookingList mbBookingStatus personId = do
  now <- getCurrentTime
  bookingId <- CSR.getBookingIdByPersonId personId >>= fromMaybeM (InvalidRequest $ "SimulatedFlow:Booking: bookingId not found for personId: " <> personId.getId)
  quoteId <- CSR.getQuoteIdByBookingId bookingId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: quoteId not found")
  estimateId <- CSR.getEstimateIdByQuoteId quoteId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: estimateId not found")
  estimate <- CSR.getEstimateById estimateId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: estimate not found")
  searchReqId <- CSR.getSearchRequestIdByEstimateId estimateId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: searchReqId not found")
  searchReq <- CSR.getSearchRequestById searchReqId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: searchReq not found")
  driverInfo <- CSD.getLinkedDriverByEstimateId estimateId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: driver not found")
  toLocation <- searchReq.toLocation & fromMaybeM (InvalidRequest "SimulatedFlow:Booking: toLocaiton not found")
  shortId <- generateShortId
  guid <- generateGUID
  CSR.linkBookingStatusBooking SRB.TRIP_ASSIGNED bookingId
  CSR.linkBookingIdWithRideId bookingId guid
  bpguid <- generateGUID
  otp <- generateOTPCode
  let simulatedRide =
        Ride.RideAPIEntity
          { id = guid,
            shortRideId = shortId,
            status = Ride.NEW,
            driverName = driverInfo.driver.driverName,
            driverNumber = Just driverInfo.driver.driverNumber,
            driverRatings = driverInfo.driver.driverRating,
            driverRegisteredAt = now,
            rideOtp = otp,
            vehicleNumber = driverInfo.driver.vehicelNumber,
            vehicleColor = "Black",
            vehicleModel = "Bajaj",
            computedPrice = Just estimate.estimatedFare,
            chargeableRideDistance = searchReq.distance,
            driverArrivalTime = Nothing,
            rideStartTime = Nothing,
            rideEndTime = Nothing,
            rideRating = Nothing,
            createdAt = now,
            updatedAt = now,
            bppRideId = bpguid,
            vehicleVariant = estimate.vehicleVariant
          }

  pure . BookingListRes . (: []) $
    SRB.BookingAPIEntity
      { id = bookingId,
        status = fromMaybe SRB.TRIP_ASSIGNED mbBookingStatus,
        agencyName = "",
        agencyNumber = "",
        estimatedFare = estimate.estimatedFare,
        discount = Nothing,
        estimatedTotalFare = estimate.estimatedTotalFare,
        fromLocation = DBL.makeSimulatedBookingLocationAPIEntity searchReq.fromLocation,
        rideList = [simulatedRide],
        tripTerms = [],
        fareBreakup = [], -- FareBreakupAPIEntity
        bookingDetails = mkSimulatedBookingAPIDetails toLocation $ fromMaybe 1200 searchReq.distance,
        rideStartTime = Nothing,
        rideEndTime = Nothing,
        duration = Nothing,
        merchantExoPhone = "0000000000",
        createdAt = now,
        updatedAt = now
      }
