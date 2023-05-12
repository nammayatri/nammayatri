{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Booking
  ( BookingListRes (..),
    bookingStatus,
    bookingList,
  )
where

import Data.OpenApi (ToSchema (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.API as DBA
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow, SimluatedCacheFlow)
import qualified Storage.CachedQueries.SimulatedFlow.Driver as CSD
import qualified Storage.CachedQueries.SimulatedFlow.SearchRequest as CSR
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import Tools.Error

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Id SRB.Booking -> Id Person.Person -> m SRB.BookingAPIEntity
bookingStatus bookingId personId = do
  person <- runInReplica (QP.findById personId) >>= fromMaybeM (PersonNotFound personId.getId)
  if person.isSimulated
    then do
      now <- getCurrentTime
      CSR.linkBookingWithPerson bookingId personId
      pure $ createSimuatedReponse now bookingId
    else do
      booking <- runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      unless (booking.riderId == personId) $ throwError AccessDenied
      SRB.buildBookingAPIEntity booking

bookingList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> m BookingListRes
bookingList personId mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  person <- runInReplica (QP.findById personId) >>= fromMaybeM (PersonNotFound personId.getId)
  if person.isSimulated
    then do
      createBookingList mbBookingStatus personId
    else do
      rbList <- runInReplica $ QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus
      BookingListRes <$> traverse SRB.buildBookingAPIEntity rbList

createBookingList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Maybe SRB.BookingStatus -> Id Person.Person -> m BookingListRes
createBookingList mbBookingStatus personId = do
  now <- getCurrentTime
  bookingId <- CSR.getBookingIdByPersonId personId >>= fromMaybeM (InvalidRequest $ "SimulatedFlow:Booking: bookingId not found for personId: " <> personId.getId)
  quoteId <- CSR.getQuoteIdByBookingId bookingId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: quoteId not found")
  estimateId <- CSR.getEstimateIdByQuoteId quoteId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: estimateId not found")
  estimate <- CSR.getEstimateById estimateId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: estimate not found")
  searchReqId <- CSR.getSearchRequestIdByEstimateId estimateId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: searchReqId not found")
  searchReq <- CSR.getSearchRequestById searchReqId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: searchReq not found")
  driverInfo <- CSD.getLinkedDriverByQuoteId quoteId >>= fromMaybeM (InvalidRequest "SimulatedFlow:Booking: driver not found")
  toLocation <- searchReq.toLocation & fromMaybeM (InvalidRequest "SimulatedFlow:Booking: toLocaiton not found")
  shortId <- generateShortId
  guid <- generateGUID
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
    DBA.BookingAPIEntity
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
        bookingDetails = SRB.mkSimulatedBookingAPIDetails toLocation $ fromMaybe 1200 searchReq.distance,
        rideStartTime = Nothing,
        rideEndTime = Nothing,
        duration = Nothing,
        merchantExoPhone = "0000000000",
        createdAt = now,
        updatedAt = now
      }

createSimuatedReponse :: UTCTime -> Id SRB.Booking -> SRB.BookingAPIEntity
createSimuatedReponse now bookingId = do
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
        DBA.DriverOfferAPIDetails $
          DBA.OneWayBookingAPIDetails
            { toLocation = bookingLocation,
              estimatedDistance = HighPrecMeters 0
            }
  SRB.BookingAPIEntity
    { id = bookingId,
      status = SRB.TRIP_ASSIGNED,
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
