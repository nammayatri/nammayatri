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
    GetRideInfoRes (..),
    RideInfo (..),
    SetDriverAcceptanceReq (..),
    SetDriverAcceptanceRes,
    NotificationStatus (..),
    DriverResponse (..),
    bookingStatus,
    bookingList,
    bookingCancel,
    getRideInfo,
    setDriverAcceptance,
  )
where

import Data.OpenApi (ToSchema (..))
import Domain.Types.AllocationEvent
import qualified Domain.Types.AllocationEvent as AllocationEvent
import Domain.Types.Booking
import qualified Domain.Types.Booking as SRB
import Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideDetails (RideDetails, getDriverNumber)
import Domain.Types.RideRequest
import qualified Domain.Types.RideRequest as SRideRequest
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverLocation as QDrLoc
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRentalFP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data RideInfo = RideInfo
  { bookingId :: Id SRB.Booking,
    pickupLoc :: BookingLocationAPIEntity,
    dropLoc :: Maybe BookingLocationAPIEntity,
    etaForPickupLoc :: Minutes,
    distanceToPickupLoc :: Meters,
    notificationExpiryTime :: UTCTime,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

newtype SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { response :: NotificationStatus
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type SetDriverAcceptanceRes = APISuccess

bookingStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Id SRB.Booking -> m SRB.BookingAPIEntity
bookingStatus bookingId = do
  booking <- Esq.runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  buildBookingAPIEntity booking

bookingList ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) =>
  SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe SRB.BookingStatus ->
  m BookingListRes
bookingList person mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  rbList <- Esq.runInReplica $ QRB.findAllByMerchant person.merchantId mbLimit mbOffset mbOnlyActive mbBookingStatus
  BookingListRes <$> traverse buildBookingAPIEntity rbList

bookingCancel ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id SRB.Booking ->
  SP.Person ->
  m APISuccess
bookingCancel bookingId admin = do
  let merchantId = admin.merchantId
  org <-
    QM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  rideReq <- buildRideReq (org.subscriberId)
  Esq.runTransaction $ RideRequest.create rideReq
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> bookingCancel : ") (show rideReq)
  return Success
  where
    buildRideReq subscriberId = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        SRideRequest.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = subscriberId,
            createdAt = now,
            _type = SRideRequest.CANCELLATION,
            info = Nothing
          }

getRideInfo ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CoreMetrics m) => Id SRB.Booking -> Id SP.Person -> m GetRideInfoRes
getRideInfo bookingId personId = do
  mbNotification <- Esq.runInReplica $ QNotificationStatus.findActiveNotificationByDriverId driverId bookingId
  case mbNotification of
    Nothing -> return $ GetRideInfoRes Nothing
    Just notification -> do
      let notificationExpiryTime = notification.expiresAt
      booking <- Esq.runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
      driver <- Esq.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      driverLocation <-
        QDrLoc.findById driver.id
          >>= fromMaybeM LocationNotFound
      let fromLocation = booking.fromLocation
      let toLocation = case booking.bookingDetails of
            SRB.OneWayDetails details -> Just details.toLocation
            SRB.RentalDetails _ -> Nothing
      distanceDuration <-
        MapSearch.getDistance booking.providerId
          MapSearch.GetDistanceReq
            { origin = driverLocation,
              destination = fromLocation,
              travelMode = Just MapSearch.CAR
            }
          (dataDecider 300)
      return $
        GetRideInfoRes $
          Just $
            RideInfo
              { bookingId = booking.id,
                pickupLoc = DBLoc.makeBookingLocationAPIEntity fromLocation,
                dropLoc = DBLoc.makeBookingLocationAPIEntity <$> toLocation,
                etaForPickupLoc = secondsToMinutes $ distanceDuration.duration,
                distanceToPickupLoc = distanceDuration.distance,
                notificationExpiryTime = notificationExpiryTime,
                estimatedFare = booking.estimatedFare,
                discount = booking.discount,
                estimatedTotalFare = booking.estimatedTotalFare
              }
  where
    driverId = cast personId

dataDecider :: Seconds -> NonEmpty (Meters, Seconds) -> (Meters, Seconds)
dataDecider threshold (x :| xs) = foldl' decider x xs
  where
    decider (distance1, duration1) (distance2, duration2)
      | distance1 > distance2 =
        if duration1 < duration2 && duration2 - duration1 < threshold
          then -- first longer but faster within threshold
            (distance1, duration1)
          else -- second shorter and faster or threshold passed
            (distance2, duration2)
      | distance1 < distance2 =
        if duration1 > duration2 && duration1 - duration2 < threshold
          then -- second longer but faster within threshold
            (distance2, duration2)
          else -- first shorter and faster or threshold passed
            (distance1, duration1)
      | otherwise = (distance1, min duration1 duration2)

responseToEventType :: NotificationStatus -> AllocationEventType
responseToEventType ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SRB.Booking -> Id SP.Person -> SetDriverAcceptanceReq -> m SetDriverAcceptanceRes
setDriverAcceptance bookingId personId req = do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  booking <-
    QRB.findById bookingId
      >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <-
    QM.findById booking.providerId
      >>= fromMaybeM (MerchantDoesNotExist booking.providerId.getId)
  guid <- generateGUID
  let driverResponse =
        DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = merchant.subscriberId,
            createdAt = currentTime,
            _type = DRIVER_RESPONSE,
            info = Just driverResponse
          }
  Esq.runTransaction $ do
    RideRequest.create rideRequest
    AllocationEvent.logAllocationEvent
      (responseToEventType response)
      bookingId
      (Just driverId)
  pure Success
  where
    response = req.response
    driverId = cast personId
    logMessage =
      "beckn:" <> bookingId.getId <> ":"
        <> getId driverId
        <> ":response"
        <> " "
        <> show response

buildRideAPIEntity :: EncFlow m r => (DRide.Ride, RideDetails) -> m DRide.RideAPIEntity
buildRideAPIEntity (ride, rideDetails) = do
  driverNumber <- getDriverNumber rideDetails
  return $
    DRide.RideAPIEntity
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        driverName = rideDetails.driverName,
        driverNumber = driverNumber,
        vehicleNumber = rideDetails.vehicleNumber,
        vehicleColor = fromMaybe "N/A" rideDetails.vehicleColor,
        vehicleVariant = fromMaybe DVeh.SEDAN rideDetails.vehicleVariant,
        vehicleModel = fromMaybe "N/A" rideDetails.vehicleModel,
        computedFare = ride.fare,
        computedTotalFare = ride.totalFare,
        actualRideDistance = roundToIntegral ride.traveledDistance,
        rideRating = ride.rideRating <&> (.ratingValue),
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt,
        chargeableDistance = ride.chargeableDistance
      }

buildBookingAPIDetails :: (CacheFlow m r, EsqDBFlow m r) => BookingDetails -> m (BookingDetailsAPIEntity, [Text])
buildBookingAPIDetails = \case
  OneWayDetails OneWayBookingDetails {..} -> do
    let details =
          OneWayDetailsAPIEntity
            OneWayBookingDetailsAPIEntity
              { toLocation = makeBookingLocationAPIEntity toLocation
              }
    pure (details, [])
  RentalDetails (RentalBookingDetails rentalFarePolicyId) -> do
    DRentalFP.RentalFarePolicy {..} <-
      QRentalFP.findById rentalFarePolicyId
        >>= fromMaybeM NoRentalFarePolicy
    let details = RentalDetailsAPIEntity RentalBookingDetailsAPIEntity {..}
    pure (details, descriptions)

buildBookingAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Booking -> m BookingAPIEntity
buildBookingAPIEntity booking = do
  let rbStatus = booking.status
  rideList <- mapM buildRideAPIEntity =<< Esq.runInReplica (QRide.findAllRideAPIEntityDataByRBId booking.id)
  fareBreakups <- Esq.runInReplica $ QFareBreakup.findAllByBookingId booking.id
  (bookingDetails, tripTerms) <- buildBookingAPIDetails booking.bookingDetails
  return $ makeBookingAPIEntity booking rbStatus rideList fareBreakups bookingDetails tripTerms
