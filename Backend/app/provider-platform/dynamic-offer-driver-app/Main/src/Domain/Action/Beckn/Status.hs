{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Status
  ( handler,
    DStatusReq (..),
    DStatusRes (..),
  )
where

import qualified BecknV2.OnDemand.Tags as Beckn
import Data.Default.Class
import Data.Either.Extra (eitherToMaybe)
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as Aadhaar
import Domain.Types.Beckn.Status
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions as B
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Beckn.Common as Common
import qualified SharedLogic.SyncRide as SyncRide
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import Tools.Error

handler ::
  Id DM.Merchant ->
  DStatusReq ->
  Flow DStatusRes
handler transporterId req = do
  transporter <-
    CQM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  booking <- B.runInReplica $ QRB.findByTransactionId req.transactionId >>= fromMaybeM (BookingNotFound req.transactionId)
  mbRide <- B.runInReplica $ QRide.findOneByBookingId booking.id
  let transporterId' = booking.providerId
      estimateId = booking.estimateId <&> getId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  info <- case mbRide of
    Just ride -> do
      chargeableDistance :: HighPrecMeters <-
        realToFrac <$> ride.chargeableDistance
          & fromMaybeM (InternalError "Ride chargeable distance is not present in OnUpdateBuildReq ride.")
      case ride.status of
        DRide.UPCOMING -> syncAssignedReq ride booking estimateId
        DRide.NEW -> syncAssignedReq ride booking estimateId
        DRide.INPROGRESS -> do
          bookingDetails <- SyncRide.fetchBookingDetails ride booking
          let tripStartLocation = bookingDetails.ride.tripStartPos
          let arrivalTimeTagGroup = if bookingDetails.isValueAddNP then mkArrivalTimeTagGroupV2 ride.driverArrivalTime else Nothing
          let odometerTag = if bookingDetails.isValueAddNP then mkOdometerTagGroupV2 ((.value) <$> ride.startOdometerReading) else Nothing
          let taggings = Just $ def {Beckn.fulfillmentTags = fromMaybe [] $ arrivalTimeTagGroup <> odometerTag}
          pure $ RideStartedReq DRideStartedReq {..}
        DRide.COMPLETED -> do
          bookingDetails <- SyncRide.fetchBookingDetails ride booking
          SyncRide.RideCompletedInfo {..} <- SyncRide.fetchRideCompletedInfo ride booking
          let traveledDistance :: HighPrecMeters = ride.traveledDistance
          let endOdometerReading :: Maybe Centesimal = (.value) <$> ride.endOdometerReading
          let tripEndLocation = bookingDetails.ride.tripEndPos
          let distanceTagGroup = mkDistanceTagGroup traveledDistance endOdometerReading chargeableDistance
          let arrivalTimeTagGroup = fromMaybe [] $ mkArrivalTimeTagGroupV2 ride.driverArrivalTime
          let tollConfidences = fromMaybe [] $ mkTollConfidenceTagGroupV2 ride.tollConfidence
          let taggings = Just $ def {Beckn.fulfillmentTags = if bookingDetails.isValueAddNP then arrivalTimeTagGroup <> distanceTagGroup <> tollConfidences else []}
          pure $ RideCompletedReq DRideCompletedReq {..}
        DRide.CANCELLED -> do
          case booking.status of
            DBooking.REALLOCATED -> do
              bookingDetails <- SyncRide.fetchBookingDetails ride booking
              bookingReallocationInfo <- SyncRide.fetchBookingReallocationInfo (Just ride) booking
              let taggings = Just $ def {Beckn.fulfillmentTags = fromMaybe [] $ mkArrivalTimeTagGroupV2 ride.driverArrivalTime}
              pure $ BookingReallocationBuildReq DBookingReallocationBuildReq {bookingDetails, bookingReallocationInfo, taggings}
            _ -> do
              bookingDetails <- SyncRide.fetchBookingDetails ride booking
              SyncRide.BookingCancelledInfo {..} <- SyncRide.fetchBookingCancelledInfo (Just ride)
              let arrivalTimeTagGroup = fromMaybe [] $ mkArrivalTimeTagGroupV2 ride.driverArrivalTime
              let taggings = Just $ def {Beckn.fulfillmentTags = if bookingDetails.isValueAddNP then arrivalTimeTagGroup else []}
              pure $ BookingCancelledReq DBookingCancelledReq {bookingDetails = Just bookingDetails, cancellationFee = Nothing, ..}
    Nothing -> do
      case booking.status of
        DBooking.NEW -> do
          pure $ NewBookingBuildReq (DNewBookingBuildReq booking.id)
        DBooking.TRIP_ASSIGNED -> do
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
        DBooking.COMPLETED -> do
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
        DBooking.CANCELLED -> do
          bookingCancelledInfo <- SyncRide.fetchBookingCancelledInfo Nothing
          pure $ BookingCancelledReq DBookingCancelledReq {bookingDetails = Nothing, cancellationFee = Nothing, cancellationSource = bookingCancelledInfo.cancellationSource, taggings = Nothing, ..}
        DBooking.REALLOCATED -> do
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
  pure DStatusRes {transporter, booking, info}
  where
    mkVehicleAgeTagGroupV2 vehicleAge' =
      vehicleAge' <&> \vehicleAge ->
        [ (Beckn.VEHICLE_AGE, Just $ show vehicleAge)
        ]

    mkArrivalTimeTagGroupV2 arrivalTime' =
      arrivalTime' <&> \arrivalTime ->
        [ (Beckn.ARRIVAL_TIME, Just $ show arrivalTime)
        ]

    mkForwardBatchTagGroupV2 previousRideDropLocation' =
      previousRideDropLocation' <&> \previousRideDropLocation ->
        [ (Beckn.PREVIOUS_RIDE_DROP_LOCATION_LAT, Just $ show previousRideDropLocation.lat),
          (Beckn.PREVIOUS_RIDE_DROP_LOCATION_LON, Just $ show previousRideDropLocation.lat)
        ]

    mkOdometerTagGroupV2 startOdometerReading' =
      startOdometerReading' <&> \startOdometerReading ->
        [ (Beckn.START_ODOMETER_READING, Just $ show startOdometerReading)
        ]

    mkTollConfidenceTagGroupV2 tollConfidence' =
      tollConfidence' <&> \tollConfidence ->
        [ (Beckn.TOLL_CONFIDENCE, Just $ show tollConfidence)
        ]

    mkDistanceTagGroup traveledDistance endOdometerReading chargeableDistance =
      [ (Beckn.CHARGEABLE_DISTANCE, Just $ show chargeableDistance),
        (Beckn.TRAVELED_DISTANCE, Just $ show traveledDistance),
        (Beckn.END_ODOMETER_READING, show <$> endOdometerReading)
      ]

    syncAssignedReq ride booking estimateId = do
      bookingDetails <- SyncRide.fetchBookingDetails ride booking
      driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM DriverInfoNotFound
      rideDetails <- runInReplica $ QRideDetails.findById ride.id >>= fromMaybeM (RideNotFound ride.id.getId)
      resp <- try @_ @SomeException (Aadhaar.fetchAndCacheAadhaarImage bookingDetails.driver driverInfo)
      let image = join (eitherToMaybe resp)
      let isDriverBirthDay = False
      let isFreeRide = False
      let driverAccountId = Nothing
      let currentRideDropLocation = if bookingDetails.isValueAddNP then mkForwardBatchTagGroupV2 ride.previousRideTripEndPos else Nothing
      let arrivalTimeTagGroup = if bookingDetails.isValueAddNP then mkArrivalTimeTagGroupV2 ride.driverArrivalTime else Nothing
      let vehicleAgeTagGroup = if bookingDetails.isValueAddNP then mkVehicleAgeTagGroupV2 rideDetails.vehicleAge else Nothing
      let taggings = Just $ def {Beckn.fulfillmentTags = fromMaybe [] $ currentRideDropLocation <> arrivalTimeTagGroup <> vehicleAgeTagGroup}
      pure $ RideAssignedReq DRideAssignedReq {vehicleAge = rideDetails.vehicleAge, ..}
