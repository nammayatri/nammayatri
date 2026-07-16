module Domain.Action.UI.EditBooking where

import API.Types.UI.EditBooking
import qualified Data.Geohash as DG
import Data.Text as T
import qualified Domain.Types.Booking
import Domain.Types.BookingUpdateRequest
import qualified Domain.Types.Location
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.OnUpdate as OU
import qualified Domain.Types.Person
import Domain.Types.RideRoute
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Lib.LocationUpdates.Internal (updatePassedThroughDrop)
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.LocationMapping as SLM
import SharedLogic.Ride
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as SQP
import qualified Storage.Queries.QueriesExtra.SearchRequestLite as QSRLite
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Notifications as Notify

postEditResult ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    API.Types.UI.EditBooking.EditBookingRespondAPIReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postEditResult (mbPersonId, _, _) bookingUpdateReqId EditBookingRespondAPIReq {..} = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  now <- getCurrentTime
  bookingUpdateReq <- B.runInReplica $ QBUR.findById bookingUpdateReqId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with id:" <> bookingUpdateReqId.getId)
  when (bookingUpdateReq.status /= USER_CONFIRMED) $ throwError $ InvalidRequest "BookingUpdateRequest is not in USER_CONFIRMED state"
  when (bookingUpdateReq.validTill < now) $ throwError $ InvalidRequest "BookingUpdateRequest is expired"
  booking <- QB.findById bookingUpdateReq.bookingId >>= fromMaybeM (BookingDoesNotExist bookingUpdateReq.bookingId.getId)
  lockEditDestination <- Redis.tryLockRedis (editDestinationLockKey driverId) 20
  if action == ACCEPT && lockEditDestination
    then do
      hasAdvancedRide <- QDI.findById driverId <&> maybe False (.hasAdvanceBooking)
      if hasAdvancedRide
        then do
          CallBAP.sendUpdateEditDestErrToBAP booking bookingUpdateReq.bapBookingUpdateRequestId "Trip Update Request Not Available" "Driver has an upcoming ride near your drop location. "
          throwError $ InvalidRequest "You have an upcoming ride near your current drop location."
        else do
          QBUR.updateStatusById DRIVER_ACCEPTED bookingUpdateReqId
          ride <- QR.findActiveByRBId bookingUpdateReq.bookingId >>= fromMaybeM (InternalError $ "Ride not found for bookingId: " <> bookingUpdateReq.bookingId.getId)
          QB.updateIsPickupOrDestinationEdited (Just True) booking.id
          fork "updateIsReallocationEnabled" $ do
            searchReq <- QSRLite.findByTransactionIdAndMerchantIdLite booking.transactionId bookingUpdateReq.merchantId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> booking.transactionId <> ",merchantId-" <> bookingUpdateReq.merchantId.getId)
            QSR.updateIsReallocationEnabled (Just False) searchReq.id
          dropLocMapping <- QLM.getLatestEndByEntityId bookingUpdateReqId.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingUpdateReqId: " <> bookingUpdateReqId.getId)
          mbDropLocation <- QL.findById dropLocMapping.locationId
          whenJust mbDropLocation $ \dl -> do
            let dropLatLong = LatLong {lat = dl.lat, lon = dl.lon}
            QDI.updateTripEndLocation (Just dropLatLong) driverId
            updatePassedThroughDrop driverId
          prevOrder <- QLM.maxOrderByEntity bookingUpdateReq.bookingId.getId
          dropLocMapBooking <- SLM.buildLocationMapping' dropLocMapping.locationId bookingUpdateReq.bookingId.getId DLM.BOOKING (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId) prevOrder
          prevOrderforRide <- QLM.maxOrderByEntity ride.id.getId
          dropLocMapRide <- SLM.buildLocationMapping' dropLocMapping.locationId ride.id.getId DLM.RIDE (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId) prevOrderforRide
          QLM.create dropLocMapBooking
          QLM.create dropLocMapRide
          routeInfo :: RouteInfo <- Redis.runInMultiCloudRedisMaybeResult (Redis.get (bookingRequestKeySoftUpdate booking.id.getId)) >>= fromMaybeM (InternalError $ "BookingRequestRoute not found for bookingId: " <> booking.id.getId)
          multipleRoutes :: Maybe [RouteAndDeviationInfo] <- Redis.runInMultiCloudRedisMaybeResult $ Redis.safeGet $ multipleRouteKeySoftUpdate booking.id.getId
          Redis.setExp (searchRequestKey booking.transactionId) routeInfo 3600
          whenJust multipleRoutes $ \allRoutes -> do
            Redis.setExp (multipleRouteKey booking.transactionId) allRoutes 3600
          let estimatedDistance = highPrecMetersToMeters <$> bookingUpdateReq.estimatedDistance
          QB.updateMultipleById bookingUpdateReq.estimatedFare bookingUpdateReq.maxEstimatedDistance estimatedDistance bookingUpdateReq.fareParamsId.getId bookingUpdateReq.bookingId
          recomputeRideFinancialsForFareUpdate booking ride bookingUpdateReq.fareParamsId bookingUpdateReq.estimatedFare
          updatedBooking <- QB.findById bookingUpdateReq.bookingId >>= fromMaybeM (BookingDoesNotExist bookingUpdateReq.bookingId.getId)
          CallBAP.sendUpdateEditDestToBAP updatedBooking (Just ride) bookingUpdateReq Nothing Nothing OU.CONFIRM_UPDATE
          void $ Redis.unlockRedis (editDestinationLockKey driverId)
          driver <- SQP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
          mbUpdatedloc <- QL.findById dropLocMapRide.locationId
          whenJust mbUpdatedloc $ \updatedloc -> do
            let (lat, lon) = (updatedloc.lat, updatedloc.lon)
            let mbGeohash = T.pack <$> DG.encode 9 (lat, lon)
            whenJust mbGeohash $ \geohash -> do
              Redis.setExp (editDestinationUpdatedLocGeohashKey driverId) geohash (2 * 60 * 60)
          Notify.notifyEditDestination ride.merchantOperatingCityId driverId driver.deviceToken -- when ride.status in [INPROGRESS, NEW]
          return Success
    else do
      let (errorType, errorMessage) = errorMessageByBookingType lockEditDestination
      CallBAP.sendUpdateEditDestErrToBAP booking bookingUpdateReq.bapBookingUpdateRequestId errorType errorMessage
      QBUR.updateStatusById DRIVER_REJECTED bookingUpdateReqId
      return Success
  where
    errorMessageByBookingType :: Bool -> (Text, Text)
    errorMessageByBookingType isLockAquired = do
      if isLockAquired && action /= ACCEPT
        then ("Trip Update Request Declined", "Request was declined by your driver. Kindly check with them offline before requesting again.")
        else ("Trip Update Request Not Available", "Edit Destination is not possible at this moment. Please try again later.")

-- Skips all driver-coupled work (ride mapping, driver FCM, geohash, notifyEditDestination) since there is no Ride.
postEditResultNoRide ::
  Domain.Types.Booking.Booking ->
  Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
  Domain.Types.Location.Location ->
  Environment.Flow ()
postEditResultNoRide booking bookingUpdateReq dropLocation = do
  QBUR.updateStatusById DRIVER_ACCEPTED bookingUpdateReq.id
  QB.updateIsPickupOrDestinationEdited (Just True) booking.id
  routeInfo :: RouteInfo <- Redis.runInMultiCloudRedisMaybeResult (Redis.get (bookingRequestKeySoftUpdate booking.id.getId)) >>= fromMaybeM (InternalError $ "BookingRequestRoute not found for bookingId: " <> booking.id.getId)
  multipleRoutes :: Kernel.Prelude.Maybe [RouteAndDeviationInfo] <- Redis.runInMultiCloudRedisMaybeResult $ Redis.safeGet $ multipleRouteKeySoftUpdate booking.id.getId
  Redis.setExp (searchRequestKey booking.transactionId) routeInfo 3600
  whenJust multipleRoutes $ \allRoutes ->
    Redis.setExp (multipleRouteKey booking.transactionId) allRoutes 3600
  burDropLocMapping <- QLM.getLatestEndByEntityId bookingUpdateReq.id.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingUpdateReqId: " <> bookingUpdateReq.id.getId)
  prevOrder <- QLM.maxOrderByEntity booking.id.getId
  dropLocMapBooking <- SLM.buildLocationMapping' burDropLocMapping.locationId booking.id.getId DLM.BOOKING (Just booking.providerId) (Just booking.merchantOperatingCityId) prevOrder
  QLM.create dropLocMapBooking
  fork "updateIsReallocationEnabled" $ do
    searchReq <- QSRLite.findByTransactionIdAndMerchantIdLite booking.transactionId booking.providerId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> booking.transactionId <> ",merchantId-" <> booking.providerId.getId)
    QSR.updateIsReallocationEnabled (Just False) searchReq.id
  let estimatedDistance = highPrecMetersToMeters <$> bookingUpdateReq.estimatedDistance
  QB.updateMultipleById bookingUpdateReq.estimatedFare bookingUpdateReq.maxEstimatedDistance estimatedDistance bookingUpdateReq.fareParamsId.getId bookingUpdateReq.bookingId
  updatedBooking <- QB.findById bookingUpdateReq.bookingId >>= fromMaybeM (BookingDoesNotExist bookingUpdateReq.bookingId.getId)
  CallBAP.sendUpdateEditDestToBAP updatedBooking Nothing bookingUpdateReq (Just dropLocation) Nothing OU.CONFIRM_UPDATE
