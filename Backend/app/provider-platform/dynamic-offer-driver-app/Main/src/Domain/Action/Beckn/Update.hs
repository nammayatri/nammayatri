{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Update where

import qualified API.Types.UI.EditBooking as EditBooking
import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified BecknV2.OnDemand.Enums as Enums
import Data.List.NonEmpty (last)
import Data.Maybe
import qualified Data.Text as T
import qualified Domain.Action.UI.EditBooking as EditBooking
import Domain.Action.UI.Ride.EndRide.Internal
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Domain.Types.OnUpdate
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideRoute as RR
import Environment
import EulerHS.Prelude hiding (drop, id, state)
import Kernel.Beam.Functions as B
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.LocationUpdates.Internal
import SharedLogic.CallBAP
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.LocationMapping as SLM
import SharedLogic.Ride
import SharedLogic.TollsDetector
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

data DUpdateReq
  = UPaymentCompletedReq PaymentCompletedReq
  | UEditLocationReq EditLocationReq
  | UAddStopReq AddStopReq
  | UEditStopReq EditStopReq

data PaymentCompletedReq = PaymentCompletedReq
  { bookingId :: Id DBooking.Booking,
    rideId :: Id DRide.Ride,
    paymentStatus :: PaymentStatus,
    paymentMethodInfo :: DMPM.PaymentMethodInfo
  }

data EditLocationReq = EditLocationReq
  { bookingId :: Id DBooking.Booking,
    rideId :: Id DRide.Ride,
    origin :: Maybe DL.Location,
    destination :: Maybe DL.Location,
    status :: Enums.OrderStatus,
    bapBookingUpdateRequestId :: Text
  }

data AddStopReq = AddStopReq
  { bookingId :: Id DBooking.Booking,
    stops :: [DL.Location]
  }

data EditStopReq = EditStopReq
  { bookingId :: Id DBooking.Booking,
    stops :: [DL.Location]
  }

getBookingId :: DUpdateReq -> Id DBooking.Booking
getBookingId (UPaymentCompletedReq req) = req.bookingId
getBookingId (UEditLocationReq req) = req.bookingId
getBookingId (UAddStopReq req) = req.bookingId
getBookingId (UEditStopReq req) = req.bookingId

data PaymentStatus = PAID | NOT_PAID

handler :: DUpdateReq -> Flow ()
handler (UPaymentCompletedReq req@PaymentCompletedReq {}) = do
  unless (req.paymentMethodInfo.paymentType == DMPM.ON_FULFILLMENT) $
    throwError $ InvalidRequest "Payment completed update available only for ON_FULFILLMENT payments."
  unless (req.paymentMethodInfo.collectedBy == DMPM.BAP) $
    throwError $ InvalidRequest "Payment completed update available only when BAP collect payment."
  when (req.paymentMethodInfo.paymentInstrument == DMPM.Cash) $
    throwError $ InvalidRequest "Payment completed update not available for cash"
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  paymentMethodId <- booking.paymentMethodId & fromMaybeM (InvalidRequest "Payment method not specified for this booking.")
  paymentMethod <-
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo paymentMethod
  unless (req.paymentMethodInfo == paymentMethodInfo) $
    throwError (InvalidRequest $ "Invalid payment method info for this booking, should be: " <> show paymentMethodInfo <> ".")
  ride <-
    QRide.findById req.rideId
      >>= fromMaybeM (RideNotFound booking.id.getId)
  unless (ride.status == DRide.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not completed yet."
  logTagInfo "Payment completed : " ("bookingId " <> req.bookingId.getId <> ", rideId " <> req.rideId.getId)
handler (UAddStopReq AddStopReq {..}) = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for booking " <> bookingId.getId)
    Just loc -> processStop booking loc False
handler (UEditStopReq EditStopReq {..}) = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for booking " <> bookingId.getId)
    Just loc -> processStop booking loc True
handler (UEditLocationReq EditLocationReq {..}) = do
  when (isNothing origin && isNothing destination) $
    throwError PickupOrDropLocationNotFound
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  when (ride.status == DRide.COMPLETED || ride.status == DRide.CANCELLED) $ throwError $ RideInvalidStatus "Can't edit destination for completed/cancelled ride."
  person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  whenJust origin $ \startLocation -> do
    QL.create startLocation
    pickupMapForBooking <- SLM.buildPickUpLocationMapping startLocation.id bookingId.getId DLM.BOOKING (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForBooking
    pickupMapForRide <- SLM.buildPickUpLocationMapping startLocation.id rideId.getId DLM.RIDE (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForRide
    let entityData = Notify.EditLocationReq {..}
    Notify.notifyPickupOrDropLocationChange person entityData

  whenJust destination $ \dropLocation -> do
    --------------------TO DO ----------------------- Dependency on other people changes
    -----------1. Add a check for forward dispatch ride -----------------
    -----------2. Add a check for last location timestamp of driver ----------------- LTS dependency
    booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
    transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
    now <- getCurrentTime
    QL.create dropLocation
    let dropLatLong = Maps.LatLong {lat = dropLocation.lat, lon = dropLocation.lon}
    let srcPt = Maps.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
    merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
    case status of
      Enums.SOFT_UPDATE -> do
        (pickedWaypoints, currentPoint) <-
          if ride.status == DRide.INPROGRESS
            then do
              locationPts <- LTS.driverLocation rideId merchantOperatingCity.merchantId ride.driverId
              let (currentPoint :: Maps.LatLong) = last locationPts.loc
              pts <- getInterpolatedPointsImplementation ride.driverId
              return (srcPt :| (pickWaypoints pts ++ [currentPoint, dropLatLong]), Just currentPoint)
            else return (srcPt :| [dropLatLong], Nothing)
        logTagInfo "update Ride soft update" $ "pickedWaypoints: " <> show pickedWaypoints
        routeResponse <-
          Maps.getRoutes merchantOperatingCity.merchantId merchantOperatingCity.id $
            Maps.GetRoutesReq
              { waypoints = pickedWaypoints,
                mode = Just Maps.CAR,
                calcPoints = True
              }
        shortestRoute <- getRouteInfoWithShortestDuration routeResponse & fromMaybeM (InternalError "No route found for new destination")
        let maxEstimatedDist = maybe Nothing (\route -> route.distance) (Maps.getLongestRouteDistance routeResponse)
        estimatedDistance <- shortestRoute.distance & fromMaybeM (InternalError "No distance found for new destination")
        (duration :: Seconds) <- shortestRoute.duration & fromMaybeM (InternalError "No duration found for new destination")
        logTagInfo "update Ride soft update" $ "pickedWaypoints: " <> show duration
        let routeInfo = RR.RouteInfo {distance = Just estimatedDistance, duration = Just duration, points = Just shortestRoute.points}
        Redis.setExp (bookingRequestKeySoftUpdate booking.id.getId) routeInfo 600
        Redis.setExp (multipleRouteKeySoftUpdate booking.id.getId) (map RR.createMultipleRouteInfo routeResponse) 600
        fareProducts <- getAllFarePoliciesProduct merchantOperatingCity.merchantId merchantOperatingCity.id srcPt (Just dropLatLong) (Just (TransactionId (Id booking.transactionId))) booking.tripCategory
        farePolicy <- getFarePolicy merchantOperatingCity.id booking.tripCategory booking.vehicleServiceTier (Just fareProducts.area) (Just (TransactionId (Id booking.transactionId)))
        mbTollInfo <- getTollInfoOnRoute merchantOperatingCity.id (Just person.id) shortestRoute.points
        fareParameters <-
          calculateFareParameters
            CalculateFareParametersParams
              { farePolicy,
                actualDistance = Just estimatedDistance,
                rideTime = booking.startTime,
                waitingTime = Nothing,
                actualRideDuration = Nothing,
                avgSpeedOfVehicle = Nothing,
                driverSelectedFare = booking.fareParams.driverSelectedFare,
                customerExtraFee = booking.fareParams.customerExtraFee,
                nightShiftCharge = booking.fareParams.nightShiftCharge,
                customerCancellationDues = booking.fareParams.customerCancellationDues,
                nightShiftOverlapChecking = False,
                estimatedDistance = Just estimatedDistance,
                estimatedRideDuration = Nothing,
                timeDiffFromUtc = Nothing,
                tollCharges = mbTollInfo <&> (\(tollCharges, _, _) -> tollCharges),
                currency = booking.currency
              }
        QFP.create fareParameters
        let validTill = addUTCTime (fromIntegral transporterConfig.editLocTimeThreshold) now
        bookingUpdateReq <- buildbookingUpdateRequest booking merchantOperatingCity.merchantId bapBookingUpdateRequestId fareParameters farePolicy.id maxEstimatedDist currentPoint estimatedDistance validTill
        QBUR.create bookingUpdateReq
        startLocMapping <- QLM.getLatestStartByEntityId bookingId.getId >>= fromMaybeM (InternalError $ "Latest start location mapping not found for bookingId: " <> bookingId.getId)
        dropLocMapping <- QLM.getLatestEndByEntityId bookingId.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingId: " <> bookingId.getId)
        startLocMap <- SLM.buildPickUpLocationMapping startLocMapping.locationId bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
        dropLocMap <- SLM.buildDropLocationMapping dropLocMapping.locationId bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
        destLocMapNew <- SLM.buildDropLocationMapping dropLocation.id bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
        QLM.create startLocMap
        QLM.create dropLocMap
        QLM.create destLocMapNew
        sendUpdateEditDestToBAP booking ride bookingUpdateReq (Just dropLocation) currentPoint SOFT_UPDATE
      Enums.CONFIRM_UPDATE -> do
        bookingUpdateReq <- QBUR.findByBAPBUReqId bapBookingUpdateRequestId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with BAPBookingUpdateRequestId" <> bapBookingUpdateRequestId)
        when (bookingUpdateReq.validTill < now) $ throwError (InvalidRequest "BookingUpdateRequest is expired")
        when (bookingUpdateReq.status /= DBUR.SOFT) $ throwError (InvalidRequest "BookingUpdateRequest is not in SOFT state")
        QBUR.updateStatusById DBUR.USER_CONFIRMED bookingUpdateReq.id
        if transporterConfig.editLocDriverPermissionNeeded
          then do
            newEstimatedDistance <- bookingUpdateReq.estimatedDistance & fromMaybeM (InternalError $ "No estimated distance found for bookingUpdateReq with Id :" <> bookingUpdateReq.id.getId)
            oldEstimatedDistance <- bookingUpdateReq.oldEstimatedDistance & fromMaybeM (InternalError $ "No estimated distance found for booking with Id :" <> booking.id.getId)
            let entityData =
                  Notify.UpdateLocationNotificationReq
                    { rideId = ride.id,
                      origin = Nothing,
                      destination = Just dropLocation,
                      stops = Nothing,
                      bookingUpdateRequestId = bookingUpdateReq.id,
                      newEstimatedDistance,
                      newEstimatedFare = bookingUpdateReq.estimatedFare,
                      oldEstimatedDistance,
                      oldEstimatedFare = bookingUpdateReq.oldEstimatedFare,
                      validTill = bookingUpdateReq.validTill
                    }
            overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf booking.merchantOperatingCityId "UPDATE_LOC_FCM" ENGLISH Nothing >>= fromMaybeM (InternalError "Overlay not found for UPDATE_LOC_FCM")
            let actions2 = map (mkActions2 bookingUpdateReq.id.getId dropLocation.lat dropLocation.lon) overlay.actions2
            let secondaryActions2 = fmap (map (mkSecondaryActions2 bookingUpdateReq.id.getId)) overlay.secondaryActions2
            let overlay' = overlay{actions2, secondaryActions2}
            Notify.sendUpdateLocOverlay merchantOperatingCity.id person (Notify.mkOverlayReq overlay') entityData
          else void $ EditBooking.postEditResult (Just person.id, merchantOperatingCity.merchantId, merchantOperatingCity.id) bookingUpdateReq.id (EditBooking.EditBookingRespondAPIReq {action = EditBooking.ACCEPT})
      _ -> throwError (InvalidRequest "Invalid status for edit location request")

mkActions2 :: Text -> Double -> Double -> FCM.FCMActions -> FCM.FCMActions
mkActions2 bookingUpdateReqId lat long action = do
  let primaryAction' = case action.primaryAction of
        FCM.CALL_API details -> do
          let ep = T.replace (Notify.templateText "bookingUpdateRequestId") bookingUpdateReqId details.endPoint
          let details' = details{endPoint = ep}
          CALL_API details'
        FCM.NAVIGATE details -> do
          let details' = details{lat, long}
          NAVIGATE details'
        _ -> action.primaryAction
  let dependentActions' = map (mkActions2 bookingUpdateReqId lat long) action.dependentActions
  FCM.FCMActions {primaryAction = primaryAction', dependentActions = dependentActions'}

mkSecondaryActions2 :: Text -> FCM.FCMActions -> FCM.FCMActions
mkSecondaryActions2 bookingUpdateReqId action = do
  let primaryAction' = case action.primaryAction of
        FCM.CALL_API details -> do
          let ep = T.replace (Notify.templateText "bookingUpdateRequestId") bookingUpdateReqId details.endPoint
          let details' = details{endPoint = ep}
          CALL_API details'
        _ -> action.primaryAction
  let dependentActions' = map (mkSecondaryActions2 bookingUpdateReqId) action.dependentActions
  FCM.FCMActions {primaryAction = primaryAction', dependentActions = dependentActions'}

buildLocation :: MonadFlow m => Common.Location -> m DL.Location
buildLocation location = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    DL.Location
      { id = guid,
        createdAt = now,
        updatedAt = now,
        lat = location.gps.lat,
        lon = location.gps.lon,
        address =
          DL.LocationAddress
            { street = location.address.street,
              door = location.address.door,
              city = location.address.city,
              state = location.address.state,
              country = location.address.country,
              building = location.address.building,
              areaCode = location.address.area_code,
              area = location.address.locality,
              fullAddress = mkFullAddress location.address
            }
      }

mkFullAddress :: Common.Address -> Maybe Text
mkFullAddress Common.Address {..} = do
  let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, city, state, area_code, country]
  if null strictFields
    then Nothing
    else Just $ T.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (T.null . T.strip)

buildLocationMapping :: KvDbFlow m r => Id DL.Location -> Text -> Bool -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> m DLM.LocationMapping
buildLocationMapping locationId entityId isEdit merchantId merchantOperatingCityId = do
  id <- generateGUID
  now <- getCurrentTime
  prevOrder <- QLM.maxOrderByEntity entityId
  when isEdit $ QLM.updatePastMappingVersions entityId prevOrder
  let version = QLM.latestTag
      tag = DLM.BOOKING
  return $
    DLM.LocationMapping
      { order = if isEdit then prevOrder else prevOrder + 1,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkLocationAPIEntity :: Common.Location -> DL.LocationAPIEntity
mkLocationAPIEntity location =
  DL.LocationAPIEntity
    { lat = location.gps.lat,
      lon = location.gps.lon,
      street = location.address.street,
      city = location.address.city,
      state = location.address.state,
      country = location.address.country,
      building = location.address.building,
      areaCode = location.address.area_code,
      area = location.address.ward,
      fullAddress = mkFullAddress location.address
    }

processStop :: DBooking.Booking -> DL.Location -> Bool -> Flow ()
processStop booking location isEdit = do
  validateStopReq booking isEdit
  locationMapping <- buildLocationMapping location.id booking.id.getId isEdit (Just booking.providerId) (Just booking.merchantOperatingCityId)
  QL.create location
  QLM.create locationMapping
  QRB.updateStop booking.id (Just location.id.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  whenJust mbRide $ \ride -> do
    person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    let entityData = Notify.StopReq {bookingId = booking.id, stop = Just (DL.makeLocationAPIEntity location), ..}
    when (ride.status == DRide.INPROGRESS) $ Notify.notifyStopModification person entityData

validateStopReq :: DBooking.Booking -> Bool -> Flow ()
validateStopReq booking isEdit = do
  unless (booking.status `elem` [DBooking.NEW, DBooking.TRIP_ASSIGNED]) $ throwError $ BookingInvalidStatus ("Cannot add stop in this booking " <> booking.id.getId) -- check for rental?
  if isEdit
    then unless (isJust booking.stopLocationId) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> booking.id.getId) -- should we throw error or just allow?
    else unless (isNothing booking.stopLocationId) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> booking.id.getId)

buildbookingUpdateRequest :: MonadFlow m => DBooking.Booking -> Id DM.Merchant -> Text -> DFP.FareParameters -> Id DFP.FarePolicy -> Maybe Meters -> Maybe Maps.LatLong -> Meters -> UTCTime -> m DBUR.BookingUpdateRequest
buildbookingUpdateRequest booking merchantId bapBookingUpdateRequestId fareParams farePolicyId maxEstimatedDistance currentPoint estimatedDistance validTill = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    DBUR.BookingUpdateRequest
      { id = guid,
        status = DBUR.SOFT,
        bapBookingUpdateRequestId,
        createdAt = now,
        updatedAt = now,
        bookingId = booking.id,
        merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        currentPointLat = (.lat) <$> currentPoint,
        currentPointLon = (.lon) <$> currentPoint,
        estimatedFare = HighPrecMoney $ toRational $ fareSum fareParams,
        estimatedDistance = Just $ metersToHighPrecMeters estimatedDistance,
        oldEstimatedFare = booking.estimatedFare,
        maxEstimatedDistance = metersToHighPrecMeters <$> maxEstimatedDistance,
        oldEstimatedDistance = metersToHighPrecMeters <$> booking.estimatedDistance,
        totalDistance = Nothing,
        travelledDistance = Nothing,
        fareParamsId = fareParams.id,
        oldFareParamsId = booking.fareParams.id,
        oldMaxEstimatedDistance = booking.maxEstimatedDistance,
        validTill,
        ..
      }
