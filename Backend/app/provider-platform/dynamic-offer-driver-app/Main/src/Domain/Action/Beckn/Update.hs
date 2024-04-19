{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Update where

import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified Beckn.Types.Core.Taxi.Update.UpdateEvent.EditLocationEvent as EditLocationU
import Data.List.NonEmpty (last)
import Data.Maybe
import qualified Data.Text as T
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
import Environment
import EulerHS.Prelude hiding (drop, id, state)
import Kernel.Beam.Functions as B
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.LocationUpdates.Internal
import SharedLogic.CallBAP
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.LocationMapping as SLM
import SharedLogic.TollsDetector
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CTC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as Maps
-- import qualified Kernel.Utils.Time as Time
import qualified Tools.Notifications as Notify

-- import Domain.Action.UI.Ride.EndRide.Internal

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
    origin :: Maybe Common.Location,
    destination :: Maybe Common.Location,
    status :: EditLocationU.UpdateStatus,
    bapBookingUpdateRequestId :: Text
  }

data AddStopReq = AddStopReq
  { bookingId :: Id DBooking.Booking,
    stops :: [Common.Location]
  }

data EditStopReq = EditStopReq
  { bookingId :: Id DBooking.Booking,
    stops :: [Common.Location]
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
  when (ride.status == DRide.COMPLETED) $ throwError $ RideInvalidStatus "Ride is already completed."
  person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  whenJust origin $ \pickup -> do
    startLocation <- buildLocation pickup
    QL.create startLocation
    pickupMapForBooking <- SLM.buildPickUpLocationMapping startLocation.id bookingId.getId DLM.BOOKING (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForBooking
    pickupMapForRide <- SLM.buildPickUpLocationMapping startLocation.id rideId.getId DLM.RIDE (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForRide
    let entityData = Notify.EditLocationReq {..}
    Notify.notifyPickupOrDropLocationChange person.merchantOperatingCityId person.id person.deviceToken entityData

  whenJust destination $ \drop -> do
    --------------------TO DO ----------------------- Dependency on other people changes
    -----------1. Add a check for forward dispatch ride -----------------
    -----------2. Add a check for last location timestamp of driver ----------------- LTS dependency
    booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
    now <- getCurrentTime
    dropLocation <- buildLocation drop
    QL.create dropLocation
    -- let (Common.Gps lat lon) = drop.gps
    let dropLatLong = Maps.LatLong {lat = dropLocation.lat, lon = dropLocation.lon}
    let srcPt = Maps.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
    merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
    case status of
      EditLocationU.SOFT_UPDATE -> do
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
        -- let routePoints = shortestRoute.points
        estimatedDistance <- shortestRoute.distance & fromMaybeM (InternalError "No distance found for new destination")
        (duration :: Seconds) <- shortestRoute.duration & fromMaybeM (InternalError "No duration found for new destination")
        logTagInfo "update Ride soft update" $ "pickedWaypoints: " <> show duration
        -- estimatedRideDuration <- if ride.status == DRide.INPROGRESS then do
        --   now <- getCurrentTime
        --   tripStart <- ride.tripStartTime & fromMaybeM (InternalError "No trip start time found for Inprogress ride")
        --   let estimatedDuration = duration - Time.nominalDiffTimeToSeconds $ Time.diffUTCTime now tripStart ----Need to correct this -----RITIKA
        --   -- let estimatedDuration' = duration - alreadyTravelledDuration
        --   return (Just estimatedDuration)
        --   else return (Just duration)
        fareProducts <- getAllFarePoliciesProduct merchantOperatingCity.merchantId merchantOperatingCity.id srcPt (Just dropLatLong) (Just booking.transactionId) (Just "transactionId") booking.tripCategory
        farePolicy <- getFarePolicy merchantOperatingCity.id booking.tripCategory booking.vehicleServiceTier (Just fareProducts.area) (Just booking.transactionId) (Just "transactionId")
        tollCharges <- getTollChargesOnRoute merchantOperatingCity.id (Just person.id) shortestRoute.points
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
                tollCharges
              }
        QFP.create fareParameters
        transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCity.id (Just person.id.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCity.id.getId)
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
      EditLocationU.CONFIRM_UPDATE -> do
        bookingUpdateReq <- QBUR.findByBAPBUReqId bapBookingUpdateRequestId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with BAPBookingUpdateRequestId" <> bapBookingUpdateRequestId)
        when (bookingUpdateReq.validTill < now) $ throwError (InvalidRequest "BookingUpdateRequest is expired")
        when (bookingUpdateReq.status /= DBUR.SOFT) $ throwError (InvalidRequest "BookingUpdateRequest is not in SOFT state")
        QBUR.updateStatusById DBUR.USER_CONFIRMED bookingUpdateReq.id
        let entityData = Notify.EditLocationReq {..}
        Notify.notifyPickupOrDropLocationChange person.merchantOperatingCityId person.id person.deviceToken entityData

-- sendOverlay merchantOpCityId personId mbDeviceToken req@FCM.FCMOverlayReq {..} = do

--       let entityData = = Notify.FCMOverlayReq
-- { title = Maybe Text,
--   description = Just "Trip Update !",
--   imageUrl = Maybe Text,
--   okButtonText = Just "Aceept & Navigate",
--   cancelButtonText = Just "Decline",
--   actions = ["CALL_API"],
--   link = Maybe Text,
--   endPoint = Just "edit/result/"<>id<>"?action=\"ACCEPT\"",
--   method = Just "POST",
--   reqBody = Null,
--   delay = Maybe Int,
--   contactSupportNumber = Maybe Text,
--   toastMessage = Maybe Text,
--   secondaryActions = Maybe [Text],
--   socialMediaLinks = Maybe [FCMMediaLink],
--   showPushNotification = Just True
-- }
-- deriving (Eq, Show, Generic, PrettyShow, ToSchema, FromJSON)

-- handler _ = throwError (InvalidRequest "Not Implemented")

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

buildLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DL.Location -> Text -> Bool -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> m DLM.LocationMapping
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

processStop :: DBooking.Booking -> Common.Location -> Bool -> Flow ()
processStop booking loc isEdit = do
  validateStopReq booking isEdit
  location <- buildLocation loc
  locationMapping <- buildLocationMapping location.id booking.id.getId isEdit (Just booking.providerId) (Just booking.merchantOperatingCityId)
  QL.create location
  QLM.create locationMapping
  QRB.updateStop booking.id (Just location.id.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  whenJust mbRide $ \ride -> do
    person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    let entityData = Notify.StopReq {bookingId = booking.id, stop = Just (mkLocationAPIEntity loc), ..}
    when (ride.status == DRide.INPROGRESS) $ Notify.notifyStopModification person.merchantOperatingCityId person.id person.deviceToken entityData

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
  let (Money fare) = booking.estimatedFare
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
        oldEstimatedFare = HighPrecMoney $ toRational fare,
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

-- pickWaypoints :: [a] -> [a]
-- pickWaypoints waypoints = do
--   let step = length waypoints `div` 10
--   take 9 $ foldr (\(n, waypoint) list -> if n `safeMod` step == 0 then waypoint : list else list) [] $ zip [1 ..] waypoints

-- safeMod :: Int -> Int -> Int
-- _ `safeMod` 0 = 0
-- a `safeMod` b = a `mod` b
