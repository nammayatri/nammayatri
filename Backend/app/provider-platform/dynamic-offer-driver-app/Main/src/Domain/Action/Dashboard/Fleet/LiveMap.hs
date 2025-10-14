module Domain.Action.Dashboard.Fleet.LiveMap (getLiveMapDrivers) where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as ATD
import qualified API.Types.ProviderPlatform.Fleet.LiveMap as Common
import Domain.Action.Dashboard.Common (mobileIndianCode)
import Domain.Action.Dashboard.Fleet.Driver (validateRequestorRoleAndGetEntityId)
import Domain.Action.UI.Invoice (getSourceAndDestination, notAvailableText)
import Domain.Action.UI.Person (getPersonNumber)
import qualified Domain.Types.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleVariant as DV
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (Meters (..))
import Kernel.Types.Error
  ( GenericError (InternalError),
    PersonError (PersonDoesNotExist, PersonNotFound),
    RideError (RideNotFound),
  )
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Logging (logError)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.WMB (getDriverCurrentLocation)
import qualified Storage.Clickhouse.Booking as CHB
import Storage.Queries.DriverInformationExtra (findAllByDriverIds)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.Vehicle as QV
import Tools.Error (GenericError (InvalidRequest))

getLiveMapDrivers ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Meters ->
  Text ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe (ID.Id ATD.Driver) ->
  Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong ->
  Environment.Flow [Common.MapDriverInfoRes]
getLiveMapDrivers merchantShortId _opCity radius requestorId mbReqFleetOwnerId mbDriverIdForRadius mbPoint = do
  when (radius.getMeters <= 0) . throwError $ InvalidRequest "Radius must be positive"
  latLong <- getPoint mbDriverIdForRadius mbPoint
  checkLatLong latLong
  requestedPerson <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbReqFleetOwnerId
  (mbFleetOwnerId, mbOperatorId) <- case entityRole of
    DP.FLEET_OWNER -> pure (Just entityId, Nothing)
    DP.OPERATOR -> pure (mbReqFleetOwnerId, Just entityId)
    _ -> throwError (InvalidRequest "Invalid Data")
  merchant <- findMerchantByShortId merchantShortId
  filtredNearbyDriverLocations <- LF.nearBy latLong.lat latLong.lon Nothing (Just autoTypeLs) radius.getMeters merchant.id mbFleetOwnerId mbOperatorId
  driverInfoList <- findAllByDriverIds $ (.driverId.getId) <$> filtredNearbyDriverLocations
  let mbPositionAndDriverInfoLs = mkTuple <$> filtredNearbyDriverLocations
      driverIdDriverInfoMap = fromList [(di.driverId, di) | di <- driverInfoList]
      mkTuple location =
        let mbRideId = (.rideId) <$> location.rideDetails
            position = LatLong location.lat location.lon
            mbDriverInfo = Kernel.Prelude.lookup location.driverId driverIdDriverInfoMap
         in (mbRideId,position,) <$> mbDriverInfo
  catMaybes <$> mapM (maybe (pure Nothing) buildMapDriverInfo) mbPositionAndDriverInfoLs
  where
    getPoint :: Kernel.Prelude.Maybe (ID.Id ATD.Driver) -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Environment.Flow LatLong
    getPoint (Just driverIdForRadius) _ = getDriverCurrentLocation $ ID.cast driverIdForRadius
    getPoint _ (Just point) = pure point
    getPoint _ _ = throwError $ InvalidRequest "Either driverIdForRadius or point coordinates must be provided"

    checkLatLong :: LatLong -> Environment.Flow ()
    checkLatLong (LatLong lat lon) = do
      when (lat < -90.0 || lat > 90.0) . throwError $
        InvalidRequest "Latitude must be between -90 and 90 degrees"
      when (lon < -180.0 || lon > 180.0) . throwError $
        InvalidRequest "Longitude must be between -180 and 180 degrees"

    autoTypeLs =
      [ DV.SUV,
        DV.AUTO_RICKSHAW,
        DV.HATCHBACK,
        DV.SEDAN,
        DV.TAXI,
        DV.TAXI_PLUS,
        DV.PREMIUM_SEDAN,
        DV.BLACK,
        DV.BLACK_XL,
        DV.SUV_PLUS
      ]

buildRideRelatedInfo ::
  Bool ->
  Kernel.Prelude.Maybe Text ->
  Environment.Flow Common.RideRelatedInfo
buildRideRelatedInfo True (Just rideId) = do
  ride <- QR.findById (ID.Id rideId) >>= fromMaybeM (RideNotFound rideId)
  mbBooking <- CHB.findById ride.bookingId
  (source, destination) <- getSourceAndDestination mbBooking
  pure
    Common.RideRelatedInfo
      { tripStartTime = ride.tripStartTime,
        fare = ride.fare,
        source,
        destination
      }
buildRideRelatedInfo _ _ = pure $ Common.RideRelatedInfo Nothing Nothing notAvailableText notAvailableText

buildMapDriverInfo ::
  (Kernel.Prelude.Maybe Text, LatLong, DDI.DriverInformation) ->
  Environment.Flow (Kernel.Prelude.Maybe Common.MapDriverInfoRes)
buildMapDriverInfo (mbRideId, position, driverInformation) = do
  let driverId = driverInformation.driverId
  mbVehicle <- QV.findById driverId
  whenNothing_ mbVehicle . logError $ "Vehicle not found for driverId: " <> driverId.getId
  forM mbVehicle $ \vehicle -> do
    driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    rideRelatedInfo <- buildRideRelatedInfo driverInformation.onRide mbRideId
    mobileNumber <-
      getPersonNumber driver
        >>= fromMaybeM (InternalError $ "Driver mobile number is not present. DriverId: " <> driver.id.getId)
    pure
      Common.MapDriverInfoRes
        { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
          driverStatus = castStatus <$> driverInformation.driverFlowStatus,
          vehicleNumber = vehicle.registrationNo,
          vehicleVariant = vehicle.variant,
          position,
          mobileCountryCode = fromMaybe mobileIndianCode driver.mobileCountryCode,
          mobileNumber,
          rideRelatedInfo
        }

castStatus :: DDFS.DriverFlowStatus -> Common.Status
castStatus = \case
  DDFS.ONLINE -> Common.ONLINE
  DDFS.OFFLINE -> Common.OFFLINE
  DDFS.SILENT -> Common.SILENT
  DDFS.ON_PICKUP -> Common.ON_PICKUP
  DDFS.ON_RIDE -> Common.ON_RIDE
  DDFS.ACTIVE -> Common.ACTIVE
  DDFS.INACTIVE -> Common.INACTIVE
