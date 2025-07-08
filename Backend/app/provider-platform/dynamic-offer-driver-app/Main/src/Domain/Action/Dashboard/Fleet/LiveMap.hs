module Domain.Action.Dashboard.Fleet.LiveMap (getLiveMapDrivers) where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as ATD
import qualified API.Types.ProviderPlatform.Fleet.LiveMap as Common
import Domain.Action.Dashboard.Common (mobileIndianCode)
import Domain.Action.Dashboard.Fleet.Driver (validateRequestorRoleAndGetEntityId)
import Domain.Action.UI.Invoice (getSourceAndDestination, notAvailableText)
import Domain.Action.UI.Person (getPersonNumber)
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleVariant as DV
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
  ( GenericError (InternalError),
    PersonError (PersonDoesNotExist, PersonNotFound),
    RideError (RideDoesNotExist, RideNotFound),
    VehicleError (VehicleNotFound),
  )
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.WMB (getDriverCurrentLocation)
import qualified Storage.Clickhouse.Booking as CHB
import Storage.Queries.DriverInformationExtra (findAllByDriverIds)
import Storage.Queries.DriverOperatorAssociationExtra (findAllActiveDriverIdByOperatorId)
import Storage.Queries.FleetDriverAssociationExtra (findAllActiveDriverIdByFleetOwnerId)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.Vehicle as QV
import Tools.Error (GenericError (InvalidRequest))

getLiveMapDrivers ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe (ID.Id ATD.Driver) ->
  Common.NearbyDriverReq ->
  Environment.Flow [Common.MapDriverInfoRes]
getLiveMapDrivers merchantShortId _opCity requestorId mbFleetOwnerId mbDriverIdForRadius req = do
  requestedPerson <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId
  driverIdAssocWithEntityIdLs <- case entityRole of
    DP.FLEET_OWNER -> findAllActiveDriverIdByFleetOwnerId entityId
    DP.OPERATOR -> findAllActiveDriverIdByOperatorId entityId
    _ -> throwError (InvalidRequest "Invalid Data")
  merchant <- findMerchantByShortId merchantShortId
  latLong <- maybe (pure req.point) (getDriverCurrentLocation . ID.cast) mbDriverIdForRadius
  nearbyDriverLocations <- LF.nearBy latLong.lat latLong.lon Nothing (Just autoTypeLs) req.radius merchant.id Nothing
  let filtredNearbyDriverLocations = filter (\location -> location.driverId `elem` driverIdAssocWithEntityIdLs) nearbyDriverLocations
  driverInfoList <- findAllByDriverIds $ (.driverId.getId) <$> filtredNearbyDriverLocations
  mbPositionAndDriverInfoLs <- forM filtredNearbyDriverLocations $
    \location -> do
      let mbRideId = (.rideId) <$> location.rideDetails
          position = LatLong location.lat location.lon
          mbDriverInfo = find (\di -> di.driverId == location.driverId) driverInfoList
      pure $ (mbRideId,position,) <$> mbDriverInfo
  fmap catMaybes . forM mbPositionAndDriverInfoLs . maybe (pure Nothing) $ case entityRole of
    DP.FLEET_OWNER -> buildFleetMapDriverInfo
    DP.OPERATOR -> buildOperatorMapDriverInfo
    _ -> pure $ throwError (InvalidRequest "Invalid Data")
  where
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

buildFleetMapDriverInfo ::
  (Kernel.Prelude.Maybe Text, LatLong, DDI.DriverInformation) ->
  Environment.Flow (Kernel.Prelude.Maybe Common.MapDriverInfoRes)
buildFleetMapDriverInfo (mbRideId, position, driverInformation) = do
  driver <- QP.findById driverInformation.driverId >>= fromMaybeM (PersonNotFound driverInformation.driverId.getId)
  (source, destination) <- setSourceAndDestination driverInformation.onRide mbRideId
  mobileNumber <-
    getPersonNumber driver
      >>= fromMaybeM (InternalError $ "Driver mobile number is not present. DriverId: " <> driver.id.getId)
  pure . Just . Common.FleetMapDriverInfo $
    Common.FleetMapDriverInfoRes
      { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
        driverStatus = Common.ONLINE, -- driverInformation.driverStatus, TODO Chenge after merge,
        position = position,
        source = source,
        destination = destination,
        mobileCountryCode = fromMaybe mobileIndianCode driver.mobileCountryCode,
        mobileNumber = mobileNumber
      }

setSourceAndDestination ::
  Bool ->
  Kernel.Prelude.Maybe Text ->
  Environment.Flow (Text, Text)
setSourceAndDestination False _ = pure (notAvailableText, notAvailableText)
setSourceAndDestination True mbRideId =
  case mbRideId of
    Just rideId -> do
      ride <- QR.findById (ID.Id rideId) >>= fromMaybeM (RideNotFound rideId)
      mbBooking <- CHB.findById ride.bookingId
      getSourceAndDestination mbBooking
    _ -> throwError $ RideDoesNotExist "rideId is Nothing"

buildOperatorMapDriverInfo ::
  (Kernel.Prelude.Maybe Text, LatLong, DDI.DriverInformation) ->
  Environment.Flow (Kernel.Prelude.Maybe Common.MapDriverInfoRes)
buildOperatorMapDriverInfo (mbRideId, position, driverInformation) = do
  let driverId = driverInformation.driverId
  driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  vehicle <- QV.findById driverId >>= fromMaybeM (VehicleNotFound $ "driverId:-" <> driverId.getId)
  (source, destination) <- setSourceAndDestination driverInformation.onRide mbRideId
  pure . Just . Common.OperatorMapDriverInfo $
    Common.OperatorMapDriverInfoRes
      { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
        driverStatus = Common.ONLINE, -- driverInformation.driverStatus, TODO Chenge after merge
        vehicleNumber = vehicle.registrationNo,
        vehicleVariant = vehicle.variant,
        vehicleStatus = Common.ONLINE, -- TODO Get from vehicle table directly through driverId after merge
        position = position,
        source = source,
        destination = destination
      }
