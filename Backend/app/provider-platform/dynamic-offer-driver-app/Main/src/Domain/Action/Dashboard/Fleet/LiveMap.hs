module Domain.Action.Dashboard.Fleet.LiveMap (getLiveMapDrivers) where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as ATD
import qualified API.Types.ProviderPlatform.Fleet.LiveMap as Common
import Data.Time (UTCTime (UTCTime, utctDay), addUTCTime, diffUTCTime, getCurrentTime)
import Domain.Action.Dashboard.Common (mobileIndianCode)
import Domain.Action.Dashboard.Fleet.Driver (validateRequestorRoleAndGetEntityId)
import Domain.Action.UI.Invoice (getSourceAndDestination, notAvailableText)
import Domain.Action.UI.Person (getPersonNumber)
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.VehicleVariant as DV
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
  ( HighPrecMoney (..),
    Meters (..),
    Seconds (..),
  )
import Kernel.Types.Error
  ( GenericError (InternalError),
    PersonError (PersonDoesNotExist, PersonNotFound),
    RideError (RideDoesNotExist, RideNotFound),
    VehicleError (VehicleNotFound),
  )
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Time (secondsToNominalDiffTime)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.WMB (getDriverCurrentLocation)
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.Ride as CHR
import qualified Storage.Queries.DailyStats as SQDS
import Storage.Queries.DriverInformationExtra (findByIdAndVerified)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.Vehicle as QV
import Tools.Error
  ( GenericError (InvalidRequest),
    TransporterError (TransporterConfigNotFound),
  )

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
  merchant <- findMerchantByShortId merchantShortId
  latLong <- maybe (pure req.point) (getDriverCurrentLocation . ID.cast) mbDriverIdForRadius
  nearbyDriverLocations <- LF.nearBy latLong.lat latLong.lon Nothing (Just autoTypeLs) req.radius merchant.id (Just entityId)
  mbPositionAndDriverInfoLs <- forM nearbyDriverLocations $
    \location -> do
      let mbRideId = (.rideId) <$> location.rideDetails
          position = LatLong location.lat location.lon
      mbDriverInfo <- findByIdAndVerified location.driverId Nothing
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
  let driverStatus = Common.ONLINE -- driverInformation.driverStatus, TODO Chenge after merge
  todaySummary <- buildTodaySummary (driver, driverInformation) driverStatus
  (source, destination) <- setSourceAndDestination driverInformation.onRide mbRideId
  mobileNumber <- getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
  pure . Just . Common.FleetMapDriverInfo $
    Common.FleetMapDriverInfoRes
      { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
        driverStatus = driverStatus,
        todaySummary = todaySummary,
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

buildTodaySummary ::
  (DP.Person, DDI.DriverInformation) ->
  Common.Status ->
  Environment.Flow Common.TodaySummary
buildTodaySummary (driver, driverInformation) driverStatus = do
  now <- liftIO getCurrentTime
  transporterConfig <-
    CTC.findByMerchantOpCityId driver.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
  let localTime = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) now
      fromDate = UTCTime (utctDay now) 0
      toDate = now
      merchantLocalDate = utctDay localTime
  rideLs <- CHR.getAllRidesByDriverId driver.id fromDate toDate
  mbDailyStats <- SQDS.findByDriverIdAndDate driver.id merchantLocalDate
  let trips = countTrips rideLs $ Trips 0 0
      onlineDuration = fromMaybe (Seconds 0) $ (.onlineDuration) =<< mbDailyStats
      additionalTimeInOnline =
        Seconds
          if driverStatus == Common.ONLINE
            then
              let startDayTime = UTCTime (utctDay localTime) 0
                  mbOnlineFrom = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) <$> driverInformation.onlineDurationRefreshedAt
                  onlineFrom = maybe startDayTime (max startDayTime) mbOnlineFrom
               in floor $ diffUTCTime localTime onlineFrom
            else 0
  pure $
    Common.TodaySummary
      { tripStatus = castStatus driverInformation.mode,
        tripsCompletedCount = maybe 0 (.numRides) mbDailyStats,
        earnings = maybe (HighPrecMoney 0) (.totalEarnings) mbDailyStats,
        totalDistance = maybe (Meters 0) (.totalDistance) mbDailyStats,
        tripBalanceLeft = 0, -- FXME
        tripsCancelled = trips.tripsCancelled,
        tripsPassed = trips.tripsPassed,
        tripsScheduled = maybe 0 (.activatedValidRides) mbDailyStats,
        onlineDuration = onlineDuration + additionalTimeInOnline
      }

castStatus :: Maybe DCommon.DriverMode -> Common.Status
castStatus = \case
  -- Needs to be changed after implementation "2. Live Activity (No. of drivers)"
  Just DCommon.ONLINE -> Common.ONLINE
  Just DCommon.SILENT -> Common.SILENT
  _ -> Common.OFFLINE

data Trips = Trips
  { tripsCancelled :: Int,
    tripsPassed :: Int
  }

-- RideStatus = UPCOMING | NEW | INPROGRESS | COMPLETED | CANCELLED

countTrips :: [CHR.Ride] -> Trips -> Trips
countTrips [] trips = trips
countTrips (rd : rds) trips =
  let trips' =
        case rd.status of
          Nothing -> trips
          Just DR.COMPLETED ->
            trips {tripsPassed = trips.tripsPassed + 1}
          Just DR.CANCELLED ->
            trips
              { tripsCancelled = trips.tripsCancelled + 1,
                tripsPassed = trips.tripsPassed + 1
              }
          _ -> countTrips rds trips
   in countTrips rds trips'

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
