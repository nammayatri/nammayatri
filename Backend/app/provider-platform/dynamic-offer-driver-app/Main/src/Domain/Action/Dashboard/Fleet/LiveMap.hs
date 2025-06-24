module Domain.Action.Dashboard.Fleet.LiveMap (getLiveMapDrivers) where

import qualified API.Types.ProviderPlatform.Fleet.LiveMap as Common
import qualified Data.List as L
import Data.Time (UTCTime (UTCTime, utctDay), addUTCTime, diffUTCTime, getCurrentTime)
import Domain.Action.Dashboard.Common (mobileIndianCode)
import Domain.Action.Dashboard.Fleet.Driver (validateOperatorToFleetAssoc) -- , validateRequestorRoleAndGetEntityId)
import Domain.Action.UI.Invoice (getSourceAndDestination, notAvailableText)
import Domain.Action.UI.Person (getPersonNumber)
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.FleetDriverAssociation as DFDA
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Environment
import EulerHS.Prelude hiding (id)
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
  )
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Time (getLocalCurrentTime, secondsToNominalDiffTime)
import SharedLogic.WMB (getDriverCurrentLocation)
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.Ride as CHR
import qualified Storage.Queries.DailyStats as SQDS
import Storage.Queries.DriverInformationExtra (findByIdAndVerified)
import Storage.Queries.DriverOperatorAssociationExtra (findAllActiveByOperatorId)
import Storage.Queries.FleetDriverAssociationExtra (findAllActiveDriverByFleetOwnerIdWithDriverInfo)
import qualified Storage.Queries.Person as QP
import Tools.Error
  ( AuthError (AccessDenied),
    GenericError (InvalidRequest),
    TransporterError (TransporterConfigNotFound),
  )

getLiveMapDrivers ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Kernel.Prelude.Maybe Text ->
  Environment.Flow [Common.MapDriverInfoRes]
getLiveMapDrivers _merchantShortId _opCity requestorId mbFleetOwnerId = do
  requestedPerson <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId
  catMaybes <$> case entityRole of
    DP.FLEET_OWNER -> do
      let limit = 10000
          offset = 0
      driverAndDriverInfoLs <- findAllActiveDriverByFleetOwnerIdWithDriverInfo entityId limit offset Nothing Nothing Nothing (Just True) Nothing
      mapM buildFleetMapDriverInfo driverAndDriverInfoLs
    DP.OPERATOR -> do
      driverIdLs <- map (.driverId) <$> findAllActiveByOperatorId entityId
      mbDriverInfoLs <- forM driverIdLs $ \driverId -> findByIdAndVerified driverId Nothing
      forM mbDriverInfoLs $ maybe (pure Nothing) buildOperatorMapDriverInfo
    _ -> throwError (InvalidRequest "Invalid Data")

buildFleetMapDriverInfo ::
  (DFDA.FleetDriverAssociation, DP.Person, DDI.DriverInformation) ->
  Environment.Flow (Kernel.Prelude.Maybe Common.MapDriverInfoRes)
buildFleetMapDriverInfo (_, driver, driverInformation) = do
  now <- liftIO getCurrentTime
  let driverStatus = Common.ONLINE -- driverInformation.driverStatus, TODO Chenge after rebase
      fromDate = UTCTime (utctDay now) 0
      toDate = now
  rideLs <- CHR.getAllRidesByDriverId driver.id fromDate toDate
  todaySummary <- buildTodaySummary (driver, driverInformation) rideLs driverStatus
  position <- getDriverCurrentLocation driver.id
  (source, destination) <-
    if null rideLs
      then pure (notAvailableText, notAvailableText)
      else do
        let ride = L.maximumBy (comparing (.tripStartTime)) rideLs
        mbBooking <- CHB.findById ride.bookingId
        getSourceAndDestination mbBooking
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

buildTodaySummary ::
  (DP.Person, DDI.DriverInformation) ->
  [CHR.Ride] ->
  Common.Status ->
  Environment.Flow Common.TodaySummary
buildTodaySummary (driver, driverInformation) rideLs driverStatus = do
  let trips = countTrips rideLs $ Trips 0 0
  transporterConfig <-
    CTC.findByMerchantOpCityId driver.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let merchantLocalDate = utctDay localTime
  mbDailyStats <- SQDS.findByDriverIdAndDate driver.id merchantLocalDate
  let onlineDuration = fromMaybe (Seconds 0) $ (.onlineDuration) =<< mbDailyStats
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
  DDI.DriverInformation ->
  Environment.Flow (Kernel.Prelude.Maybe Common.MapDriverInfoRes)
buildOperatorMapDriverInfo driverInformation = do
  now <- liftIO getCurrentTime
  driver <- QP.findById driverInformation.driverId >>= fromMaybeM (PersonNotFound driverInformation.driverId.getId)
  position <- getDriverCurrentLocation driver.id
  let fromDate = UTCTime (utctDay now) 0
      toDate = now
  rideLs <- CHR.getAllRidesByDriverId driver.id fromDate toDate
  (source, destination) <-
    if null rideLs
      then pure (notAvailableText, notAvailableText)
      else do
        let ride = L.maximumBy (comparing (.tripStartTime)) rideLs
        mbBooking <- CHB.findById ride.bookingId
        getSourceAndDestination mbBooking
  pure . Just . Common.OperatorMapDriverInfo $
    Common.OperatorMapDriverInfoRes
      { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
        driverStatus = Common.ONLINE, -- driverInformation.driverStatus, TODO Chenge after rebase
        vehicleNumber = "X000XX00", -- TODO Get from vehicle table directly through driverId
        vehicleStatus = Common.ONLINE, -- TODO Get from vehicle table directly through driverId
        position = position,
        source = source,
        destination = destination
      }

-- TODO Will import from Domain.Action.Dashboard.Fleet.Driver after rebase
validateRequestorRoleAndGetEntityId ::
  DP.Person ->
  Kernel.Prelude.Maybe Text ->
  Environment.Flow (DP.Role, Text)
validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId = do
  case requestedPerson.role of
    DP.FLEET_OWNER -> do
      -- Fleet Owner tries to do operation
      fleetOwnerid <- maybe (pure requestedPerson.id.getId) (\val -> if requestedPerson.id.getId == val then pure requestedPerson.id.getId else throwError AccessDenied) mbFleetOwnerId
      pure (DP.FLEET_OWNER, fleetOwnerid)
    DP.OPERATOR -> do
      case mbFleetOwnerId of
        Just fleetOwnerId -> do
          -- Operator tries to do operation on behalf of the fleet
          validateOperatorToFleetAssoc requestedPerson.id.getId fleetOwnerId
          pure (DP.FLEET_OWNER, fleetOwnerId)
        Nothing -> pure (DP.OPERATOR, requestedPerson.id.getId) -- Operator tries to do operation
    _ -> throwError (InvalidRequest "Invalid Data")
