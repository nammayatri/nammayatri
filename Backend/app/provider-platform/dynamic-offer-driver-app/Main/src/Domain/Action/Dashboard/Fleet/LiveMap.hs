{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Fleet.LiveMap (getLiveMapDrivers) where

import qualified API.Types.ProviderPlatform.Fleet.LiveMap as Common
import qualified Data.List as L
import Data.OpenApi (ToSchema)
import Data.Ord (comparing)
import Data.Time (UTCTime (UTCTime, utctDay), getCurrentTime)
--import qualified Domain.Types.MerchantOperatingCity
import Domain.Action.Dashboard.Common (mobileIndianCode)
import Domain.Action.UI.Invoice (getSourceAndDestination)
import Domain.Action.UI.Person (getPersonNumber)
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (Meters (..), Money (..), Seconds (..))
import Kernel.Types.Error
  ( GenericError (InternalError),
    PersonError (PersonNotFound),
    RideError (RideDoesNotExist),
    VehicleError (VehicleNotFound),
  )
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import Kernel.Utils.Time (getLocalCurrentTime)
import Servant
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.WMB (getDriverCurrentLocation)
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.Ride as CHR
import qualified Storage.Clickhouse.RideDetails as CHRD
import qualified Storage.Queries.DailyStats as SQDS
import Storage.Queries.DriverInformationExtra (findAllWithLimitOffsetByMerchantId, findByIdAndVerified)
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Error (DriverInformationError (..), TransporterError (TransporterConfigNotFound))

getLiveMapDrivers ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [Common.MapDriverInfoRes]
getLiveMapDrivers merchantShortId _opCity = do
  merchant <- findMerchantByShortId merchantShortId
  driverAndDriverInfoLs <- findAllWithLimitOffsetByMerchantId Nothing Nothing Nothing Nothing merchant.id
  catMaybes <$> mapM buildMapDriverInfo driverAndDriverInfoLs

buildMapDriverInfo :: (DP.Person, DDI.DriverInformation) -> Environment.Flow (Kernel.Prelude.Maybe Common.MapDriverInfoRes)
buildMapDriverInfo (driver, driverInformation) = do
  now <- liftIO getCurrentTime
  let fromDate = UTCTime (utctDay now) 0
      toDate = now
  rideLs <- CHR.getAllRidesByDriverId driver.id fromDate toDate
  if null rideLs
    then pure Nothing
    else do
      todaySummary <- buildTodaySummary (driver, driverInformation) rideLs
      let ride = L.maximumBy (comparing (.tripStartTime)) rideLs
      position <- getDriverCurrentLocation driver.id
      mbBooking <- CHB.findById ride.bookingId
      (source, destination) <- getSourceAndDestination mbBooking
      pure . Just $
        Common.MapDriverInfoRes
          { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
            driverStatus = castDriverStatus driverInformation.mode,
            todaySummary = todaySummary,
            position = position,
            source = source,
            destination = destination
          }

castDriverStatus :: Maybe DCommon.DriverMode -> Common.Status
castDriverStatus = \case
  -- Needs to be changed after implementation "2. Live Activity (No. of drivers)"
  Just DCommon.ONLINE -> Common.ONLINE
  Just DCommon.SILENT -> Common.SILENT
  _ -> Common.OFFLINE

buildTodaySummary :: (DP.Person, DDI.DriverInformation) -> [CHR.Ride] -> Environment.Flow Common.TodaySummary
buildTodaySummary (driver, driverInformation) rideLs = do
  mobileNumber <- getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
  let trips = countTrips rideLs $ Trips 0 0 0 0 0 0
  transporterConfig <-
    CTC.findByMerchantOpCityId driver.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let merchantLocalDate = utctDay localTime
  mbDailyStats <- SQDS.findByDriverIdAndDate driver.id merchantLocalDate
  let onlineDuration = fromMaybe (Seconds 0) $ (.onlineDuration) =<< mbDailyStats
  pure $
    Common.TodaySummary
      { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
        tripStatus = castDriverStatus driverInformation.mode,
        tripsCompletedCount = trips.tripsCompletedCount,
        earnings = Money trips.fare,
        totalDistance = Meters trips.chargeableDistance,
        tripBalanceLeft = 0, -- FXME
        tripsCancelled = trips.tripsCancelled,
        tripsPassed = trips.tripsPassed,
        tripsScheduled = trips.tripsScheduled,
        onlineDuration = onlineDuration, --TODO
        mobileCountryCode = fromMaybe mobileIndianCode driver.mobileCountryCode,
        mobileNumber = mobileNumber
      }

data Trips = Trips
  { tripsCompletedCount :: Int,
    tripsCancelled :: Int,
    tripsPassed :: Int,
    tripsScheduled :: Int,
    chargeableDistance :: Int,
    fare :: Int
  }

-- RideStatus = UPCOMING | NEW | INPROGRESS | COMPLETED | CANCELLED

countTrips :: [CHR.Ride] -> Trips -> Trips
countTrips [] trips = trips
countTrips (rd : rds) trips =
  let trips' =
        case rd.status of
          Nothing -> trips
          Just DR.COMPLETED ->
            trips
              { tripsCompletedCount = trips.tripsCompletedCount + 1,
                tripsPassed = trips.tripsPassed + 1
              }
          Just DR.CANCELLED ->
            trips
              { tripsCancelled = trips.tripsCancelled + 1,
                tripsPassed = trips.tripsPassed + 1
              }
          _ -> countTrips rds $ trips {tripsScheduled = trips.tripsScheduled + 1}
   in countTrips rds $
        trips'
          { chargeableDistance = trips.chargeableDistance + fromMaybe 0 rd.chargeableDistance,
            fare = trips.fare + fromMaybe 0 rd.fare
          }
