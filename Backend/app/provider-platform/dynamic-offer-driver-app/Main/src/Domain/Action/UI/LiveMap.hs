{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.LiveMap (getLiveMapDrivers) where

import qualified API.Types.UI.LiveMap as Common
import qualified Data.List as L
import Data.OpenApi (ToSchema)
import Data.Ord (comparing)
import Data.Time (UTCTime (UTCTime, utctDay), getCurrentTime)
import Domain.Action.UI.Invoice (getSourceAndDestination)
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride as DR
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
  ( PersonError (PersonNotFound),
    RideError (RideDoesNotExist),
    VehicleError (VehicleNotFound),
  )
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing (fromMaybeM, throwError)
import SharedLogic.WMB (getDriverCurrentLocation)
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.Ride as CHR
import qualified Storage.Clickhouse.RideDetails as CHRD
import Storage.Queries.DriverInformationExtra (findByIdAndVerified)
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Error (DriverInformationError (..))

getLiveMapDrivers ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow [Common.MapDriverInfoRes]
getLiveMapDrivers (mbPersonId, _merchantId, _merchantOpCityId) = do
  driver <- traverse QP.findById mbPersonId >>= fromMaybeM (PersonNotFound $ show mbPersonId) . join
  now <- liftIO getCurrentTime
  let fromDate = UTCTime (utctDay now) 0
      toDate = now
  rideLs <- CHR.getAllRidesByDriverId driver.id fromDate toDate
  when (null rideLs) . throwError . RideDoesNotExist $ "for driverId " <> driver.id.getId
  let ride = L.maximumBy (comparing (.tripStartTime)) rideLs
  vehicleNumber <-
    CHRD.findByIdAndVehicleNumber (Kernel.Types.Id.cast ride.id) Nothing
      >>= fromMaybeM (VehicleNotFound $ "for rideId" <> ride.id.getId) . Kernel.Prelude.msum
  driverInformation <- findByIdAndVerified driver.id Nothing >>= fromMaybeM DriverInfoNotFound
  position <- getDriverCurrentLocation driver.id
  mbBooking <- CHB.findById ride.bookingId
  (source, destination) <- getSourceAndDestination mbBooking
  pure . pure $
    Common.MapDriverInfoRes
      { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
        driverStatus = castDriverStatus driverInformation.mode,
        vehicleNumber = vehicleNumber,
        vehicleStatus = getVehicleStatus driverInformation rideLs,
        position = position,
        source = source,
        destination = destination
      }
  where
    castDriverStatus = \case
      -- Needs to be changed after implementation "2. Live Activity (No. of drivers)"
      Just DCommon.ONLINE -> Common.ONLINE
      Just DCommon.SILENT -> Common.SILENT
      _ -> Common.OFFLINE
    getVehicleStatus driverInformation rideLs
      | not driverInformation.active = Common.InActive
      | driverInformation.onRide = Common.OnRide
      | any checkRideStatus rideLs = Common.TripAssigned
      | otherwise = Common.Pending
      where
        checkRideStatus ride = ride.status `elem` activeRideStatuses
        activeRideStatuses = Just <$> [DR.UPCOMING, DR.NEW, DR.INPROGRESS]
