{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.Ride where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id

data RideT f = RideT
  { bookingId :: C f (Id DB.Booking),
    shortId :: C f (ShortId DRide.Ride),
    driverName :: C f (Maybe Text),
    vehicleNumber :: C f (Maybe Text),
    chargeableDistance :: C f (Maybe HighPrecMeters),
    -- chargeableDistanceValue :: C f (Maybe HighPrecDistance), -- FIXME uncomment
    -- distanceUnit :: C f (Maybe HighPrecDistance), -- FIXME uncomment
    totalFare :: C f (Maybe HighPrecMoney),
    rideStartTime :: C f (Maybe UTCTime),
    rideEndTime :: C f (Maybe UTCTime),
    createdAt :: C f UTCTime,
    updatedAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show Ride

-- TODO move to TH (quietSnake)
rideTTable :: RideT (FieldModification RideT)
rideTTable =
  RideT
    { bookingId = "booking_id",
      shortId = "short_id",
      driverName = "driver_name",
      vehicleNumber = "vehicle_number",
      chargeableDistance = "chargeable_distance",
      totalFare = "total_fare",
      rideStartTime = "ride_start_time",
      rideEndTime = "ride_end_time",
      createdAt = "created_at",
      updatedAt = "updated_at"
    }

type Ride = RideT Identity

$(TH.mkClickhouseInstances ''RideT 'SELECT_FINAL_MODIFIER)

findRideByBookingId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DB.Booking ->
  UTCTime ->
  m (Maybe Ride)
findRideByBookingId bookingId createdAt = do
  ride <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \ride ->
              ride.bookingId CH.==. bookingId
                CH.&&. ride.createdAt >=. createdAt
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  return $ listToMaybe ride
