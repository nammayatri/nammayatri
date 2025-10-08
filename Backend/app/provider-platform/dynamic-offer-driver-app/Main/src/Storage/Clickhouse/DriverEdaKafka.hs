{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.DriverEdaKafka where

import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data DriverEdaKafkaT f = DriverEdaKafkaT
  { driverId :: C f (Id DP.Driver),
    rideId :: C f (Maybe (Id DRide.Ride)),
    timestamp :: C f UTCTime, -- DateTime64(3) on clickhouse side
    accuracy :: C f (Maybe Double),
    rideStatus :: C f (Maybe Status),
    lat :: C f (Maybe Double),
    lon :: C f (Maybe Double),
    merchantId :: C f (Maybe (Id DM.Merchant)),
    updatedAt :: C f (Maybe UTCTime), -- Nullable(String) on clickhouse side
    createdAt :: C f (Maybe UTCTime),
    onRide :: C f (Maybe Bool),
    active :: C f (Maybe Bool),
    partitionDate :: C f Time.Day, -- Date on clickhouse side
    date :: C f CH.DateTime -- DateTime on clickhouse side
  }
  deriving (Generic)

deriving instance Show DriverEdaKafka

-- FIXME Status from API.Types.ProviderPlatform.Management.Ride
data Status
  = ON_RIDE
  | ON_PICKUP
  | IDLE
  deriving (Show, Read)

instance ClickhouseValue Status

driverEdaKafkaTTable :: DriverEdaKafkaT (FieldModification DriverEdaKafkaT)
driverEdaKafkaTTable =
  DriverEdaKafkaT
    { driverId = "driver_id",
      rideId = "rid",
      timestamp = "ts",
      accuracy = "acc",
      rideStatus = "rideStatus",
      lat = "lat",
      lon = "lon",
      merchantId = "mid",
      updatedAt = "updated_at",
      createdAt = "created_at",
      onRide = "on_ride",
      active = "active",
      partitionDate = "partition_date",
      date = "date"
    }

type DriverEdaKafka = DriverEdaKafkaT Identity

$(TH.mkClickhouseInstances ''DriverEdaKafkaT 'NO_SELECT_MODIFIER)

-- FIXME what if we use the same operator like CH.filter_ twice
findAll :: CH.HasClickhouseEnv CH.ATLAS_KAFKA m => UTCTime -> UTCTime -> Id DP.Driver -> Maybe (Id DRide.Ride) -> m [DriverEdaKafka]
findAll firstDate lastDate driverId rideId = do
  CH.findAll $
    CH.select $
      CH.orderBy_ (\driverEdaKafka _ -> CH.asc driverEdaKafka.timestamp) $
        CH.filter_
          ( \driverEdaKafka ->
              ( driverEdaKafka.partitionDate CH.==. Time.utctDay firstDate
                  CH.||. driverEdaKafka.partitionDate ==. Time.utctDay lastDate
              )
                CH.&&. driverEdaKafka.driverId CH.==. driverId
                CH.&&. driverEdaKafka.rideId CH.==. rideId
          )
          (CH.all_ @CH.ATLAS_KAFKA driverEdaKafkaTTable)

-- up to 5 columns supported now
findAllTuple ::
  CH.HasClickhouseEnv CH.ATLAS_KAFKA m =>
  UTCTime ->
  UTCTime ->
  Id DP.Driver ->
  Maybe (Id DRide.Ride) ->
  m [(Maybe Double, Maybe Double, UTCTime, Maybe Double, Maybe Status)]
findAllTuple firstDate lastDate driverId rideId = do
  CH.findAll $
    CH.select_ (\dek -> CH.notGrouped (dek.lat, dek.lon, dek.timestamp, dek.accuracy, dek.rideStatus)) $
      CH.orderBy_ (\dek _ -> CH.asc dek.timestamp) $
        CH.filter_
          ( \dek ->
              ( dek.partitionDate CH.==. Time.utctDay firstDate
                  CH.||. dek.partitionDate ==. Time.utctDay lastDate
              )
                CH.&&. dek.driverId CH.==. driverId
                CH.&&. dek.rideId CH.==. rideId
          )
          (CH.all_ @CH.ATLAS_KAFKA driverEdaKafkaTTable)

-- Query using timestamp range instead of partitionDate equality as timestamp seems to be much better indexed with utc time not timeofday
findAllTupleByTimestamp ::
  CH.HasClickhouseEnv CH.ATLAS_KAFKA m =>
  UTCTime ->
  UTCTime ->
  Id DP.Driver ->
  Int ->
  m [(Maybe Double, Maybe Double, UTCTime, Maybe Double, Maybe Status)]
findAllTupleByTimestamp startTs endTs driverId limit = do
  CH.findAll $
    CH.select_ (\dek -> CH.notGrouped (dek.lat, dek.lon, dek.timestamp, dek.accuracy, dek.rideStatus)) $
      CH.orderBy_ (\dek _ -> CH.asc dek.timestamp) $
        CH.limit_ limit $
          CH.filter_
            ( \dek ->
                (dek.timestamp CH.>=. startTs CH.&&. dek.timestamp CH.<=. endTs)
                  CH.&&. dek.driverId CH.==. driverId
            )
            (CH.all_ @CH.ATLAS_KAFKA driverEdaKafkaTTable)
