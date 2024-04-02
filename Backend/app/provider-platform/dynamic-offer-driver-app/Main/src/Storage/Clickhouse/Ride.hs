{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.Ride where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

data RideT f = RideT
  { id :: C f (Id DRide.Ride),
    status :: C f (Maybe DRide.RideStatus),
    fare :: C f (Maybe Int),
    driverId :: C f (Maybe (Id DP.Driver))
  }
  deriving (Generic)

deriving instance Show Ride

instance ClickhouseValue DRide.RideStatus

rideTTable :: RideT (FieldModification RideT)
rideTTable =
  RideT
    { id = "id",
      status = "status",
      fare = "fare",
      driverId = "driver_id"
    }

type Ride = RideT Identity

$(TH.mkClickhouseInstances ''RideT)

getCompletedRidesByDriver ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  Id DP.Person ->
  m Int
getCompletedRidesByDriver rideIds driverId = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.groupBy ride.status $ \_status -> CH.count_ ride.id) $
        CH.filter_
          ( \ride _ ->
              ride.status CH.==. Just DRide.COMPLETED
                CH.&&. ride.driverId CH.==. Just (cast driverId)
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  pure $ fromMaybe 0 (listToMaybe res)

getRidesByIdAndStatus ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  DRide.RideStatus ->
  m Int
getRidesByIdAndStatus rideIds status = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.groupBy ride.status $ \_status -> CH.count_ ride.id) $
        CH.filter_
          ( \ride _ ->
              ride.status CH.==. Just status
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  pure $ fromMaybe 0 (listToMaybe res)

getEarningsByDriver ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  Id DP.Person ->
  m Int
getEarningsByDriver rideIds driverId = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.groupBy ride.status $ \_status -> CH.sum_ ride.fare) $
        CH.filter_
          ( \ride _ ->
              ride.status CH.==. Just DRide.COMPLETED
                CH.&&. ride.driverId CH.==. Just (cast driverId)
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  case res of
    [] -> pure 0
    [earnings] -> pure $ fromMaybe 0 earnings
    _ -> throwError $ InternalError "getEarningsByDriver query returns more than 1 response"

getEarningsByIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRide.Ride] ->
  m Int
getEarningsByIds rideIds = do
  res <-
    CH.findAll $
      CH.select_ (\ride -> CH.groupBy ride.status $ \_status -> CH.sum_ ride.fare) $
        CH.filter_
          ( \ride _ ->
              ride.status CH.==. Just DRide.COMPLETED
                CH.&&. ride.id `in_` rideIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)
  case res of
    [] -> pure 0
    [earnings] -> pure $ fromMaybe 0 earnings
    _ -> throwError $ InternalError "getEarningsByDriver query returns more than 1 response"
