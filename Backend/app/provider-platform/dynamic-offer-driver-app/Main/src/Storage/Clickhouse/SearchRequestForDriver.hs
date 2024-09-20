{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.SearchRequestForDriver where

import qualified Domain.Types.Common as DI
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Domain.Types.ServiceTierType as DServiceTierType
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data SearchRequestForDriverT f = SearchRequestForDriverT
  { id :: C f (Id DSRD.SearchRequestForDriver),
    driverId :: C f (Id DP.Driver),
    fromLocGeohash :: C f (Maybe Text),
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity), --   Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    mode :: C f (Maybe DI.DriverMode),
    vehicleServiceTier :: C f (DServiceTierType.ServiceTierType),
    requestId :: C f (Id DSR.SearchRequest), -- Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    -- fleetOwnerId :: C f (Maybe Text),
    createdAt :: C f UTCTime --,
  }
  deriving (Generic)

deriving instance Show SearchRequestForDriver

searchRequestForDriverTTable :: SearchRequestForDriverT (FieldModification SearchRequestForDriverT)
searchRequestForDriverTTable =
  SearchRequestForDriverT
    { id = "id",
      driverId = "driverId",
      fromLocGeohash = "from_loc_geohash",
      merchantOperatingCityId = "merchant_operating_city_id",
      mode = "mode",
      vehicleServiceTier = "vehicle_service_tier",
      requestId = "request_id",
      createdAt = "created_at"
    }

type SearchRequestForDriver = SearchRequestForDriverT Identity

$(TH.mkClickhouseInstances ''SearchRequestForDriverT)

-- instance ClickhouseValue DI.DriverMode
-- instance ClickhouseValue DServiceTierType.ServiceTierType

calulateSupplyDemandByGeohashAndServiceTier ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Int, Int, DServiceTierType.ServiceTierType)]
calulateSupplyDemandByGeohashAndServiceTier from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let supplyCount = CH.count_ (CH.distinct srfd.driverId)
          let demandCount = CH.count_ (CH.distinct srfd.requestId)
          CH.groupBy (srfd.fromLocGeohash, srfd.vehicleServiceTier) $ \(fromLocGeohash, vehicleServiceTier) -> do
            (fromLocGeohash, supplyCount, demandCount, vehicleServiceTier)
      )
      $ CH.filter_
        ( \srfd _ ->
            -- rideDetails.fleetOwnerId CH.==. fleetOwnerId
            srfd.createdAt >=. from
              CH.&&. srfd.createdAt <=. to
              CH.&&. srfd.mode CH.==. Just DI.ONLINE
              CH.&&. CH.isNotNull srfd.fromLocGeohash
              --   CH.&&. CH.not_ (srfd.vehicleServiceTier `in_` [DServiceTierType.AUTO_RICKSHAW, DServiceTierType.BIKE, DServiceTierType.AMBULANCE_TAXI, DServiceTierType.AMBULANCE_TAXI_OXY, DServiceTierType.AMBULANCE_AC, DServiceTierType.AMBULANCE_AC_OXY, DServiceTierType.AMBULANCE_VENTILATOR, DServiceTierType.DELIVERY_BIKE])
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

--   res <-
--     CH.findAll $
--       CH.select_
--         ( \ride -> do
--             let earnings = CH.sum_ ride.fare
--             let distanceTravelled = CH.sum_ ride.chargeableDistance
--             let completedRides = CH.sum_ $ CH.if_ (ride.status CH.==.. CH.valColumn (Just DRide.COMPLETED)) (CH.valColumn 1) (CH.valColumn 0)
--             let cancelledRides = CH.sum_ $ CH.if_ (ride.status CH.==.. CH.valColumn (Just DRide.CANCELLED)) (CH.valColumn 1) (CH.valColumn 0)
--             let duration = CH.sum_ (CH.timeDiff ride.createdAt ride.updatedAt)
--             CH.groupBy ride.driverId $ \driverId -> do
--               (earnings, distanceTravelled, completedRides, cancelledRides, duration, driverId)
--         )
--         $ CH.orderBy_ sortOn $
--           CH.limit_ limit $
--             CH.offset_ offset $
--               CH.filter_
--                 ( \ride _ ->
--                     ride.status `in_` [Just DRide.COMPLETED, Just DRide.CANCELLED]
--                       CH.&&. ride.createdAt >=. from
--                       CH.&&. ride.createdAt <=. to
--                       CH.&&. ride.id `in_` rideIds
--                       CH.&&. CH.whenJust_ mbDriverId (\driverId -> ride.driverId CH.==. Just (cast driverId))
--                 )
--                 (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideTTable)

-- findIdsByFleetOwnerAndVehicle ::
--   CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
--   Maybe Text ->
--   Text ->
--   UTCTime ->
--   UTCTime ->
--   m [Id DRD.RideDetails]
-- findIdsByFleetOwnerAndVehicle fleetOwnerId vehicleNumber from to = do
--   CH.findAll $
--     CH.select_ (\rd -> CH.notGrouped (rd.id)) $
--       CH.filter_
--         ( \rideDetails _ ->
--             rideDetails.fleetOwnerId CH.==. fleetOwnerId
--               CH.&&. rideDetails.vehicleNumber CH.==. Just vehicleNumber
--               CH.&&. rideDetails.createdAt >=. (Just $ CH.DateTime from)
--               CH.&&. rideDetails.createdAt <=. (Just $ CH.DateTime to)
--         )
--         (CH.all_ @CH.APP_SERVICE_CLICKHOUSE rideDetailsTTable)
