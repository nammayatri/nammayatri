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
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity),
    mode :: C f (Maybe DI.DriverMode),
    vehicleServiceTier :: C f DServiceTierType.ServiceTierType,
    requestId :: C f (Id DSR.SearchRequest),
    createdAt :: C f CH.DateTime --,
  }
  deriving (Generic)

deriving instance Show SearchRequestForDriver

searchRequestForDriverTTable :: SearchRequestForDriverT (FieldModification SearchRequestForDriverT)
searchRequestForDriverTTable =
  SearchRequestForDriverT
    { id = "id",
      driverId = "driver_id",
      fromLocGeohash = "from_loc_geohash",
      merchantOperatingCityId = "merchant_operating_city_id",
      mode = "mode",
      vehicleServiceTier = "vehicle_service_tier",
      requestId = "search_request_id",
      createdAt = "created_at"
    }

type SearchRequestForDriver = SearchRequestForDriverT Identity

$(TH.mkClickhouseInstances ''SearchRequestForDriverT 'SELECT_FINAL_MODIFIER)

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
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
              CH.&&. srfd.mode CH.==. Just DI.ONLINE
              CH.&&. CH.isNotNull srfd.fromLocGeohash
              --   CH.&&. CH.not_ (srfd.vehicleServiceTier `in_` [DServiceTierType.AUTO_RICKSHAW, DServiceTierType.BIKE, DServiceTierType.AMBULANCE_TAXI, DServiceTierType.AMBULANCE_TAXI_OXY, DServiceTierType.AMBULANCE_AC, DServiceTierType.AMBULANCE_AC_OXY, DServiceTierType.AMBULANCE_VENTILATOR, DServiceTierType.DELIVERY_BIKE])
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)
