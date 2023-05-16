{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.CachedQueries.SimulatedFlow.Driver
  ( findAllDrivers,
    cacheSelectedDriver,
    getLinkedDriverByEstimateId,
  )
where

import qualified Domain.Types.Estimate as DE
import Domain.Types.SimulatedFlow.Driver
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig

allDriversKey :: Text
allDriversKey = "Customer:SimulatedFlow:AllSimulatedDrivers"

mkDriverLinkKey :: Id DE.Estimate -> Text
mkDriverLinkKey estimateId = "Customer:SimulatedFlow:Link:Estimate:Driver:" <> estimateId.getId

findAllDrivers :: SimluatedCacheFlow m r => m [SimulatedDriver]
findAllDrivers = Redis.lRange allDriversKey 0 (-1)

data DriverInfo = DriverInfo
  { driver :: SimulatedDriver,
    location :: Maps.LatLong,
    driverRoute :: Maps.RouteInfo
  }
  deriving (Generic, ToJSON, FromJSON)

cacheSelectedDriver :: SimluatedCacheFlow m r => Id DE.Estimate -> SimulatedDriver -> Maps.LatLong -> Maps.RouteInfo -> m ()
cacheSelectedDriver estimateId selectedDriver driverPosition routeInfo = do
  expTime <- fromIntegral <$> asks (.simulatedDataCacheConfig.configsExpTime)
  Redis.setExp (mkDriverLinkKey estimateId) (DriverInfo selectedDriver driverPosition routeInfo) expTime

getLinkedDriverByEstimateId :: SimluatedCacheFlow m r => Id DE.Estimate -> m (Maybe DriverInfo)
getLinkedDriverByEstimateId estimateId = Redis.get (mkDriverLinkKey estimateId)
