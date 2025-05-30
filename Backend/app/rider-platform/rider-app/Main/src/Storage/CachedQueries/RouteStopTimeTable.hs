{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.RouteStopTimeTable
  ( findByRouteCodeAndStopCode,
    CalledForFare (..),
    castVehicleType,
  )
where

import qualified BecknV2.FRFS.Enums
import qualified BecknV2.OnDemand.Enums as VehicleCategory
import Data.Text as Text
import Domain.Types.GTFSFeedInfo (GTFSFeedInfo)
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopTimeTable
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude as P
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.GTFSFeedInfo as GTFSFeedInfo
import qualified Storage.GraphqlQueries.RouteStopTimeTable as Queries
import Tools.Error

modifyCodesToGTFS :: GTFSFeedInfo -> Text -> Text
modifyCodesToGTFS gtfsFeedInfo codes = gtfsFeedInfo.feedId.getId <> ":" <> codes

castVehicleType :: MonadFlow m => VehicleCategory.VehicleCategory -> m BecknV2.FRFS.Enums.VehicleCategory
castVehicleType vehicleType = do
  case vehicleType of
    VehicleCategory.BUS -> return BecknV2.FRFS.Enums.BUS
    VehicleCategory.METRO -> return BecknV2.FRFS.Enums.METRO
    VehicleCategory.SUBWAY -> return BecknV2.FRFS.Enums.SUBWAY
    _ -> throwError $ InternalError "Invalid vehicle type"

findByRouteCodeAndStopCode ::
  ( MonadFlow m,
    ServiceFlow m r
  ) =>
  IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  [Text] ->
  Text ->
  m [RouteStopTimeTable]
findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes' stopCode' = do
  vehicleType <- castVehicleType integratedBPPConfig.vehicleCategory
  gtfsFeedInfo <- GTFSFeedInfo.findByVehicleTypeAndCity vehicleType integratedBPPConfig.merchantOperatingCityId integratedBPPConfig.merchantId
  let routeCodes = P.map (modifyCodesToGTFS gtfsFeedInfo) routeCodes'
      stopCode = modifyCodesToGTFS gtfsFeedInfo stopCode'
  allTrips <-
    Hedis.safeGet (routeTimeTableKey stopCode) >>= \case
      Just a -> pure a
      Nothing -> cacheRouteStopTimeInfo stopCode /=<< Queries.findByRouteCodeAndStopCode integratedBPPConfig.id merchantId merchantOpId routeCodes' stopCode vehicleType
  logDebug $ "Fetched route stop time table cached: " <> show allTrips <> "for routeCodes:" <> show routeCodes <> " and stopCode:" <> show stopCode
  val <- L.getOptionLocal CalledForFare
  return $ P.filter (\trip -> (trip.routeCode `P.elem` routeCodes') || (val == Just True)) allTrips

cacheRouteStopTimeInfo :: (CacheFlow m r, MonadFlow m) => Text -> [RouteStopTimeTable] -> m ()
cacheRouteStopTimeInfo stopCode routeStopInfo = do
  let expTime = 60 * 60
  let idKey = routeTimeTableKey stopCode
  Hedis.setExp idKey routeStopInfo expTime

routeTimeTableKey :: Text -> Text
routeTimeTableKey stopCode = "routeStop-time-table:" <> stopCode

data CalledForFare = CalledForFare
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity CalledForFare Bool
