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
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopTimeTable
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Prelude as P
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.OTPRest.Common as OTPRestCommon
import qualified Storage.GraphqlQueries.RouteStopTimeTable as Queries
import qualified System.Environment as Se
import Tools.Error

modifyCodesToGTFS :: IntegratedBPPConfig -> Text -> Text
modifyCodesToGTFS integratedBPPConfig codes = integratedBPPConfig.feedKey <> ":" <> codes

castVehicleType :: MonadFlow m => VehicleCategory.VehicleCategory -> m BecknV2.FRFS.Enums.VehicleCategory
castVehicleType vehicleType = do
  case vehicleType of
    VehicleCategory.BUS -> return BecknV2.FRFS.Enums.BUS
    VehicleCategory.METRO -> return BecknV2.FRFS.Enums.METRO
    VehicleCategory.SUBWAY -> return BecknV2.FRFS.Enums.SUBWAY
    _ -> throwError $ InternalError "Invalid vehicle type"

findByRouteCodeAndStopCode ::
  ( MonadFlow m,
    CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  [Text] ->
  Text ->
  Bool ->
  Bool ->
  m [RouteStopTimeTable]
findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes' stopCode' needOnlyOneTrip subWayCached = do
  vehicleType <- castVehicleType integratedBPPConfig.vehicleCategory
  let routeCodes = P.map (modifyCodesToGTFS integratedBPPConfig) routeCodes'
      stopCode = modifyCodesToGTFS integratedBPPConfig stopCode'
  routeStopTimeTable <- Hedis.safeGet (routeTimeTableKey stopCode routeCodes needOnlyOneTrip)
  allTrips <-
    case (routeStopTimeTable, vehicleType == BecknV2.FRFS.Enums.SUBWAY, subWayCached) of
      (Just a, False, _) -> do
        logDebug $ "Fetched route stop time table cached: " <> show a <> "for routeCodes:" <> show routeCodes <> " and stopCode:" <> show stopCode
        pure a
      (Just a, True, True) -> do
        logDebug $ "Fetched route stop time table cached: " <> show a <> "for routeCodes:" <> show routeCodes <> " and stopCode:" <> show stopCode
        pure a
      _ -> do
        stopCodes <-
          P.map (modifyCodesToGTFS integratedBPPConfig)
            <$> case vehicleType of
              a | a `P.elem` [BecknV2.FRFS.Enums.METRO, BecknV2.FRFS.Enums.SUBWAY] -> do
                OTPRestCommon.getChildrenStationsCodes integratedBPPConfig stopCode'
                  >>= \case
                    [] -> pure [stopCode']
                    stopCodes@(_ : _) -> pure stopCodes
              _ -> pure [stopCode']
        allTrips <- Queries.findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes' stopCodes vehicleType needOnlyOneTrip
        unless (P.null allTrips) $ cacheRouteStopTimeInfo stopCode routeCodes allTrips needOnlyOneTrip
        pure allTrips
  val <- L.getOptionLocal CalledForFare
  return $ P.filter (\trip -> (trip.routeCode `P.elem` routeCodes') || (val == Just True)) allTrips

cacheRouteStopTimeInfo :: (CacheFlow m r, MonadFlow m) => Text -> [Text] -> [RouteStopTimeTable] -> Bool -> m ()
cacheRouteStopTimeInfo stopCode routeCodes routeStopInfo needOnlyOneTrip = do
  expTime <- liftIO $ fromMaybe 3600 . (>>= readMaybe) <$> Se.lookupEnv "ROUTE_STOP_TIMETABLE_CACHE_EXPIRY_SECONDS"
  let idKey = routeTimeTableKey stopCode routeCodes needOnlyOneTrip
  Hedis.setExp idKey routeStopInfo expTime

routeTimeTableKey :: Text -> [Text] -> Bool -> Text
routeTimeTableKey stopCode routeCodes needOnlyOneTrip = "routeStop-time-table:" <> stopCode <> ":" <> Text.intercalate ":" routeCodes <> ":" <> show needOnlyOneTrip

data CalledForFare = CalledForFare
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity CalledForFare Bool
