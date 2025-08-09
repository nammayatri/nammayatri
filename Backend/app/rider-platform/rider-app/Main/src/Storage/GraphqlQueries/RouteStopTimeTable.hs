{-
 Copyright 2025-26, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.GraphqlQueries.RouteStopTimeTable
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import BecknV2.FRFS.Enums (ServiceTierType (..), VehicleCategory (..))
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopTimeTable
import EulerHS.Prelude (concatMapM)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.GraphqlQueries.Client as Client

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
  [Text] ->
  VehicleCategory ->
  m [RouteStopTimeTable]
findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes stopCodes vehicleCategory = do
  concatMapM
    ( \stopCode -> do
        let variables =
              Client.RouteStopTimeTableQueryVars
                { Client.routeIds = routeCodes,
                  Client.stopId = stopCode
                }
        result <- Client.executeRouteStopTimeTableQuery integratedBPPConfig variables
        logDebug $ "GraphQL query result: " <> show result

        case result of
          Left err -> do
            logError $ "GraphQL query failed: " <> show err
            pure []
          Right response -> do
            pure $ concatMap (parseToRouteStopTimeTable integratedBPPConfig.id merchantId merchantOpId vehicleCategory) response.routeStopTimeTables
    )
    stopCodes

-- Helper function to convert GraphQL response to domain type
parseToRouteStopTimeTable ::
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  VehicleCategory ->
  Client.TimetableEntry ->
  [RouteStopTimeTable]
parseToRouteStopTimeTable integratedBPPConfigId mid mocid vehicleCategory entry =
  case vehicleCategory of
    SUBWAY ->
      [ RouteStopTimeTable
          { integratedBppConfigId = integratedBPPConfigId,
            routeCode = entry.routeCode,
            serviceTierType = SECOND_CLASS,
            stopCode = entry.stopCode,
            timeOfArrival = entry.timeOfArrival,
            timeOfDeparture = entry.timeOfDeparture,
            tripId = Id entry.tripId,
            merchantId = Just mid,
            merchantOperatingCityId = Just mocid,
            createdAt = entry.createdAt,
            updatedAt = entry.updatedAt,
            delay = Nothing,
            source = GTFS,
            stage = entry.stage,
            providerStopCode = entry.providerStopCode,
            platformCode = entry.platformCode,
            isStageStop = entry.isStageStop
          },
        RouteStopTimeTable
          { integratedBppConfigId = integratedBPPConfigId,
            routeCode = entry.routeCode,
            serviceTierType = FIRST_CLASS,
            stopCode = entry.stopCode,
            timeOfArrival = entry.timeOfArrival,
            timeOfDeparture = entry.timeOfDeparture,
            tripId = Id entry.tripId,
            merchantId = Just mid,
            merchantOperatingCityId = Just mocid,
            createdAt = entry.createdAt,
            updatedAt = entry.updatedAt,
            delay = Nothing,
            source = GTFS,
            stage = entry.stage,
            providerStopCode = entry.providerStopCode,
            platformCode = entry.platformCode,
            isStageStop = entry.isStageStop
          }
      ]
    _ ->
      [ RouteStopTimeTable
          { integratedBppConfigId = integratedBPPConfigId,
            routeCode = entry.routeCode,
            serviceTierType = entry.serviceTierType,
            stopCode = entry.stopCode,
            timeOfArrival = entry.timeOfArrival,
            timeOfDeparture = entry.timeOfDeparture,
            tripId = Id entry.tripId,
            merchantId = Just mid,
            merchantOperatingCityId = Just mocid,
            createdAt = entry.createdAt,
            updatedAt = entry.updatedAt,
            delay = Nothing,
            source = GTFS,
            stage = entry.stage,
            providerStopCode = entry.providerStopCode,
            platformCode = entry.platformCode,
            isStageStop = entry.isStageStop
          }
      ]
