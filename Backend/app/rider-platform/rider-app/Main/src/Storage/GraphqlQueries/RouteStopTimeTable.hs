module Storage.GraphqlQueries.RouteStopTimeTable
  ( findByRouteCodeAndStopCode,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType (..), VehicleCategory (..))
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopTimeTable
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.GraphqlQueries.Client as Client
import Tools.MultiModal

findByRouteCodeAndStopCode ::
  ( MonadFlow m,
    ServiceFlow m r
  ) =>
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  [Text] ->
  Text ->
  VehicleCategory ->
  m [RouteStopTimeTable]
findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes stopCode vehicleCategory = do
  let variables =
        Client.RouteStopTimeTableQueryVars
          { Client.routeCode = routeCodes,
            Client.stopCode = stopCode
          }
  transitreq <- getTransitServiceReq merchantId merchantOpId
  baseUrl <- case transitreq of
    MultiModalTypes.OTPTransitConfig cfg -> return cfg.baseUrl
    _ -> throwError $ InternalError "Transit service request is not OTPTransitConfig"
  result <- Client.executeRouteStopTimeTableQuery baseUrl variables
  logDebug $ "GraphQL query result: " <> show result

  case result of
    Left err -> do
      logError $ "GraphQL query failed: " <> show err
      pure []
    Right response -> do
      pure $ concatMap (parseToRouteStopTimeTable integratedBPPConfig merchantId merchantOpId vehicleCategory) response.routeStopTimeTables

-- Helper function to convert GraphQL response to domain type
parseToRouteStopTimeTable ::
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  VehicleCategory ->
  Client.TimetableEntry ->
  [RouteStopTimeTable]
parseToRouteStopTimeTable integratedBPPConfig mid mocid vehicleCategory entry =
  case vehicleCategory of
    SUBWAY ->
      [ RouteStopTimeTable
          { integratedBppConfigId = integratedBPPConfig,
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
            stage = entry.stage
          },
        RouteStopTimeTable
          { integratedBppConfigId = integratedBPPConfig,
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
            stage = entry.stage
          }
      ]
    _ ->
      [ RouteStopTimeTable
          { integratedBppConfigId = integratedBPPConfig,
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
            stage = entry.stage
          }
      ]
