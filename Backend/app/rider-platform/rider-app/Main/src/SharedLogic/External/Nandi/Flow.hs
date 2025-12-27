module SharedLogic.External.Nandi.Flow where

import BecknV2.FRFS.Enums
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.API.Nandi as NandiAPI
import SharedLogic.External.Nandi.Types

getRouteStopMappingByRouteCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByRouteCode baseUrl gtfsId routeCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByRouteId gtfsId routeCode) "getRouteStopMappingByRouteCode" NandiAPI.nandiGetRouteStopMappingByRouteIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_ROUTE_CODE_API") baseUrl)

getRouteStopMappingByStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByStopCode baseUrl gtfsId stopCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByStopCode gtfsId stopCode) "getRouteStopMappingByStopCode" NandiAPI.nandiGetRouteStopMappingByStopCodeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_STOP_CODE_API") baseUrl)

getRouteByRouteId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m (Maybe RouteInfoNandi)
getRouteByRouteId baseUrl gtfsId routeId = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiRouteByRouteId gtfsId routeId) "getRouteByRouteId" NandiAPI.nandiRouteByRouteIdAPI >>= \case
      Right route -> pure (Just route)
      Left err -> do
        logError $ "Error getting route by route id: " <> show err
        pure Nothing

getRoutesByRouteIds :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> [Text] -> m [RouteInfoNandi]
getRoutesByRouteIds baseUrl gtfsId routeIds = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiRoutesByRouteIds gtfsId routeIds) "getRoutesByRouteIds" NandiAPI.nandiRoutesByRouteIdsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTES_BY_ROUTE_IDS_API") baseUrl)

getRouteBusSchedule :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m BusScheduleDetails
getRouteBusSchedule baseUrl gtfsId routeId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiBusRouteSchedule gtfsId routeId) "getRouteBusSchedule" NandiAPI.nandiBusRouteScheduleAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_BUS_ROUTE_SCHEDULE_API") baseUrl)

getRouteByFuzzySearch :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m [RouteInfoNandi]
getRouteByFuzzySearch baseUrl gtfsId query = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiRouteFuzzySearch gtfsId query) "getRouteByFuzzySearch" NandiAPI.nandiRouteFuzzySearchAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_FUZZY_SEARCH_API") baseUrl)

getRoutesByGtfsId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> m [RouteInfoNandi]
getRoutesByGtfsId baseUrl gtfsId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiRoutesByGtfsId gtfsId) "getRoutesByGtfsId" NandiAPI.nandiRoutesByGtfsIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTES_BY_GTFS_ID_API") baseUrl)

getRouteStopMappingInMemoryServer :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Maybe Text -> Maybe Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingInMemoryServer baseUrl gtfsId routeCode' stopCode' = do
  case (routeCode', stopCode') of
    (Just routeCode, Just stopCode) -> do
      routeStopMapping <- getRouteStopMappingByRouteCode baseUrl gtfsId routeCode
      return $ filter (\r -> r.stopCode == stopCode) routeStopMapping
    (Just routeCode, Nothing) -> getRouteStopMappingByRouteCode baseUrl gtfsId routeCode
    (Nothing, Just stopCode) -> getRouteStopMappingByStopCode baseUrl gtfsId stopCode
    (Nothing, Nothing) -> do
      logError $ "routeCode or stopCode is not provided, skipping gtfs inmemory server rest api calls" <> show (baseUrl, gtfsId)
      throwError $ InternalError "routeCode or stopCode is not provided, skipping gtfs inmemory server rest api calls"

getStationsByGtfsId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> m [RouteStopMappingInMemoryServerWithPublicData]
getStationsByGtfsId baseUrl gtfsId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopsByGtfsId gtfsId) "getStationsByGtfsId" NandiAPI.nandiStopsByGtfsIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STATIONS_BY_GTFS_ID_API") baseUrl)

getStationsByGtfsIdAndStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m RouteStopMappingInMemoryServer
getStationsByGtfsIdAndStopCode baseUrl gtfsId stopCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopsByGtfsIdAndStopCode gtfsId stopCode) "getStationsByGtfsIdAndStopCode" NandiAPI.nandiStopsByGtfsIdAndStopCodeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STATIONS_BY_GTFS_ID_AND_STOP_CODE_API") baseUrl)

getStationsByGtfsIdFuzzySearch :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getStationsByGtfsIdFuzzySearch baseUrl gtfsId query = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopsByGtfsIdFuzzySearch gtfsId query) "getStationsByGtfsIdFuzzySearch" NandiAPI.nandiStopsByGtfsIdFuzzySearchAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STATIONS_BY_GTFS_ID_FUZZY_SEARCH_API") baseUrl)

getGtfsVersion :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> m Text
getGtfsVersion baseUrl gtfsId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGtfsVersion gtfsId) "getGtfsVersion" NandiAPI.nandiGtfsVersionAPI >>= fromEitherM (ExternalAPICallError (Just ("UNABLE_TO_CALL_NANDI_GET_GTFS_VERSION_API:" <> show gtfsId)) baseUrl)

getStopChildren :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m [Text]
getStopChildren baseUrl gtfsId stopCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopChildren gtfsId stopCode) "getStopChildren" NandiAPI.nandiStopChildrenAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STOP_CHILDREN_API") baseUrl)

caseTextToVehicleCategory :: Text -> BecknV2.FRFS.Enums.VehicleCategory
caseTextToVehicleCategory "BUS" = BecknV2.FRFS.Enums.BUS
caseTextToVehicleCategory "TRAIN" = BecknV2.FRFS.Enums.METRO
caseTextToVehicleCategory "SUBWAY" = BecknV2.FRFS.Enums.SUBWAY
caseTextToVehicleCategory _ = BecknV2.FRFS.Enums.BUS

getVehicleServiceType :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> Maybe Bool -> m (Maybe VehicleServiceTypeResponse)
getVehicleServiceType baseUrl gtfsId vehicleNumber mbPassVerifyReq = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiVehicleServiceType gtfsId vehicleNumber mbPassVerifyReq) "getVehicleServiceType" NandiAPI.nandiVehicleServiceTypeAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting vehicle service type: " <> show err
        pure Nothing

getVehiclesByServiceType :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> ServiceTierType -> m [Text]
getVehiclesByServiceType baseUrl gtfsId serviceType = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiVehiclesByServiceType gtfsId (show serviceType)) "getVehiclesByServiceType" NandiAPI.nandiVehiclesByServiceTypeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_VEHICLES_BY_SERVICE_TYPE_API") baseUrl)

getVehicleInfo :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m (Maybe VehicleInfoResponse)
getVehicleInfo baseUrl gtfsId vehicleNumber = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiVehicleInfo gtfsId vehicleNumber) "getVehicleInfo" NandiAPI.nandiVehicleInfoAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting vehicle info: " <> show err
        pure Nothing

getVehicleOperationInfo :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> m (Maybe VehicleOperationInfo)
getVehicleOperationInfo baseUrl fleetNo = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiVehicleOperationData fleetNo) "getVehicleOperationInfo" NandiAPI.nandiVehicleOperationDataAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting vehicle operation info: " <> show err
        pure Nothing

postGtfsGraphQL :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> GtfsGraphQLRequest -> m Value
postGtfsGraphQL baseUrl request = do
  withShortRetry $ callAPI baseUrl (NandiAPI.postNandiGtfsGraphQL request) "postGtfsGraphQL" NandiAPI.nandiGtfsGraphQLAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_POST_GTFS_GRAPHQL_API") baseUrl)

getStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m (Maybe StopCodeResponse)
getStopCode baseUrl gtfsId providerStopCode = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiStopCode gtfsId providerStopCode) "getStopCode" NandiAPI.nandiStopCodeAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting stop code: " <> show err
        pure Nothing

getNandiTripInfo :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> m (Maybe TripInfoResponse)
getNandiTripInfo baseUrl tripId = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiTripInfo tripId) "getNandiTripInfo" NandiAPI.nandiTripInfoAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting trip info: " <> show err
        pure Nothing

postRouteStopMappingByStopCodes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> [Text] -> m [RouteStopMappingInMemoryServer]
postRouteStopMappingByStopCodes baseUrl gtfsId stopCodes = do
  withShortRetry $ callAPI baseUrl (NandiAPI.postNandiRouteStopMappingByStopCodes (RouteStopMappingByStopCodesReq {..})) "postRouteStopMappingByStopCodes" NandiAPI.nandiRouteStopMappingByStopCodesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_POST_ROUTE_STOP_MAPPING_BY_STOP_CODES_API") baseUrl)

getExampleTrip :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m (Maybe TripDetails)
getExampleTrip baseUrl gtfsId routeId = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiExampleTrip gtfsId routeId) "getExampleTrip" NandiAPI.nandiExampleTripAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting example trip: " <> show err
        pure Nothing

getDepotNames :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> m [Text]
getDepotNames baseUrl = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiDepotNames) "getDepotNames" NandiAPI.nandiDepotNamesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_DEPOT_NAMES_API") baseUrl)

getDepotIds :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> m [Text]
getDepotIds baseUrl = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiDepotIds) "getDepotIds" NandiAPI.nandiDepotIdsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_DEPOT_IDS_API") baseUrl)

getVehiclesFromByDepotName :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Maybe Text -> m [DepotVehicle]
getVehiclesFromByDepotName baseUrl depotName = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetVehiclesFromByDepotName depotName) "getVehiclesFromByDepotName" NandiAPI.nandiGetVehiclesFromDepotNameAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_VEHICLES_FROM_BY_DEPOT_NAME_API") baseUrl)

getVehiclesFromByDepotId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Maybe Text -> m [DepotVehicle]
getVehiclesFromByDepotId baseUrl depotId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetVehiclesFromByDepotId depotId) "getVehiclesFromByDepotId" NandiAPI.nandiGetVehiclesFromDepotIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_VEHICLES_FROM_BY_DEPOT_ID_API") baseUrl)

getDepotNameById :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> m Text
getDepotNameById baseUrl depotId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiDepotNameById depotId) "getDepotNameById" NandiAPI.nandiDepotNameByIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_DEPOT_NAME_BY_ID_API") baseUrl)
