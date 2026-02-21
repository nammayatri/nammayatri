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

getAlternateStationsByGtfsIdAndStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getAlternateStationsByGtfsIdAndStopCode baseUrl gtfsId stopCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiAlternateStopsByGtfsIdAndStopCode gtfsId stopCode) "getNandiAlternateStopsByGtfsIdAndStopCode" NandiAPI.nandiAlternateStopsByGtfsIdAndStopCodeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ALTERNATE_STATIONS_BY_GTFS_ID_AND_STOP_CODE_API") baseUrl)

operatorGetRow :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> Maybe Text -> m Value
operatorGetRow baseUrl gtfsId table column =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorRow gtfsId table column) "operatorGetRow" NandiAPI.operatorGetRowAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_GET_ROW_API") baseUrl)

operatorGetAllRows :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> Maybe Int -> Maybe Int -> m [Value]
operatorGetAllRows baseUrl gtfsId table limit offset =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorAllRows gtfsId table limit offset) "operatorGetAllRows" NandiAPI.operatorGetAllRowsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_GET_ALL_ROWS_API") baseUrl)

operatorDeleteRow :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> Value -> m RowsAffectedResp
operatorDeleteRow baseUrl gtfsId table pkValue =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorDeleteRow gtfsId table pkValue) "operatorDeleteRow" NandiAPI.operatorDeleteRowAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_DELETE_ROW_API") baseUrl)

operatorUpsertRow :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> Value -> m Value
operatorUpsertRow baseUrl gtfsId table body =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorUpsertRow gtfsId table body) "operatorUpsertRow" NandiAPI.operatorUpsertRowAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_UPSERT_ROW_API") baseUrl)

operatorServiceTypes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [ServiceType]
operatorServiceTypes baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorServiceTypes gtfsId) "operatorServiceTypes" NandiAPI.operatorServiceTypesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_SERVICE_TYPES_API") baseUrl)

operatorRoutes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [NandiRoute]
operatorRoutes baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorRoutes gtfsId) "operatorRoutes" NandiAPI.operatorRoutesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_ROUTES_API") baseUrl)

operatorDepots :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Depot]
operatorDepots baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorDepots gtfsId) "operatorDepots" NandiAPI.operatorDepotsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_DEPOTS_API") baseUrl)

operatorShiftTypes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Text]
operatorShiftTypes baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorShiftTypes gtfsId) "operatorShiftTypes" NandiAPI.operatorShiftTypesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_SHIFT_TYPES_API") baseUrl)

operatorScheduleNumbers :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [ScheduleNumber]
operatorScheduleNumbers baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorScheduleNumbers gtfsId) "operatorScheduleNumbers" NandiAPI.operatorScheduleNumbersAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_SCHEDULE_NUMBERS_API") baseUrl)

operatorDayTypes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Text]
operatorDayTypes baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorDayTypes gtfsId) "operatorDayTypes" NandiAPI.operatorDayTypesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_DAY_TYPES_API") baseUrl)

operatorTripTypes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Text]
operatorTripTypes baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorTripTypes gtfsId) "operatorTripTypes" NandiAPI.operatorTripTypesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_TRIP_TYPES_API") baseUrl)

operatorBreakTypes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Text]
operatorBreakTypes baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorBreakTypes gtfsId) "operatorBreakTypes" NandiAPI.operatorBreakTypesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_BREAK_TYPES_API") baseUrl)

operatorTripDetails :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m [NandiTripDetail]
operatorTripDetails baseUrl gtfsId scheduleNumber =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorTripDetails gtfsId (Just scheduleNumber)) "operatorTripDetails" NandiAPI.operatorTripDetailsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_TRIP_DETAILS_API") baseUrl)

operatorFleets :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Fleet]
operatorFleets baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorFleets gtfsId) "operatorFleets" NandiAPI.operatorFleetsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_FLEETS_API") baseUrl)

operatorConductors :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m Employee
operatorConductors baseUrl gtfsId token =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorConductors gtfsId (Just token)) "operatorConductors" NandiAPI.operatorConductorsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_CONDUCTORS_API") baseUrl)

operatorDrivers :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m Employee
operatorDrivers baseUrl gtfsId token =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorDrivers gtfsId (Just token)) "operatorDrivers" NandiAPI.operatorDriversAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_DRIVERS_API") baseUrl)

operatorDeviceIds :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Text]
operatorDeviceIds baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorDeviceIds gtfsId) "operatorDeviceIds" NandiAPI.operatorDeviceIdsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_DEVICE_IDS_API") baseUrl)

operatorTabletIds :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> m [Text]
operatorTabletIds baseUrl gtfsId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorTabletIds gtfsId) "operatorTabletIds" NandiAPI.operatorTabletIdsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_TABLET_IDS_API") baseUrl)

operatorOperators :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m [Employee]
operatorOperators baseUrl gtfsId role =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorOperators gtfsId (Just role)) "operatorOperators" NandiAPI.operatorOperatorsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_OPERATORS_API") baseUrl)

operatorWaybillStatus :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> UpdateWaybillStatusReq -> m RowsAffectedResp
operatorWaybillStatus baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorWaybillStatus gtfsId req) "operatorWaybillStatus" NandiAPI.operatorWaybillStatusAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_WAYBILL_STATUS_API") baseUrl)

operatorWaybillFleet :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> UpdateWaybillFleetReq -> m RowsAffectedResp
operatorWaybillFleet baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorWaybillFleet gtfsId req) "operatorWaybillFleet" NandiAPI.operatorWaybillFleetAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_WAYBILL_FLEET_API") baseUrl)

operatorWaybillTablet :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> UpdateWaybillTabletReq -> m RowsAffectedResp
operatorWaybillTablet baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorWaybillTablet gtfsId req) "operatorWaybillTablet" NandiAPI.operatorWaybillTabletAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_WAYBILL_TABLET_API") baseUrl)

operatorWaybills :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Maybe Int -> Maybe Int -> m [Value]
operatorWaybills baseUrl gtfsId limit offset =
  withShortRetry $ callAPI baseUrl (NandiAPI.getOperatorWaybills gtfsId limit offset) "operatorWaybills" NandiAPI.operatorWaybillsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_OPERATOR_WAYBILLS_API") baseUrl)