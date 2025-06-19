module SharedLogic.External.Nandi.Flow where

import BecknV2.FRFS.Enums
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.API.Nandi as NandiAPI
import SharedLogic.External.Nandi.Types

getRouteStopMappingByRouteCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByRouteCode baseUrl gtfsId routeCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByRouteId gtfsId routeCode) "getRouteStopMappingByRouteCode" NandiAPI.nandiGetRouteStopMappingByRouteIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_ROUTE_CODE_API") baseUrl)

getRouteStopMappingByStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByStopCode baseUrl gtfsId stopCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByStopCode gtfsId stopCode) "getRouteStopMappingByStopCode" NandiAPI.nandiGetRouteStopMappingByStopCodeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_STOP_CODE_API") baseUrl)

getRouteByRouteId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m (Maybe RouteInfoNandi)
getRouteByRouteId baseUrl gtfsId routeId = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiRouteByRouteId gtfsId routeId) "getRouteByRouteId" NandiAPI.nandiRouteByRouteIdAPI >>= \case
      Right route -> pure (Just route)
      Left err -> do
        logError $ "Error getting route by route id: " <> show err
        pure Nothing

getRouteByFuzzySearch :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m [RouteInfoNandi]
getRouteByFuzzySearch baseUrl gtfsId query = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiRouteFuzzySearch gtfsId query) "getRouteByFuzzySearch" NandiAPI.nandiRouteFuzzySearchAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_FUZZY_SEARCH_API") baseUrl)

getRoutesByGtfsId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m [RouteInfoNandi]
getRoutesByGtfsId baseUrl gtfsId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiRoutesByGtfsId gtfsId) "getRoutesByGtfsId" NandiAPI.nandiRoutesByGtfsIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTES_BY_GTFS_ID_API") baseUrl)

getRouteStopMappingInMemoryServer :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Maybe Text -> Maybe Text -> m [RouteStopMappingInMemoryServer]
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

getStationsByGtfsId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m [RouteStopMappingInMemoryServer]
getStationsByGtfsId baseUrl gtfsId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopsByGtfsId gtfsId) "getStationsByGtfsId" NandiAPI.nandiStopsByGtfsIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STATIONS_BY_GTFS_ID_API") baseUrl)

getStationsByGtfsIdAndStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m RouteStopMappingInMemoryServer
getStationsByGtfsIdAndStopCode baseUrl gtfsId stopCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopsByGtfsIdAndStopCode gtfsId stopCode) "getStationsByGtfsIdAndStopCode" NandiAPI.nandiStopsByGtfsIdAndStopCodeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STATIONS_BY_GTFS_ID_AND_STOP_CODE_API") baseUrl)

getStationsByGtfsIdFuzzySearch :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getStationsByGtfsIdFuzzySearch baseUrl gtfsId query = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopsByGtfsIdFuzzySearch gtfsId query) "getStationsByGtfsIdFuzzySearch" NandiAPI.nandiStopsByGtfsIdFuzzySearchAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STATIONS_BY_GTFS_ID_FUZZY_SEARCH_API") baseUrl)

caseTextToVehicleCategory :: Text -> BecknV2.FRFS.Enums.VehicleCategory
caseTextToVehicleCategory "BUS" = BecknV2.FRFS.Enums.BUS
caseTextToVehicleCategory "TRAIN" = BecknV2.FRFS.Enums.METRO
caseTextToVehicleCategory "SUBWAY" = BecknV2.FRFS.Enums.SUBWAY
caseTextToVehicleCategory _ = BecknV2.FRFS.Enums.BUS

getVehicleServiceType :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m (Maybe VehicleServiceTypeResponse)
getVehicleServiceType baseUrl vehicleNumber = do
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiVehicleServiceType vehicleNumber) "getVehicleServiceType" NandiAPI.nandiVehicleServiceTypeAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting vehicle service type: " <> show err
        pure Nothing
