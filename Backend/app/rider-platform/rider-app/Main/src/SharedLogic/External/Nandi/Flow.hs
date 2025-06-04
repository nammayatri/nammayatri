module SharedLogic.External.Nandi.Flow where

import BecknV2.FRFS.Enums
import qualified Data.Map as Map
import Data.Text (splitOn)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.API.Nandi as NandiAPI
import SharedLogic.External.Nandi.Types
import System.Environment (lookupEnv)

getAllRoutePatterns :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m [NandiPattern]
getAllRoutePatterns baseUrl gtfsId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiPatterns gtfsId) "getAllRoutePatterns" NandiAPI.nandiPatternsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_PATTERNS_API") baseUrl)

getRoutePatternDetails :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m NandiPatternDetails
getRoutePatternDetails baseUrl routeId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetSpecificPattern routeId) "getRoutePatternDetails" NandiAPI.nandiGetSpecificPatternAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_SPECIFIC_PATTERN_API") baseUrl)

getRoutesFromNandi :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m [NandiRoutesRes]
getRoutesFromNandi baseUrl gtfsId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiRoutes gtfsId) "getRoutesFromNandi" NandiAPI.nandiRoutesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_ROUTES_API") baseUrl)

getRouteStopMappingByRouteCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByRouteCode baseUrl gtfsId routeCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByRouteId gtfsId routeCode) "getRouteStopMappingByRouteCode" NandiAPI.nandiGetRouteStopMappingByRouteIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_ROUTE_CODE_API") baseUrl)

getRouteStopMappingByStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByStopCode baseUrl gtfsId stopCode = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByStopCode gtfsId stopCode) "getRouteStopMappingByStopCode" NandiAPI.nandiGetRouteStopMappingByStopCodeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_STOP_CODE_API") baseUrl)

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

getRouteStopMapping :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m [RouteStopMappingNandi]
getRouteStopMapping baseUrl gtfsId = do
  env <- fromMaybe False . (>>= readMaybe) <$> liftIO (lookupEnv "ENABLE_INMEMORY_SERVER")
  unless env $ do
    logDebug "ENABLE_INMEMORY_SERVER is not set, skipping nandi rest api calls"
    throwError $ InternalError "ENABLE_INMEMORY_SERVER is not set, skipping nandi rest api calls"

  allRoutePatterns <- getAllRoutePatterns baseUrl gtfsId
  logDebug $ "allRoutePatterns from nandi rest api: " <> show allRoutePatterns
  allRouteDetails <- getRoutesFromNandi baseUrl gtfsId
  logDebug $ "allRouteDetails from nandi rest api: " <> show allRouteDetails
  let routePatternMap = Map.fromList [(p.routeId, p) | p <- allRoutePatterns]
      routeDetailsMap = Map.fromList [(r.id, r) | r <- allRouteDetails]
      matchingPatterns = Map.filterWithKey (\k _ -> k `elem` map (.routeId) allRoutePatterns) routePatternMap

  allMappings <- forM (Map.toList matchingPatterns) $ \(_, p) -> do
    patternDetails <- getRoutePatternDetails baseUrl p.id
    routeDetail <- fromMaybeM (InternalError ("ROUTE_NOT_FOUND" <> p.routeId)) $ Map.lookup p.routeId routeDetailsMap
    let stopMappings =
          zipWith
            ( \stop idx ->
                RouteStopMappingNandi
                  { dailyTripCount = Just $ length patternDetails.trips,
                    endPoint = maybe (Kernel.External.Maps.Types.LatLong 0 0) (\s -> Kernel.External.Maps.Types.LatLong s.lat s.lon) $ lastMay patternDetails.stops,
                    routeLongName = fromMaybe "sampleLongName" routeDetail.longName,
                    routeShortName = fromMaybe "sampleShortName" routeDetail.shortName,
                    startPoint = maybe (Kernel.External.Maps.Types.LatLong 0 0) (\s -> Kernel.External.Maps.Types.LatLong s.lat s.lon) $ headMay patternDetails.stops,
                    stopCount = Just $ length patternDetails.stops,
                    estimatedTravelTimeFromPreviousStop = Nothing, -- This would need to be calculated if available
                    routeCode = last $ splitOn ":" routeDetail.id,
                    sequenceNum = idx,
                    stopCode = last $ splitOn ":" stop.code,
                    stopName = stop.name,
                    stopPoint = Kernel.External.Maps.Types.LatLong stop.lat stop.lon,
                    vehicleType = caseTextToVehicleCategory routeDetail.mode
                  }
            )
            patternDetails.stops [1 ..]

    pure stopMappings

  let allMappingsList = concat allMappings
  logDebug $ "allMappingsList from nandi rest api: " <> show allMappingsList <> " length: " <> show (length allMappingsList)
  pure allMappingsList

caseTextToVehicleCategory :: Text -> BecknV2.FRFS.Enums.VehicleCategory
caseTextToVehicleCategory "BUS" = BecknV2.FRFS.Enums.BUS
caseTextToVehicleCategory "TRAIN" = BecknV2.FRFS.Enums.METRO
caseTextToVehicleCategory "SUBWAY" = BecknV2.FRFS.Enums.SUBWAY
caseTextToVehicleCategory _ = BecknV2.FRFS.Enums.BUS
