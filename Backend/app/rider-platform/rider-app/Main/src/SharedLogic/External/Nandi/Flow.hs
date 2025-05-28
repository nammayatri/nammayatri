module SharedLogic.External.Nandi.Flow where

import BecknV2.FRFS.Enums
import qualified Data.Map as Map
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.API.Nandi as NandiAPI
import SharedLogic.External.Nandi.Types

getAllRoutePatterns :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> m [NandiPattern]
getAllRoutePatterns baseUrl = do
  withShortRetry $ callAPI baseUrl NandiAPI.getNandiPatterns "getAllRoutePatterns" NandiAPI.nandiPatternsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_PATTERNS_API") baseUrl)

getRoutePatternDetails :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> Text -> m NandiPatternDetails
getRoutePatternDetails baseUrl routeId = do
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetSpecificPattern routeId) "getRoutePatternDetails" NandiAPI.nandiGetSpecificPatternAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_SPECIFIC_PATTERN_API") baseUrl)

getRoutesFromNandi :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> m [NandiRoutesRes]
getRoutesFromNandi baseUrl = do
  withShortRetry $ callAPI baseUrl NandiAPI.getNandiRoutes "getRoutesFromNandi" NandiAPI.nandiRoutesAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_ROUTES_API") baseUrl)

getRouteStopMapping :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c) => BaseUrl -> m [RouteStopMappingNandi]
getRouteStopMapping baseUrl = do
  allRoutePatterns <- getAllRoutePatterns baseUrl
  allRouteDetails <- getRoutesFromNandi baseUrl

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
                    routeLongName = routeDetail.longName,
                    routeShortName = routeDetail.shortName,
                    startPoint = maybe (Kernel.External.Maps.Types.LatLong 0 0) (\s -> Kernel.External.Maps.Types.LatLong s.lat s.lon) $ headMay patternDetails.stops,
                    stopCount = Just $ length patternDetails.stops,
                    estimatedTravelTimeFromPreviousStop = Nothing, -- This would need to be calculated if available
                    routeCode = routeDetail.id,
                    sequenceNum = idx,
                    stopCode = stop.code,
                    stopName = stop.name,
                    stopPoint = Kernel.External.Maps.Types.LatLong stop.lat stop.lon,
                    vehicleType = caseTextToVehicleCategory routeDetail.mode
                  }
            )
            patternDetails.stops [1 ..]

    pure stopMappings

  let allMappingsList = concat allMappings

  pure allMappingsList

caseTextToVehicleCategory :: Text -> BecknV2.FRFS.Enums.VehicleCategory
caseTextToVehicleCategory "BUS" = BecknV2.FRFS.Enums.BUS
caseTextToVehicleCategory "TRAIN" = BecknV2.FRFS.Enums.METRO
caseTextToVehicleCategory "SUBWAY" = BecknV2.FRFS.Enums.SUBWAY
caseTextToVehicleCategory _ = BecknV2.FRFS.Enums.BUS
