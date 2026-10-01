module Lib.GtfsDataServer.Flow where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.GtfsDataServer.API.Nandi as NandiAPI
import Lib.GtfsDataServer.Types
import qualified Network.HTTP.Types.Status as HttpStatus
import Servant.Client (ClientError (..), ResponseF (..))

getRouteStopMappingByRouteCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByRouteCode baseUrl gtfsId routeCode =
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByRouteId gtfsId routeCode) "getRouteStopMappingByRouteCode" NandiAPI.nandiGetRouteStopMappingByRouteIdAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_ROUTE_CODE_API") baseUrl)

getRouteStopMappingByStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingByStopCode baseUrl gtfsId stopCode =
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiGetRouteStopMappingByStopCode gtfsId stopCode) "getRouteStopMappingByStopCode" NandiAPI.nandiGetRouteStopMappingByStopCodeAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_ROUTE_STOP_MAPPING_BY_STOP_CODE_API") baseUrl)

getRouteByRouteId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m (Maybe RouteInfoNandi)
getRouteByRouteId baseUrl gtfsId routeId =
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiRouteByRouteId gtfsId routeId) "getRouteByRouteId" NandiAPI.nandiRouteByRouteIdAPI >>= \case
      Right route -> pure (Just route)
      Left err -> do
        logError $ "Error getting route by route id: " <> show err
        pure Nothing

getRouteStopMappingInMemoryServer :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Maybe Text -> Maybe Text -> m [RouteStopMappingInMemoryServer]
getRouteStopMappingInMemoryServer baseUrl gtfsId routeCode' stopCode' =
  case (routeCode', stopCode') of
    (Just routeCode, Just stopCode) -> do
      rsm <- getRouteStopMappingByRouteCode baseUrl gtfsId routeCode
      return $ filter (\r -> r.stopCode == stopCode) rsm
    (Just routeCode, Nothing) -> getRouteStopMappingByRouteCode baseUrl gtfsId routeCode
    (Nothing, Just stopCode) -> getRouteStopMappingByStopCode baseUrl gtfsId stopCode
    (Nothing, Nothing) -> do
      logError $ "routeCode or stopCode is not provided, skipping gtfs inmemory server rest api calls" <> show (baseUrl, gtfsId)
      throwError $ InternalError "routeCode or stopCode is not provided"

getExampleTrip :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m (Maybe TripDetails)
getExampleTrip baseUrl gtfsId routeId =
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiExampleTrip gtfsId routeId) "getExampleTrip" NandiAPI.nandiExampleTripAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting example trip: " <> show err
        pure Nothing

getBusTripSchedule :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> Int -> Text -> m BusScheduleDetails
getBusTripSchedule baseUrl gtfsId waybillNo tripNumber routeId =
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiBusTripSchedule gtfsId waybillNo tripNumber routeId) "getBusTripSchedule" NandiAPI.nandiBusTripScheduleAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_BUS_TRIP_SCHEDULE_API") baseUrl)

gimsCurrentOperation :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> GimsOperationAnchor -> m GimsCurrentOperationResp
gimsCurrentOperation baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorCurrentOperation gtfsId req) "gimsCurrentOperation" NandiAPI.operatorCurrentOperationAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_GIMS_CURRENT_OPERATION_API") baseUrl)

gimsTripAction :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> GimsTripActionReq -> m Value
gimsTripAction baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorTripAction gtfsId req) "gimsTripAction" NandiAPI.operatorTripActionAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_GIMS_TRIP_ACTION_API") baseUrl)

gimsCurrentTripDetails :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> GimsCurrentTripDetailsReq -> m GimsCurrentTripDetailsResp
gimsCurrentTripDetails baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorCurrentTripDetails gtfsId req) "gimsCurrentTripDetails" NandiAPI.operatorCurrentTripDetailsAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_GIMS_CURRENT_TRIP_DETAILS_API") baseUrl)

-- | Fails open (Nothing) rather than throwing -- callers fall back to a client-supplied tripId/routeId
-- when GIMS is slow/unreachable, so a GIMS blip degrades to stale-but-working, not broken.
gimsActiveTrip :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> GimsOperationAnchor -> m (Maybe GimsActiveTripResp)
gimsActiveTrip baseUrl gtfsId req =
  withShortRetry $
    callAPI baseUrl (NandiAPI.postOperatorActiveTrip gtfsId req) "gimsActiveTrip" NandiAPI.operatorActiveTripAPI >>= \case
      Right resp -> pure (Just resp)
      Left err -> do
        logError $ "Error getting active trip: " <> show err
        pure Nothing

gimsEmployeeLogin :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> GimsEmployeeLoginReq -> m GimsEmployeeLoginResp
gimsEmployeeLogin baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorEmployeeLogin gtfsId req) "gimsEmployeeLogin" NandiAPI.operatorEmployeeLoginAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_GIMS_EMPLOYEE_LOGIN_API") baseUrl)

getStopChildren :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m [Text]
getStopChildren baseUrl gtfsId stopCode =
  withShortRetry $ callAPI baseUrl (NandiAPI.getNandiStopChildren gtfsId stopCode) "getStopChildren" NandiAPI.stopChildrenAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_NANDI_GET_STOP_CHILDREN_API") baseUrl)

getStopCode :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Text -> m (Maybe StopCodeResponse)
getStopCode baseUrl gtfsId providerStopCode =
  withShortRetry $
    callAPI baseUrl (NandiAPI.getNandiStopCode gtfsId providerStopCode) "getStopCode" NandiAPI.stopCodeAPI >>= \case
      Right response -> pure (Just response)
      Left err -> do
        logError $ "Error getting stop code: " <> show err
        pure Nothing

gimsVerifyConductor :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> GimsVerifyReq -> m GimsVerifyResp
gimsVerifyConductor baseUrl gtfsId req =
  withShortRetry $ callAPI baseUrl (NandiAPI.postOperatorVerify gtfsId req) "gimsVerifyConductor" NandiAPI.operatorVerifyAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_GIMS_VERIFY_CONDUCTOR_API") baseUrl)

-- ─── transitV2 ─────────────────────────────────────────────────────────────

-- | GIMS answers a refused action (wrong trip order, overlap, not found) with 4xx and
-- @{"error": "<message>"}@. Surface that message as an InvalidRequest so the driver / ops sees
-- why; anything else stays an ExternalAPICallError.
gimsV2Result :: (MonadThrow m, Log m) => Text -> BaseUrl -> Either ClientError a -> m a
gimsV2Result errCode baseUrl = \case
  Right a -> pure a
  Left err@(FailureResponse _ Response {responseStatusCode = status, responseBody = body})
    | code >= 400 && code < 500,
      Just msg <- gimsErrorMessage body ->
      throwError $ InvalidRequest msg
    | otherwise -> throwError $ ExternalAPICallError (Just errCode) baseUrl err
    where
      code = HttpStatus.statusCode status
  Left err -> throwError $ ExternalAPICallError (Just errCode) baseUrl err
  where
    gimsErrorMessage :: LBS.ByteString -> Maybe Text
    gimsErrorMessage b = case A.decode b of
      Just (A.Object o) | Just (A.String m) <- KeyMap.lookup "error" o -> Just m
      _ -> Nothing

-- | Not retried: an action must not be applied twice. Returns the run after the action.
gimsV2TripAction :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasRequestId r) => BaseUrl -> Text -> Maybe Text -> Maybe Text -> GimsV2TripActionReq -> m GimsV2CurrentOperationResp
gimsV2TripAction baseUrl gtfsId mbOperatorId mbActorId req =
  callAPI baseUrl (NandiAPI.postGimsV2TripAction gtfsId mbOperatorId mbActorId req) "gimsV2TripAction" NandiAPI.gimsV2TripActionAPI
    >>= gimsV2Result "UNABLE_TO_CALL_GIMS_V2_TRIP_ACTION_API" baseUrl

gimsV2CurrentOperation :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Maybe Text -> GimsV2Anchor -> m GimsV2CurrentOperationResp
gimsV2CurrentOperation baseUrl gtfsId mbOperatorId anchor =
  withShortRetry (callAPI baseUrl (NandiAPI.postGimsV2CurrentOperation gtfsId mbOperatorId anchor) "gimsV2CurrentOperation" NandiAPI.gimsV2CurrentOperationAPI)
    >>= gimsV2Result "UNABLE_TO_CALL_GIMS_V2_CURRENT_OPERATION_API" baseUrl

-- | Fails open (Nothing), like 'gimsActiveTrip'.
gimsV2ActiveTrip :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, HasRequestId r) => BaseUrl -> Text -> Maybe Text -> GimsV2Anchor -> m (Maybe GimsV2ActiveTripResp)
gimsV2ActiveTrip baseUrl gtfsId mbOperatorId anchor =
  withShortRetry (callAPI baseUrl (NandiAPI.postGimsV2ActiveTrip gtfsId mbOperatorId anchor) "gimsV2ActiveTrip" NandiAPI.gimsV2ActiveTripAPI) >>= \case
    Right resp -> pure (Just resp)
    Left err -> do
      logError $ "Error getting v2 active trip: " <> show err
      pure Nothing
