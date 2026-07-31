{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Domain.Action.UI.TransitOperator where

import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Data.Map.Strict as Map
import Domain.Action.UI.TransitOperator.Validation (preprocessUpsertBody, preprocessUpsertBodyAtIdx)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant (Merchant)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (ShortId (..))
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.External.Nandi.Flow as NandiFlow
import SharedLogic.External.Nandi.Types
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.MultiModal as MM
import qualified Tools.Notifications as Notifications

resolveBaseUrlAndGtfsId :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow (BaseUrl, Text)
resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory = do
  merchantOpCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

  let opCityId = merchantOpCity.id
  bppConfig <-
    SIBC.findIntegratedBPPConfig Nothing opCityId vehicleCategory DIBC.MULTIMODAL

  baseUrl <- MM.getOTPRestServiceReq bppConfig.merchantId opCityId
  pure (baseUrl, bppConfig.feedKey)

transitOperatorUnblockBusUtil :: ShortId Merchant -> Context.City -> Text -> Flow Kernel.Types.APISuccess.APISuccess
transitOperatorUnblockBusUtil merchantShortId city vehicleNumber = do
  merchantOpCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  configs <- SIBC.findAllIntegratedBPPConfig merchantOpCity.id BecknSpec.BUS DIBC.MULTIMODAL
  forM_ configs $ \cfg -> Hedis.del (cfg.id.getId <> ":blocked:" <> vehicleNumber)
  pure Kernel.Types.APISuccess.Success

transitOperatorGetRowUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> NandiTable -> Maybe Text -> Flow NandiRow
transitOperatorGetRowUtil merchantShortId city vehicleCategory table column = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorGetRow baseUrl gtfsId table column

transitOperatorGetAllRowsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> NandiTable -> Maybe Int -> Maybe Int -> Flow [NandiRow]
transitOperatorGetAllRowsUtil merchantShortId city vehicleCategory table limit offset = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorGetAllRows baseUrl gtfsId table limit offset

transitOperatorDeleteRowUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> NandiTable -> Value -> Flow RowsAffectedResp
transitOperatorDeleteRowUtil merchantShortId city vehicleCategory table pkValue = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDeleteRow baseUrl gtfsId table pkValue

transitOperatorUpsertRowUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> NandiTable -> Maybe Text -> Value -> Flow NandiRow
transitOperatorUpsertRowUtil merchantShortId city vehicleCategory table toRegen body = do
  processedBody <- preprocessUpsertBody table body
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorUpsertRow baseUrl gtfsId table toRegen processedBody

transitOperatorUpsertRowsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> NandiTable -> Maybe Text -> [Value] -> Flow [NandiRow]
transitOperatorUpsertRowsUtil merchantShortId city vehicleCategory table toRegen bodies = do
  when (length bodies > 500) $
    throwError $ InvalidRequest "Batch size exceeds maximum of 500 rows per request"
  if null bodies
    then pure []
    else do
      processedBodies <- zipWithM (preprocessUpsertBodyAtIdx table) [0 ..] bodies
      (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
      NandiFlow.operatorUpsertRows baseUrl gtfsId table toRegen processedBodies

transitOperatorGetServiceTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [ServiceType]
transitOperatorGetServiceTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorServiceTypes baseUrl gtfsId

transitOperatorGetRoutesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [NandiRoute]
transitOperatorGetRoutesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorRoutes baseUrl gtfsId

transitOperatorGetDepotsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Depot]
transitOperatorGetDepotsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDepots baseUrl gtfsId

transitOperatorGetShiftTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [ShiftType]
transitOperatorGetShiftTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorShiftTypes baseUrl gtfsId

transitOperatorGetScheduleNumbersUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [ScheduleNumber]
transitOperatorGetScheduleNumbersUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorScheduleNumbers baseUrl gtfsId

transitOperatorGetDayTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [DayType]
transitOperatorGetDayTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDayTypes baseUrl gtfsId

transitOperatorGetTripTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [TripType]
transitOperatorGetTripTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorTripTypes baseUrl gtfsId

transitOperatorGetBreakTypesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [BreakType]
transitOperatorGetBreakTypesUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorBreakTypes baseUrl gtfsId

transitOperatorGetTripDetailsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow [NandiTripDetail]
transitOperatorGetTripDetailsUtil merchantShortId city vehicleCategory scheduleNumber = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorTripDetails baseUrl gtfsId scheduleNumber

transitOperatorGetFleetsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Fleet]
transitOperatorGetFleetsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorFleets baseUrl gtfsId

transitOperatorGetConductorUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow Employee
transitOperatorGetConductorUtil merchantShortId city vehicleCategory token = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorConductors baseUrl gtfsId token

transitOperatorGetDriverUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow Employee
transitOperatorGetDriverUtil merchantShortId city vehicleCategory token = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDrivers baseUrl gtfsId token

transitOperatorGetDeviceIdsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetDeviceIdsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDeviceIds baseUrl gtfsId

transitOperatorGetTabletIdsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [Text]
transitOperatorGetTabletIdsUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorTabletIds baseUrl gtfsId

transitOperatorGetOperatorsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> OperatorRole -> Flow [Employee]
transitOperatorGetOperatorsUtil merchantShortId city vehicleCategory role = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorOperators baseUrl gtfsId role

transitOperatorUpdateWaybillStatusUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> UpdateWaybillStatusReq -> Flow RowsAffectedResp
transitOperatorUpdateWaybillStatusUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybillStatus baseUrl gtfsId req

transitOperatorUpdateWaybillFleetUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> UpdateWaybillFleetReq -> Flow RowsAffectedResp
transitOperatorUpdateWaybillFleetUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybillFleet baseUrl gtfsId req

-- | Granular update of a waybill's mutable operational fields (crew/fleet/devices/status). After the GIMS
-- write, reflects the driver/fleet change on affected customer tickets. The ticket refresh runs
-- synchronously (critical: tickets must be updated before we return); only the customer notifications are
-- forked (best-effort, can be slow).
transitOperatorUpdateWaybillDetailsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> UpdateWaybillDetailsReq -> Flow RowsAffectedResp
transitOperatorUpdateWaybillDetailsUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  res <- NandiFlow.operatorWaybillDetails baseUrl gtfsId req
  fanOutWaybillRefresh baseUrl gtfsId req.waybill_no
  pure res

-- | Reflect a waybill fleet/driver change on the customer tickets riding that waybill: for every confirmed
-- booking on the waybill, refresh its driver + (assigned) bus from freshly-fetched waybill metadata. The
-- metadata is read directly via NandiFlow (bypassing the 30s OTPRest in-mem cache) since the operator's
-- write just landed. The refresh (persisting to booking/leg) runs synchronously here; the per-booking
-- customer notification is forked (best-effort). A lookup/apply failure for one booking never aborts the rest.
fanOutWaybillRefresh :: BaseUrl -> Text -> Text -> Flow ()
fanOutWaybillRefresh baseUrl gtfsId waybillNo = do
  bookings <- QFRFSTicketBooking.findAllConfirmedByWaybillNo waybillNo
  unless (null bookings) $ do
    eMeta <- withTryCatch "fanOutWaybillRefresh:getWaybillMetadata" (NandiFlow.getWaybillMetadata baseUrl gtfsId waybillNo)
    case eMeta of
      Left err -> logError $ "fanOutWaybillRefresh: metadata fetch failed for waybillNo=" <> waybillNo <> ": " <> show err
      Right meta -> do
        legs <- QJourneyLeg.findAllByLegSearchIds (map (\b -> b.searchId.getId) bookings)
        let legMap = Map.fromList $ mapMaybe (\l -> (,l) <$> l.legSearchId) legs
        -- Batch-load the riders up front so the per-booking notification below needs no query in the loop.
        persons <- QP.findAllByIds (map (.riderId) bookings)
        let personMap = Map.fromList $ map (\p -> (p.id, p)) persons
        forM_ bookings $ \booking -> do
          let mbJourneyLeg = Map.lookup booking.searchId.getId legMap
          eRefresh <- withTryCatch ("fanOutWaybillRefresh:apply:" <> booking.id.getId) $ JMU.applyWaybillMetadataToTicket booking mbJourneyLeg meta
          case eRefresh of
            Left err -> logError $ "fanOutWaybillRefresh: apply failed for booking " <> booking.id.getId <> ": " <> show err
            Right refreshInfo ->
              -- Notify only when the driver and/or the assigned bus actually changed. The notification
              -- (FCM push + external WhatsApp) is slow and best-effort, so it is forked out of the
              -- critical, synchronous refresh path above.
              when (refreshInfo.driverChanged || refreshInfo.busChanged) $
                whenJust (Map.lookup booking.riderId personMap) $ \person -> do
                  let vehicleNo = fromMaybe "" refreshInfo.finalBoardedBusNumber
                      -- Label the trip as "<fromStop> - <toStop>" (stop names, falling back to codes) instead
                      -- of the raw route name.
                      routeName = fromMaybe booking.fromStationCode booking.fromStationName <> " - " <> fromMaybe booking.toStationCode booking.toStationName
                      mbJourneyId = (.journeyId) <$> mbJourneyLeg
                  fork ("fanOutWaybillRefresh:notify:" <> booking.id.getId) $ do
                    eNotify <- withTryCatch ("fanOutWaybillRefresh:notify:" <> booking.id.getId) $ Notifications.notifyFrfsTripDetailsUpdated person booking.id.getId vehicleNo routeName booking.tripId mbJourneyId refreshInfo.driverChanged refreshInfo.busChanged
                    case eNotify of
                      Left err -> logError $ "fanOutWaybillRefresh: notify failed for booking " <> booking.id.getId <> ": " <> show err
                      Right _ -> pure ()

transitOperatorUpdateWaybillTabletUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> UpdateWaybillTabletReq -> Flow RowsAffectedResp
transitOperatorUpdateWaybillTabletUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybillTablet baseUrl gtfsId req

transitOperatorGetWaybillsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Int -> Maybe Int -> Flow [NandiWaybillRow]
transitOperatorGetWaybillsUtil merchantShortId city vehicleCategory limit offset = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorWaybills baseUrl gtfsId limit offset

transitOperatorQueryRowsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> NandiTable -> QueryBody -> Flow [NandiRow]
transitOperatorQueryRowsUtil merchantShortId city vehicleCategory table body = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorQueryRows baseUrl gtfsId table body

-- ===== Stop & route management (clubber / editor) =====

transitOperatorSearchStopsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Maybe Int -> Maybe Bool -> Flow [EnrichedStop]
transitOperatorSearchStopsUtil merchantShortId city vehicleCategory q limit withRoutes = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorSearchStops baseUrl gtfsId q limit withRoutes

transitOperatorNearbyStopsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Double -> Double -> Maybe Double -> Maybe Int -> Maybe Bool -> Flow [EnrichedStop]
transitOperatorNearbyStopsUtil merchantShortId city vehicleCategory lat lon radius limit withRoutes = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorNearbyStops baseUrl gtfsId lat lon radius limit withRoutes

transitOperatorBulkReplaceStopsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> BulkReplaceReq -> Flow BulkReplaceResult
transitOperatorBulkReplaceStopsUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorBulkReplaceStops baseUrl gtfsId req

transitOperatorRouteStopsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow RouteStopsResponse
transitOperatorRouteStopsUtil merchantShortId city vehicleCategory routeId = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorRouteStops baseUrl gtfsId routeId

transitOperatorInsertRouteStopUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> InsertRouteStopReq -> Flow InsertRouteStopResp
transitOperatorInsertRouteStopUtil merchantShortId city vehicleCategory routeId req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorInsertRouteStop baseUrl gtfsId routeId req

transitOperatorReprocessRoutesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> ReprocessReq -> Flow [ReprocessResult]
transitOperatorReprocessRoutesUtil merchantShortId city vehicleCategory req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorReprocessRoutes baseUrl gtfsId req

transitOperatorExportRouteStopMappingUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Flow [RouteStopMappingExport]
transitOperatorExportRouteStopMappingUtil merchantShortId city vehicleCategory = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorExportRouteStopMapping baseUrl gtfsId
