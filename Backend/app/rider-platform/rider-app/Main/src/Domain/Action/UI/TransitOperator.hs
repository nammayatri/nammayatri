{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Domain.Action.UI.TransitOperator where

import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Domain.Action.UI.TransitOperator.Validation (preprocessUpsertBody, preprocessUpsertBodyAtIdx)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
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
import qualified SharedLogic.External.Nandi.TransitV2Types as V2
import SharedLogic.External.Nandi.Types
import SharedLogic.FRFSUtils (unixToUTC)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.VehicleSeatLayoutMappingExtra as CQVehicleSeatLayoutMapping
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

transitOperatorGetFleetsUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Int -> Maybe Int -> Flow [Fleet]
transitOperatorGetFleetsUtil merchantShortId city vehicleCategory limit offset = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorFleets baseUrl gtfsId limit offset

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

transitOperatorGetScheduleTripRepeatUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow ScheduleTripRepeatConfig
transitOperatorGetScheduleTripRepeatUtil merchantShortId city vehicleCategory scheduleTripId = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorGetScheduleTripRepeat baseUrl gtfsId scheduleTripId

transitOperatorSetScheduleTripRepeatUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> SetScheduleTripRepeatReq -> Flow ScheduleTripRepeatConfig
transitOperatorSetScheduleTripRepeatUtil merchantShortId city vehicleCategory scheduleTripId req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorSetScheduleTripRepeat baseUrl gtfsId scheduleTripId req

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
  gimsReq <- validateVehicleChange baseUrl gtfsId req
  res <- NandiFlow.operatorWaybillDetails baseUrl gtfsId gimsReq
  fanOutWaybillRefresh baseUrl gtfsId req.waybill_no False
  pure res

-- | The bus being replaced is read off the waybill rather than taken from the request, and both buses must
-- share one seat layout: tickets already issued on this waybill carry seats from the old layout, which would
-- not exist on a differently laid-out bus. A waybill with no vehicle yet is a first assignment, not a swap.
validateVehicleChange :: BaseUrl -> Text -> UpdateWaybillDetailsReq -> Flow UpdateWaybillDetailsReq
validateVehicleChange baseUrl gtfsId req = case req.vehicle_no of
  Nothing -> pure req
  Just rawVehicleNo -> do
    newVehicleNo <- validateNonBlank rawVehicleNo
    meta <- NandiFlow.getWaybillMetadata baseUrl gtfsId req.waybill_no Nothing
    let currentVehicleNo = T.strip meta.vehicle_no
    unless (T.null currentVehicleNo) $ do
      mbCurrentLayoutId <- findSeatLayoutId currentVehicleNo
      mbNewLayoutId <- findSeatLayoutId newVehicleNo
      unless (mbCurrentLayoutId == mbNewLayoutId) $
        throwError $
          InvalidRequest $
            mconcat
              [ "updateWaybillDetails: seat layout mismatch on waybill ",
                req.waybill_no,
                " - current vehicle ",
                currentVehicleNo,
                " (seatLayoutId: ",
                showLayoutId mbCurrentLayoutId,
                ") and requested vehicle ",
                newVehicleNo,
                " (seatLayoutId: ",
                showLayoutId mbNewLayoutId,
                ") do not share the same seat layout, vehicle change is not allowed"
              ]
    pure (req :: UpdateWaybillDetailsReq) {vehicle_no = Just newVehicleNo}
  where
    validateNonBlank rawVehicleNo = do
      let vehicleNo = T.strip rawVehicleNo
      when (T.null vehicleNo) $ throwError $ InvalidRequest "updateWaybillDetails: vehicle_no must not be blank"
      pure vehicleNo
    findSeatLayoutId vehicleNo = fmap (.seatLayoutId) <$> CQVehicleSeatLayoutMapping.findByVehicleNoAndGtfsIdCached vehicleNo gtfsId
    showLayoutId = maybe "<none>" (.getId)

-- | Live trip-start time (first stop's ETA on the current schedule), matching what
-- Lib.JourneyModule.Types.mkLegInfoFromFrfsBooking uses to gate driver details on the ticket. Fetched
-- fresh (not booking.startTime, which is frozen at booking-confirmation time and can go stale if the
-- trip is later rescheduled/delayed) so both gates agree on the same, current departure time.
getLiveTripStartTime :: DFRFSTicketBooking.FRFSTicketBooking -> Flow (Maybe UTCTime)
getLiveTripStartTime booking = case (booking.tripId, booking.routeCode) of
  (Just tripId, Just routeCode) -> do
    integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
    let (waybillNo, tripNo) = JMU.getWaybillNoAndTripNoFromTripId tripId
    eSchedule <- withTryCatch "getLiveTripStartTime:getBusTripSchedule" (OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBPPConfig)
    case eSchedule of
      Right (firstSchedule : _) -> pure $ unixToUTC . (.arrivalTimeUnix) <$> listToMaybe firstSchedule.eta
      _ -> pure Nothing
  _ -> pure Nothing

-- | Reflect a waybill fleet/driver change on the customer tickets riding that waybill: for every confirmed
-- booking on the waybill, refresh its driver + (assigned) bus from freshly-fetched waybill metadata. The
-- metadata is read directly via NandiFlow (bypassing the 30s OTPRest in-mem cache) since the operator's
-- write just landed. The refresh (persisting to booking/leg) runs synchronously here; the per-booking
-- customer notification is forked (best-effort). A lookup/apply failure for one booking never aborts the rest.
-- `perTrip` (transitV2 runs, crew per trip): each booking is refreshed with its own trip's crew, one
-- metadata read per trip; otherwise one read for the whole waybill.
fanOutWaybillRefresh :: BaseUrl -> Text -> Text -> Bool -> Flow ()
fanOutWaybillRefresh baseUrl gtfsId waybillNo perTrip = do
  bookings <- QFRFSTicketBooking.findAllConfirmedByWaybillNo waybillNo
  unless (null bookings) $ do
    let tripKey booking =
          if perTrip
            then booking.tripId >>= JMU.tripNoToMaybe . snd . JMU.getWaybillNoAndTripNoFromTripId
            else Nothing
    metas <- forM (ordNub (map tripKey bookings)) $ \mbTripNo -> do
      eMeta <- withTryCatch "fanOutWaybillRefresh:getWaybillMetadata" (NandiFlow.getWaybillMetadata baseUrl gtfsId waybillNo mbTripNo)
      case eMeta of
        Left err -> do
          logError $ "fanOutWaybillRefresh: metadata fetch failed for waybillNo=" <> waybillNo <> " trip=" <> show mbTripNo <> ": " <> show err
          pure Nothing
        Right meta -> pure (Just (mbTripNo, meta))
    let metaByTrip = Map.fromList (catMaybes metas)
    unless (Map.null metaByTrip) $ do
      legs <- QJourneyLeg.findAllByLegSearchIds (map (\b -> b.searchId.getId) bookings)
      let legMap = Map.fromList $ mapMaybe (\l -> (,l) <$> l.legSearchId) legs
      -- Batch-load the riders up front so the per-booking notification below needs no query in the loop.
      persons <- QP.findAllByIds (map (.riderId) bookings)
      let personMap = Map.fromList $ map (\p -> (p.id, p)) persons
      -- Notify only when the driver and/or the assigned bus actually changed. The notification (FCM push +
      -- external WhatsApp) is slow and best-effort, so it is forked out of the critical, synchronous
      -- refresh path above.
      toNotify <-
        fmap catMaybes $
          forM (mapMaybe (\b -> (b,) <$> Map.lookup (tripKey b) metaByTrip) bookings) $ \(booking, meta) -> do
            let mbJourneyLeg = Map.lookup booking.searchId.getId legMap
            eRefresh <- withTryCatch ("fanOutWaybillRefresh:apply:" <> booking.id.getId) $ JMU.applyWaybillMetadataToTicket booking mbJourneyLeg meta
            case eRefresh of
              Left err -> do
                logError $ "fanOutWaybillRefresh: apply failed for booking " <> booking.id.getId <> ": " <> show err
                pure Nothing
              Right refreshInfo ->
                pure $
                  if refreshInfo.driverChanged || refreshInfo.busChanged
                    then (\person -> (booking, refreshInfo, person, (.journeyId) <$> mbJourneyLeg)) <$> Map.lookup booking.riderId personMap
                    else Nothing
      -- Grouped by tripId: bookings on the same trip share one live schedule fetch (getLiveTripStartTime)
      -- instead of each booking fetching it separately -- a waybill can have confirmed bookings across
      -- more than one trip (findAllConfirmedByWaybillNo isn't trip-scoped), so this groups rather than
      -- assuming a single shared trip.
      let groups = Map.elems $ Map.fromListWith (<>) [(b.tripId, [item]) | item@(b, _, _, _) <- toNotify]
      forM_ groups $ \bookingGroup ->
        whenJust (listToMaybe bookingGroup) $ \(firstBooking, _, _, _) ->
          fork ("fanOutWaybillRefresh:notify:" <> waybillNo <> ":" <> fromMaybe "" firstBooking.tripId) $ do
            mbStartTime <- getLiveTripStartTime firstBooking
            forM_ bookingGroup $ \(booking, refreshInfo, person, mbJourneyId) -> do
              let vehicleNo = fromMaybe "" refreshInfo.finalBoardedBusNumber
                  -- Label the trip as "<fromStop> - <toStop>" (stop names, falling back to codes) instead
                  -- of the raw route name.
                  routeName = fromMaybe booking.fromStationCode booking.fromStationName <> " - " <> fromMaybe booking.toStationCode booking.toStationName
              eNotify <- withTryCatch ("fanOutWaybillRefresh:notify:" <> booking.id.getId) $ Notifications.notifyFrfsTripDetailsUpdated person booking.id.getId vehicleNo routeName booking.tripId mbJourneyId refreshInfo.driverChanged refreshInfo.busChanged mbStartTime
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

-- ===== Vehicle management (GIMS) =====

-- Strip surrounding whitespace and treat blank/whitespace-only as absent — mirrors GIMS server-side normalization.
nonBlankText :: Maybe Text -> Maybe Text
nonBlankText = (>>= \t -> let t' = T.strip t in if T.null t' then Nothing else Just t')

transitOperatorUpsertVehiclesUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> [VehicleUpsertRequest] -> Flow [Fleet]
transitOperatorUpsertVehiclesUtil merchantShortId city vehicleCategory items = do
  when (null items) $ throwError $ InvalidRequest "upsertVehicles: body must contain at least one vehicle"
  normalized <- traverse normalizeItem items
  -- Reject intra-batch duplicates on the natural conflict key: GIMS behavior on same-vehicle_no twice in one POST is implementation-defined (last-wins vs first-wins vs whole-batch-fail).
  let counts = Map.fromListWith (+) $ zip (map (.vehicle_no) normalized) (repeat (1 :: Int))
      dupes = Map.keys $ Map.filter (> 1) counts
  unless (null dupes) $
    throwError $ InvalidRequest $ "upsertVehicles: duplicate vehicle_no in batch: " <> T.intercalate ", " dupes
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorUpsertVehicles baseUrl gtfsId normalized
  where
    -- Strip once at the boundary so a padded write and a clean-string query land on the same natural key.
    normalizeItem req = do
      let vNo = T.strip req.vehicle_no
      when (T.null vNo) $ throwError $ InvalidRequest "upsertVehicles: vehicle_no must not be blank"
      pure
        req{vehicle_no = vNo,
            fleet_no = nonBlankText req.fleet_no,
            tag_number = nonBlankText req.tag_number,
            status = nonBlankText req.status
           }

transitOperatorDeleteVehicleUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Text -> Flow RowsAffectedResp
transitOperatorDeleteVehicleUtil merchantShortId city vehicleCategory vehicleId = do
  -- guard blank/whitespace: an empty Capture segment would silently target the wrong URL (or 404) instead of failing loud
  let vehicleId' = T.strip vehicleId
  when (T.null vehicleId') $ throwError $ InvalidRequest "deleteVehicle: vehicleId must not be blank"
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorDeleteVehicle baseUrl gtfsId vehicleId'

transitOperatorQueryVehicleUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Maybe Text -> Maybe Text -> Flow [Fleet]
transitOperatorQueryVehicleUtil merchantShortId city vehicleCategory vehicleNo tagNumber fleetNo = do
  let vehicleNo' = nonBlankText vehicleNo
      tagNumber' = nonBlankText tagNumber
      fleetNo' = nonBlankText fleetNo
  when (isNothing vehicleNo' && isNothing tagNumber' && isNothing fleetNo') $
    throwError $ InvalidRequest "queryVehicle: at least one of vehicleNo, tagNumber, fleetNo is required"
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorQueryVehicle baseUrl gtfsId vehicleNo' tagNumber' fleetNo'

-- ===== transitV2 (GIMS /internal/operator/{gtfs_id}/v2/...) =====
-- Typed (SharedLogic.External.Nandi.TransitV2Types): the dashboard person goes to GIMS as
-- x-actor-person-id, `operatorId` as x-operator-id; GIMS owns validation.

transitOperatorV2GetUtil :: Data.Aeson.FromJSON resp => ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Maybe Text -> [Text] -> NandiFlow.OperatorV2Query -> Flow resp
transitOperatorV2GetUtil merchantShortId city vehicleCategory mbOperatorId mbRequestorId path q = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorV2Get baseUrl gtfsId (nonBlankText mbOperatorId) (nonBlankText mbRequestorId) path q

transitOperatorV2PostUtil :: (Data.Aeson.ToJSON req, Data.Aeson.FromJSON resp) => ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Maybe Text -> [Text] -> req -> Flow resp
transitOperatorV2PostUtil merchantShortId city vehicleCategory mbOperatorId mbRequestorId path body = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  NandiFlow.operatorV2Post baseUrl gtfsId (nonBlankText mbOperatorId) (nonBlankText mbRequestorId) path body

-- | Bodyless actions (delete / resolve): GIMS takes an empty object.
transitOperatorV2ActionUtil :: Data.Aeson.FromJSON resp => ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Maybe Text -> [Text] -> Flow resp
transitOperatorV2ActionUtil merchantShortId city vehicleCategory mbOperatorId mbRequestorId path =
  transitOperatorV2PostUtil merchantShortId city vehicleCategory mbOperatorId mbRequestorId path (Data.Aeson.object [])

-- ── transitV2: bus / crew changes with the same side effects as waybill updates ──
-- (seat-layout check on a bus swap; refresh + notify riders of the affected trips).

-- | Tickets already issued on a run carry seats of its bus's layout; a swap must keep the layout.
-- A run with no bus yet is a first assignment, not a swap.
ensureSameSeatLayout :: Text -> Text -> Maybe Text -> Maybe Text -> Flow ()
ensureSameSeatLayout gtfsId label mbCurrent mbNew =
  case (nonBlankText mbCurrent, nonBlankText mbNew) of
    (Just current, Just new) | current /= new -> do
      let layoutOf v = fmap (.seatLayoutId) <$> CQVehicleSeatLayoutMapping.findByVehicleNoAndGtfsIdCached v gtfsId
      currentLayout <- layoutOf current
      newLayout <- layoutOf new
      unless (currentLayout == newLayout) $
        throwError $
          InvalidRequest $
            label <> ": buses " <> current <> " and " <> new <> " don't share a seat layout ("
              <> maybe "<none>" (.getId) currentLayout
              <> " vs "
              <> maybe "<none>" (.getId) newLayout
              <> "), bus change is not allowed"
    _ -> pure ()

-- | Run-level bus change: seat-layout check, GIMS write, then refresh the run's bookings.
transitOperatorV2UpdateRunVehicleUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Maybe Text -> Text -> V2.V2UpdateVehicleReq -> Flow V2.V2DutyGroup
transitOperatorV2UpdateRunVehicleUtil merchantShortId city vehicleCategory mbOperatorId mbRequestorId runId req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  let op = nonBlankText mbOperatorId
      actor = nonBlankText mbRequestorId
  detail :: V2.V2DutyGroupDetail <- NandiFlow.operatorV2Get baseUrl gtfsId op actor ["duty-groups", runId] NandiFlow.emptyOperatorV2Query
  ensureSameSeatLayout gtfsId "updateRunVehicle" detail.dutyGroup.vehicleNumber req.vehicleNumber
  res <- NandiFlow.operatorV2Post baseUrl gtfsId op actor ["duty-groups", runId, "vehicle"] req
  fanOutWaybillRefresh baseUrl gtfsId detail.dutyGroup.waybillNo True
  pure res

-- | Run-level crew change, then refresh the run's bookings (each with its own trip's crew).
transitOperatorV2UpdateRunCrewUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Maybe Text -> Text -> V2.V2UpdateCrewReq -> Flow V2.V2DutyGroupDetail
transitOperatorV2UpdateRunCrewUtil merchantShortId city vehicleCategory mbOperatorId mbRequestorId runId req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  res :: V2.V2DutyGroupDetail <- NandiFlow.operatorV2Post baseUrl gtfsId (nonBlankText mbOperatorId) (nonBlankText mbRequestorId) ["duty-groups", runId, "crew"] req
  fanOutWaybillRefresh baseUrl gtfsId res.dutyGroup.waybillNo True
  pure res

-- | Trip-level crew change, then refresh the bookings of that run (per trip).
transitOperatorV2UpdateTripCrewUtil :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Maybe Text -> Text -> V2.V2UpdateCrewReq -> Flow V2.V2Duty
transitOperatorV2UpdateTripCrewUtil merchantShortId city vehicleCategory mbOperatorId mbRequestorId dutyId req = do
  (baseUrl, gtfsId) <- resolveBaseUrlAndGtfsId merchantShortId city vehicleCategory
  let op = nonBlankText mbOperatorId
      actor = nonBlankText mbRequestorId
  res :: V2.V2Duty <- NandiFlow.operatorV2Post baseUrl gtfsId op actor ["duties", dutyId, "crew"] req
  detail :: V2.V2DutyGroupDetail <- NandiFlow.operatorV2Get baseUrl gtfsId op actor ["duty-groups", res.dutyGroupId] NandiFlow.emptyOperatorV2Query
  fanOutWaybillRefresh baseUrl gtfsId detail.dutyGroup.waybillNo True
  pure res
