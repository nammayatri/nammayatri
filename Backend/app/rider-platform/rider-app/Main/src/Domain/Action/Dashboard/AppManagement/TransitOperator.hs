{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TransitOperator
  ( transitOperatorGetRow,
    transitOperatorGetAllRows,
    transitOperatorDeleteRow,
    transitOperatorUpsertRow,
    transitOperatorUpsertRows,
    transitOperatorQueryRows,
    transitOperatorGetServiceTypes,
    transitOperatorGetRoutes,
    transitOperatorGetDepots,
    transitOperatorGetShiftTypes,
    transitOperatorGetScheduleNumbers,
    transitOperatorGetDayTypes,
    transitOperatorGetTripTypes,
    transitOperatorGetBreakTypes,
    transitOperatorGetTripDetails,
    transitOperatorGetFleets,
    transitOperatorGetConductor,
    transitOperatorGetDriver,
    transitOperatorGetDeviceIds,
    transitOperatorGetTabletIds,
    transitOperatorGetOperators,
    transitOperatorUpdateWaybillStatus,
    transitOperatorGetScheduleTripRepeat,
    transitOperatorSetScheduleTripRepeat,
    transitOperatorUpdateWaybillFleet,
    transitOperatorUpdateWaybillDetails,
    transitOperatorUpdateWaybillTablet,
    transitOperatorGetWaybills,
    transitOperatorGetDeviceVehicleMappingList,
    transitOperatorUpsertDeviceVehicleMapping,
    transitOperatorUnblockBus,
    transitOperatorSearchStops,
    transitOperatorNearbyStops,
    transitOperatorBulkReplaceStops,
    transitOperatorRouteStops,
    transitOperatorInsertRouteStop,
    transitOperatorReprocessRoutes,
    transitOperatorExportRouteStopMapping,
    transitOperatorUpsertVehicles,
    transitOperatorDeleteVehicle,
    transitOperatorQueryVehicle,
    transitOperatorV2ListTripGroups,
    transitOperatorV2UpsertTripGroup,
    transitOperatorV2GetTripGroup,
    transitOperatorV2DeleteTripGroup,
    transitOperatorV2ListTrips,
    transitOperatorV2UpsertTrips,
    transitOperatorV2DeleteTrip,
    transitOperatorV2ListDutyRepeats,
    transitOperatorV2UpsertDutyRepeat,
    transitOperatorV2DeleteDutyRepeat,
    transitOperatorV2PreviewDutyRepeats,
    transitOperatorV2GenerateDutyRepeats,
    transitOperatorV2ListDutyGroups,
    transitOperatorV2CreateDutyGroup,
    transitOperatorV2GetDutyGroup,
    transitOperatorV2ListDuties,
    transitOperatorV2GetDutyGroupLogs,
    transitOperatorV2UpdateDutyGroupVehicle,
    transitOperatorV2UpdateDutyGroupCrew,
    transitOperatorV2SetDutyGroupActive,
    transitOperatorV2DeleteDutyGroup,
    transitOperatorV2UpdateDutyCrew,
    transitOperatorV2DeleteDuty,
    transitOperatorV2ListGenerationFailures,
    transitOperatorV2ResolveGenerationFailure,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperator as APITransitOp
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv (FromNamedRecord (..), Header, decodeByName, (.:))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Domain.Action.UI.TransitOperator as DTOp
import qualified Domain.Types.DeviceVehicleMapping
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as NandiFlow
import qualified "this" SharedLogic.External.Nandi.TransitV2Types
import qualified "this" SharedLogic.External.Nandi.Types
import qualified Storage.Queries.DeviceVehicleMapping as QDvm

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow merchantShortId opCity column table vehicleCategory =
  DTOp.transitOperatorGetRowUtil merchantShortId opCity vehicleCategory table column

transitOperatorUnblockBus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorUnblockBus merchantShortId opCity vehicleNumber =
  DTOp.transitOperatorUnblockBusUtil merchantShortId opCity vehicleNumber

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows merchantShortId opCity limit offset table vehicleCategory =
  DTOp.transitOperatorGetAllRowsUtil merchantShortId opCity vehicleCategory table limit offset

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow merchantShortId opCity table vehicleCategory req =
  DTOp.transitOperatorDeleteRowUtil merchantShortId opCity vehicleCategory table req

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow merchantShortId opCity toRegen table vehicleCategory req =
  DTOp.transitOperatorUpsertRowUtil merchantShortId opCity vehicleCategory table toRegen req

transitOperatorUpsertRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> [Data.Aeson.Value] -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorUpsertRows merchantShortId opCity toRegen table vehicleCategory req =
  DTOp.transitOperatorUpsertRowsUtil merchantShortId opCity vehicleCategory table toRegen req

transitOperatorQueryRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.QueryBody -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorQueryRows merchantShortId opCity table vehicleCategory req =
  DTOp.transitOperatorQueryRowsUtil merchantShortId opCity vehicleCategory table req

transitOperatorGetServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ServiceType])
transitOperatorGetServiceTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetServiceTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRoute])
transitOperatorGetRoutes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetRoutesUtil merchantShortId opCity vehicleCategory

transitOperatorGetDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Depot])
transitOperatorGetDepots merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetDepotsUtil merchantShortId opCity vehicleCategory

transitOperatorGetShiftTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ShiftType])
transitOperatorGetShiftTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetShiftTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetScheduleNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ScheduleNumber])
transitOperatorGetScheduleNumbers merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetScheduleNumbersUtil merchantShortId opCity vehicleCategory

transitOperatorGetDayTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.DayType])
transitOperatorGetDayTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetDayTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetTripTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.TripType])
transitOperatorGetTripTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetTripTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetBreakTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.BreakType])
transitOperatorGetBreakTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetBreakTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiTripDetail])
transitOperatorGetTripDetails merchantShortId opCity scheduleNumber vehicleCategory =
  DTOp.transitOperatorGetTripDetailsUtil merchantShortId opCity vehicleCategory scheduleNumber

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets merchantShortId opCity limit offset vehicleCategory =
  DTOp.transitOperatorGetFleetsUtil merchantShortId opCity vehicleCategory limit offset

transitOperatorGetConductor :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetConductor merchantShortId opCity token vehicleCategory =
  DTOp.transitOperatorGetConductorUtil merchantShortId opCity vehicleCategory token

transitOperatorGetDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetDriver merchantShortId opCity token vehicleCategory =
  DTOp.transitOperatorGetDriverUtil merchantShortId opCity vehicleCategory token

transitOperatorGetDeviceIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
transitOperatorGetDeviceIds merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetDeviceIdsUtil merchantShortId opCity vehicleCategory

transitOperatorGetTabletIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
transitOperatorGetTabletIds merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetTabletIdsUtil merchantShortId opCity vehicleCategory

transitOperatorGetOperators :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Employee])
transitOperatorGetOperators merchantShortId opCity role vehicleCategory =
  DTOp.transitOperatorGetOperatorsUtil merchantShortId opCity vehicleCategory role

transitOperatorUpdateWaybillStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillStatus merchantShortId opCity vehicleCategory req =
  DTOp.transitOperatorUpdateWaybillStatusUtil merchantShortId opCity vehicleCategory req

transitOperatorGetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorGetScheduleTripRepeat merchantShortId opCity scheduleTripId vehicleCategory =
  DTOp.transitOperatorGetScheduleTripRepeatUtil merchantShortId opCity vehicleCategory scheduleTripId

transitOperatorSetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.SetScheduleTripRepeatReq -> Environment.Flow SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorSetScheduleTripRepeat merchantShortId opCity scheduleTripId vehicleCategory req =
  DTOp.transitOperatorSetScheduleTripRepeatUtil merchantShortId opCity vehicleCategory scheduleTripId req

transitOperatorUpdateWaybillFleet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillFleet merchantShortId opCity vehicleCategory req =
  DTOp.transitOperatorUpdateWaybillFleetUtil merchantShortId opCity vehicleCategory req

transitOperatorUpdateWaybillDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillDetailsReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillDetails merchantShortId opCity vehicleCategory req =
  DTOp.transitOperatorUpdateWaybillDetailsUtil merchantShortId opCity vehicleCategory req

transitOperatorUpdateWaybillTablet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillTablet merchantShortId opCity vehicleCategory req =
  DTOp.transitOperatorUpdateWaybillTabletUtil merchantShortId opCity vehicleCategory req

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiWaybillRow])
transitOperatorGetWaybills merchantShortId opCity limit offset vehicleCategory =
  DTOp.transitOperatorGetWaybillsUtil merchantShortId opCity vehicleCategory limit offset

-- CSV row type for DeviceVehicleMapping
data DeviceVehicleMappingCsvRow = DeviceVehicleMappingCsvRow
  { device_id :: Text,
    fleet_id :: Text
  }

instance FromNamedRecord DeviceVehicleMappingCsvRow where
  parseNamedRecord r =
    DeviceVehicleMappingCsvRow
      <$> r .: "device_id"
      <*> r .: "fleet_id"

transitOperatorGetDeviceVehicleMappingList ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    Environment.Flow APITransitOp.DeviceVehicleMappingListRes
  )
transitOperatorGetDeviceVehicleMappingList merchantShortId opCity = do
  (_, gtfsId) <- DTOp.resolveBaseUrlAndGtfsId merchantShortId opCity BecknV2.OnDemand.Enums.BUS
  mappings <- QDvm.findAllByGtfsId gtfsId
  let items = map toItem mappings
  pure $
    APITransitOp.DeviceVehicleMappingListRes
      { APITransitOp.mappings = items
      }
  where
    toItem dvm =
      APITransitOp.DeviceVehicleMappingItem
        { deviceId = dvm.deviceId,
          vehicleNo = dvm.vehicleNo,
          gtfsId = dvm.gtfsId,
          createdAt = dvm.createdAt,
          updatedAt = dvm.updatedAt
        }

transitOperatorUpsertDeviceVehicleMapping ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    APITransitOp.UpsertDeviceVehicleMappingReq ->
    Environment.Flow APITransitOp.UpsertDeviceVehicleMappingResp
  )
transitOperatorUpsertDeviceVehicleMapping merchantShortId opCity req = do
  (_, gtfsId) <- DTOp.resolveBaseUrlAndGtfsId merchantShortId opCity BecknV2.OnDemand.Enums.BUS
  csvRows <- readCsv req.file
  existingList <- QDvm.findAllByGtfsId gtfsId
  let existingMap = Map.fromList [(m.deviceId, m) | m <- existingList]

  unprocessedEntries <- fmap catMaybes $
    forM csvRows $ \row -> do
      result <-
        withTryCatch "upsertDeviceVehicleMapping" $
          upsertRow existingMap row.device_id row.fleet_id gtfsId
      case result of
        Left err -> do
          logError $ "Error upserting device vehicle mapping: " <> row.device_id <> "error: " <> show err
          pure (Just row.device_id)
        Right _ -> pure Nothing

  pure $
    APITransitOp.UpsertDeviceVehicleMappingResp
      { success = case length unprocessedEntries of
          0 -> "All mappings upserted successfully"
          _ -> "Some mappings failed to upsert",
        unprocessedEntries = unprocessedEntries
      }
  where
    readCsv :: FilePath -> Environment.Flow [DeviceVehicleMappingCsvRow]
    readCsv csvFile = do
      csvData <- liftIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector DeviceVehicleMappingCsvRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> pure $ V.toList v

    upsertRow :: Map.Map Text Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping -> Text -> Text -> Text -> Environment.Flow ()
    upsertRow existingMap deviceId vehicleNo gtfsId = do
      now <- getCurrentTime
      case Map.lookup deviceId existingMap of
        Just dvm ->
          QDvm.updateByPrimaryKey
            Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping
              { deviceId = deviceId,
                vehicleNo = vehicleNo,
                gtfsId = gtfsId,
                createdAt = dvm.createdAt,
                updatedAt = now,
                merchantId = Kernel.Prelude.Nothing,
                merchantOperatingCityId = Kernel.Prelude.Nothing
              }
        Nothing ->
          QDvm.create
            Domain.Types.DeviceVehicleMapping.DeviceVehicleMapping
              { deviceId = deviceId,
                vehicleNo = vehicleNo,
                gtfsId = gtfsId,
                createdAt = now,
                updatedAt = now,
                merchantId = Kernel.Prelude.Nothing,
                merchantOperatingCityId = Kernel.Prelude.Nothing
              }

-- ===== Stop & route management (clubber / editor) =====

transitOperatorSearchStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorSearchStops merchantShortId opCity limit withRoutes q vehicleCategory =
  DTOp.transitOperatorSearchStopsUtil merchantShortId opCity vehicleCategory q limit withRoutes

transitOperatorNearbyStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorNearbyStops merchantShortId opCity limit radius withRoutes lat lon vehicleCategory =
  DTOp.transitOperatorNearbyStopsUtil merchantShortId opCity vehicleCategory lat lon radius limit withRoutes

transitOperatorBulkReplaceStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.BulkReplaceReq -> Environment.Flow SharedLogic.External.Nandi.Types.BulkReplaceResult)
transitOperatorBulkReplaceStops merchantShortId opCity vehicleCategory req =
  DTOp.transitOperatorBulkReplaceStopsUtil merchantShortId opCity vehicleCategory req

transitOperatorRouteStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.RouteStopsResponse)
transitOperatorRouteStops merchantShortId opCity routeId vehicleCategory =
  DTOp.transitOperatorRouteStopsUtil merchantShortId opCity vehicleCategory routeId

transitOperatorInsertRouteStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.InsertRouteStopReq -> Environment.Flow SharedLogic.External.Nandi.Types.InsertRouteStopResp)
transitOperatorInsertRouteStop merchantShortId opCity routeId vehicleCategory req =
  DTOp.transitOperatorInsertRouteStopUtil merchantShortId opCity vehicleCategory routeId req

transitOperatorReprocessRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.ReprocessReq -> Environment.Flow [SharedLogic.External.Nandi.Types.ReprocessResult])
transitOperatorReprocessRoutes merchantShortId opCity vehicleCategory req =
  DTOp.transitOperatorReprocessRoutesUtil merchantShortId opCity vehicleCategory req

transitOperatorExportRouteStopMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.RouteStopMappingExport])
transitOperatorExportRouteStopMapping merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorExportRouteStopMappingUtil merchantShortId opCity vehicleCategory

transitOperatorUpsertVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> [SharedLogic.External.Nandi.Types.VehicleUpsertRequest] -> Environment.Flow [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorUpsertVehicles merchantShortId opCity vehicleCategory items =
  DTOp.transitOperatorUpsertVehiclesUtil merchantShortId opCity vehicleCategory items

transitOperatorDeleteVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteVehicle merchantShortId opCity vehicleCategory vehicleId =
  DTOp.transitOperatorDeleteVehicleUtil merchantShortId opCity vehicleCategory vehicleId

transitOperatorQueryVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorQueryVehicle merchantShortId opCity fleetNo tagNumber vehicleNo vehicleCategory =
  DTOp.transitOperatorQueryVehicleUtil merchantShortId opCity vehicleCategory vehicleNo tagNumber fleetNo

-- transitV2: pass-through to GIMS /internal/operator/{gtfs_id}/v2/... (see Domain.Action.UI.TransitOperator).

transitOperatorV2ListTripGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage)
transitOperatorV2ListTripGroups merchantShortId opCity code conductorTokenNumber depotId driverTokenNumber limit offset operatorId search shift tripType vehicleNumber zone requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["trip-groups"] NandiFlow.emptyOperatorV2Query {NandiFlow.limit = limit, NandiFlow.offset = offset, NandiFlow.shift = shift, NandiFlow.depotId = depotId, NandiFlow.code = code, NandiFlow.search = search, NandiFlow.zone = zone, NandiFlow.tripType = tripType, NandiFlow.vehicleNumber = vehicleNumber, NandiFlow.driverTokenNumber = driverTokenNumber, NandiFlow.conductorTokenNumber = conductorTokenNumber}

transitOperatorV2UpsertTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2UpsertTripGroup merchantShortId opCity operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2PostUtil merchantShortId opCity vehicleCategory operatorId requestorId ["trip-groups", "upsert"] req

transitOperatorV2GetTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2GetTripGroup merchantShortId opCity tripGroupId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["trip-groups", tripGroupId] NandiFlow.emptyOperatorV2Query

transitOperatorV2DeleteTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTripGroup merchantShortId opCity tripGroupId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2ActionUtil merchantShortId opCity vehicleCategory operatorId requestorId ["trip-groups", tripGroupId, "delete"]

transitOperatorV2ListTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2ListTrips merchantShortId opCity tripGroupId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["trip-groups", tripGroupId, "trips"] NandiFlow.emptyOperatorV2Query

transitOperatorV2UpsertTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2UpsertTrips merchantShortId opCity tripGroupId operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2PostUtil merchantShortId opCity vehicleCategory operatorId requestorId ["trip-groups", tripGroupId, "trips", "upsert"] req

transitOperatorV2DeleteTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTrip merchantShortId opCity tripId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2ActionUtil merchantShortId opCity vehicleCategory operatorId requestorId ["trips", tripId, "delete"]

transitOperatorV2ListDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage)
transitOperatorV2ListDutyRepeats merchantShortId opCity code conductorTokenNumber driverTokenNumber limit offset operatorId repeatStatus search tripGroupId vehicleNumber requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-repeats"] NandiFlow.emptyOperatorV2Query {NandiFlow.limit = limit, NandiFlow.offset = offset, NandiFlow.tripGroupId = tripGroupId, NandiFlow.code = code, NandiFlow.search = search, NandiFlow.vehicleNumber = vehicleNumber, NandiFlow.driverTokenNumber = driverTokenNumber, NandiFlow.conductorTokenNumber = conductorTokenNumber, NandiFlow.repeatStatus = repeatStatus}

transitOperatorV2UpsertDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp)
transitOperatorV2UpsertDutyRepeat merchantShortId opCity operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2PostUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-repeats", "upsert"] req

transitOperatorV2DeleteDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyRepeat merchantShortId opCity dutyRepeatId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2ActionUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-repeats", dutyRepeatId, "delete"]

transitOperatorV2PreviewDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2PreviewDutyRepeats merchantShortId opCity operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2PostUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-repeats", "preview"] req

transitOperatorV2GenerateDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2GenerateDutyRepeats merchantShortId opCity operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2PostUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-repeats", "generate"] req

transitOperatorV2ListDutyGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage)
transitOperatorV2ListDutyGroups merchantShortId opCity code conductorTokenNumber depotId driverTokenNumber isActive limit offset operationDate operatorId search tripGroupId vehicleNumber requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-groups"] NandiFlow.emptyOperatorV2Query {NandiFlow.limit = limit, NandiFlow.offset = offset, NandiFlow.code = code, NandiFlow.search = search, NandiFlow.depotId = depotId, NandiFlow.tripGroupId = tripGroupId, NandiFlow.operationDate = operationDate, NandiFlow.vehicleNumber = vehicleNumber, NandiFlow.driverTokenNumber = driverTokenNumber, NandiFlow.conductorTokenNumber = conductorTokenNumber, NandiFlow.isActive = isActive}

transitOperatorV2CreateDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2CreateDutyGroup merchantShortId opCity operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2PostUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-groups", "create"] req

transitOperatorV2GetDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2GetDutyGroup merchantShortId opCity dutyGroupId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-groups", dutyGroupId] NandiFlow.emptyOperatorV2Query

transitOperatorV2ListDuties :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2Duty])
transitOperatorV2ListDuties merchantShortId opCity dutyGroupId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-groups", dutyGroupId, "duties"] NandiFlow.emptyOperatorV2Query

transitOperatorV2GetDutyGroupLogs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog])
transitOperatorV2GetDutyGroupLogs merchantShortId opCity dutyGroupId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-groups", dutyGroupId, "logs"] NandiFlow.emptyOperatorV2Query

transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2UpdateDutyGroupVehicle merchantShortId opCity dutyGroupId operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2UpdateRunVehicleUtil merchantShortId opCity vehicleCategory operatorId requestorId dutyGroupId req

transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2UpdateDutyGroupCrew merchantShortId opCity dutyGroupId operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2UpdateRunCrewUtil merchantShortId opCity vehicleCategory operatorId requestorId dutyGroupId req

transitOperatorV2SetDutyGroupActive :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2SetDutyGroupActive merchantShortId opCity dutyGroupId operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2PostUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-groups", dutyGroupId, "active"] req

transitOperatorV2DeleteDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyGroup merchantShortId opCity dutyGroupId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2ActionUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duty-groups", dutyGroupId, "delete"]

transitOperatorV2UpdateDutyCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2Duty)
transitOperatorV2UpdateDutyCrew merchantShortId opCity dutyId operatorId requestorId vehicleCategory req =
  DTOp.transitOperatorV2UpdateTripCrewUtil merchantShortId opCity vehicleCategory operatorId requestorId dutyId req

transitOperatorV2DeleteDuty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDuty merchantShortId opCity dutyId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2ActionUtil merchantShortId opCity vehicleCategory operatorId requestorId ["duties", dutyId, "delete"]

transitOperatorV2ListGenerationFailures :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage)
transitOperatorV2ListGenerationFailures merchantShortId opCity limit offset operatorId resolved requestorId vehicleCategory =
  DTOp.transitOperatorV2GetUtil merchantShortId opCity vehicleCategory operatorId requestorId ["generation-failures"] NandiFlow.emptyOperatorV2Query {NandiFlow.limit = limit, NandiFlow.offset = offset, NandiFlow.resolved = resolved}

transitOperatorV2ResolveGenerationFailure :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog)
transitOperatorV2ResolveGenerationFailure merchantShortId opCity failureId operatorId requestorId vehicleCategory =
  DTOp.transitOperatorV2ActionUtil merchantShortId opCity vehicleCategory operatorId requestorId ["generation-failures", failureId, "resolve"]
