{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TransitOperator
  ( transitOperatorGetRow,
    transitOperatorGetAllRows,
    transitOperatorDeleteRow,
    transitOperatorUpsertRow,
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
    transitOperatorUpdateWaybillFleet,
    transitOperatorUpdateWaybillTablet,
    transitOperatorGetWaybills,
    transitOperatorGetDeviceVehicleMappingList,
    transitOperatorUpsertDeviceVehicleMapping,
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
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (Header, throwError)
import qualified "this" SharedLogic.External.Nandi.Types
import qualified Storage.Queries.DeviceVehicleMapping as QDvm

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow merchantShortId opCity column table vehicleCategory =
  DTOp.transitOperatorGetRowUtil merchantShortId opCity vehicleCategory table column

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows merchantShortId opCity limit offset table vehicleCategory =
  DTOp.transitOperatorGetAllRowsUtil merchantShortId opCity vehicleCategory table limit offset

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow merchantShortId opCity table vehicleCategory req =
  DTOp.transitOperatorDeleteRowUtil merchantShortId opCity vehicleCategory table req

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow merchantShortId opCity toRegen table vehicleCategory req =
  DTOp.transitOperatorUpsertRowUtil merchantShortId opCity vehicleCategory table toRegen req

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

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetFleetsUtil merchantShortId opCity vehicleCategory

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

transitOperatorUpdateWaybillFleet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillFleet merchantShortId opCity vehicleCategory req =
  DTOp.transitOperatorUpdateWaybillFleetUtil merchantShortId opCity vehicleCategory req

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
