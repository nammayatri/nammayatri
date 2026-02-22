{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TransitOperator
  ( transitOperatorGetRow,
    transitOperatorGetAllRows,
    transitOperatorDeleteRow,
    transitOperatorUpsertRow,
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
  )
where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.TransitOperator as DTOp
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import qualified "this" SharedLogic.External.Nandi.Types
import Tools.Auth

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Data.Aeson.Value)
transitOperatorGetRow merchantShortId opCity column table vehicleCategory =
  DTOp.transitOperatorGetRowUtil merchantShortId opCity vehicleCategory table column

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Data.Aeson.Value])
transitOperatorGetAllRows merchantShortId opCity limit offset table vehicleCategory =
  DTOp.transitOperatorGetAllRowsUtil merchantShortId opCity vehicleCategory table limit offset

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow merchantShortId opCity table vehicleCategory req =
  DTOp.transitOperatorDeleteRowUtil merchantShortId opCity vehicleCategory table req

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow Data.Aeson.Value)
transitOperatorUpsertRow merchantShortId opCity table vehicleCategory req =
  DTOp.transitOperatorUpsertRowUtil merchantShortId opCity vehicleCategory table req

transitOperatorGetServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ServiceType])
transitOperatorGetServiceTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetServiceTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRoute])
transitOperatorGetRoutes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetRoutesUtil merchantShortId opCity vehicleCategory

transitOperatorGetDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Depot])
transitOperatorGetDepots merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetDepotsUtil merchantShortId opCity vehicleCategory

transitOperatorGetShiftTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
transitOperatorGetShiftTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetShiftTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetScheduleNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ScheduleNumber])
transitOperatorGetScheduleNumbers merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetScheduleNumbersUtil merchantShortId opCity vehicleCategory

transitOperatorGetDayTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
transitOperatorGetDayTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetDayTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetTripTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
transitOperatorGetTripTypes merchantShortId opCity vehicleCategory =
  DTOp.transitOperatorGetTripTypesUtil merchantShortId opCity vehicleCategory

transitOperatorGetBreakTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
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

transitOperatorGetOperators :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Employee])
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

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Data.Aeson.Value])
transitOperatorGetWaybills merchantShortId opCity limit offset vehicleCategory =
  DTOp.transitOperatorGetWaybillsUtil merchantShortId opCity vehicleCategory limit offset
