{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules
  ( transitOperatorV2ListTripGroups,
    transitOperatorV2CreateTripGroup,
    transitOperatorV2GetTripGroup,
    transitOperatorV2UpdateTripGroup,
    transitOperatorV2DeleteTripGroup,
    transitOperatorV2ListTrips,
    transitOperatorV2ReplaceTrips,
    transitOperatorV2ListDutyRepeats,
    transitOperatorV2CreateDutyRepeat,
    transitOperatorV2PreviewDutyRepeats,
    transitOperatorV2GenerateDutyRepeats,
    transitOperatorV2UpdateDutyRepeat,
    transitOperatorV2DeleteDutyRepeat,
    transitOperatorV2ListDutyGroups,
    transitOperatorV2CreateDutyGroup,
    transitOperatorV2GetDutyGroup,
    transitOperatorV2DeleteDutyGroup,
    transitOperatorV2ListDuties,
    transitOperatorV2GetDutyGroupLogs,
    transitOperatorV2UpdateDutyGroupVehicle,
    transitOperatorV2UpdateDutyGroupCrew,
    transitOperatorV2SetDutyGroupActive,
    transitOperatorV2UpdateDutyCrew,
    transitOperatorV2ListGenerationFailures,
    transitOperatorV2ResolveGenerationFailure,
  )
where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified "bms-backend" Lib.BmsBackend.Transit.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.V2.API as API
import qualified "bms-backend" Lib.BmsBackend.Transit.V2.Types
import Servant
import SharedLogic.TransitOperatorV2 (tms, tmsUnit, transitCtx)
import Tools.Auth

transitOperatorV2ListTripGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.TripGroupList)
transitOperatorV2ListTripGroups merchantShortId opCity limit offset zone shift tripType depotId code search crew vehicle requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listTripGroups" API.listTripGroupsAPI (API.listTripGroups ctx.gtfsId limit offset zone shift tripType depotId code search crew vehicle)

transitOperatorV2CreateTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateTripGroupReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.TripGroup)
transitOperatorV2CreateTripGroup merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createTripGroup" API.createTripGroupAPI (API.createTripGroup ctx.gtfsId req)

transitOperatorV2GetTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.TripGroup)
transitOperatorV2GetTripGroup merchantShortId opCity tripGroupId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getTripGroup" API.getTripGroupAPI (API.getTripGroup ctx.gtfsId tripGroupId)

transitOperatorV2UpdateTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateTripGroupReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.TripGroup)
transitOperatorV2UpdateTripGroup merchantShortId opCity tripGroupId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateTripGroup" API.updateTripGroupAPI (API.updateTripGroup ctx.gtfsId tripGroupId req)

transitOperatorV2DeleteTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteTripGroup merchantShortId opCity tripGroupId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteTripGroup" API.deleteTripGroupAPI (API.deleteTripGroup ctx.gtfsId tripGroupId)

transitOperatorV2ListTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Lib.BmsBackend.Transit.V2.Types.Trip])
transitOperatorV2ListTrips merchantShortId opCity tripGroupId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listTrips" API.listTripsAPI (API.listTrips ctx.gtfsId tripGroupId)

transitOperatorV2ReplaceTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.ReplaceTripsReq -> Environment.Flow [Lib.BmsBackend.Transit.V2.Types.Trip])
transitOperatorV2ReplaceTrips merchantShortId opCity tripGroupId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "replaceTrips" API.replaceTripsAPI (API.replaceTrips ctx.gtfsId tripGroupId req)

transitOperatorV2ListDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyRepeatViewList)
transitOperatorV2ListDutyRepeats merchantShortId opCity limit offset tripGroupId code search vehicle crew repeatStatus requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listDutyRepeats" API.listDutyRepeatsAPI (API.listDutyRepeats ctx.gtfsId limit offset tripGroupId code search vehicle crew repeatStatus)

transitOperatorV2CreateDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateDutyRepeatReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp)
transitOperatorV2CreateDutyRepeat merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createDutyRepeat" API.createDutyRepeatAPI (API.createDutyRepeat ctx.gtfsId req)

transitOperatorV2PreviewDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq -> Environment.Flow [Lib.BmsBackend.Transit.V2.Types.GenerateEntry])
transitOperatorV2PreviewDutyRepeats merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "previewDutyRepeats" API.previewDutyRepeatsAPI (API.previewDutyRepeats ctx.gtfsId req)

transitOperatorV2GenerateDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq -> Environment.Flow [Lib.BmsBackend.Transit.V2.Types.GenerateEntry])
transitOperatorV2GenerateDutyRepeats merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "generateDutyRepeats" API.generateDutyRepeatsAPI (API.generateDutyRepeats ctx.gtfsId req)

transitOperatorV2UpdateDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateDutyRepeatReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp)
transitOperatorV2UpdateDutyRepeat merchantShortId opCity dutyRepeatId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateDutyRepeat" API.updateDutyRepeatAPI (API.updateDutyRepeat ctx.gtfsId dutyRepeatId req)

transitOperatorV2DeleteDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDutyRepeat merchantShortId opCity dutyRepeatId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteDutyRepeat" API.deleteDutyRepeatAPI (API.deleteDutyRepeat ctx.gtfsId dutyRepeatId)

transitOperatorV2ListDutyGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyGroupListItemList)
transitOperatorV2ListDutyGroups merchantShortId opCity limit offset date from to tripGroupId depotId vehicle crew status search requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listDutyGroups" API.listDutyGroupsAPI (API.listDutyGroups ctx.gtfsId limit offset date from to tripGroupId depotId vehicle crew status search)

transitOperatorV2CreateDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateDutyGroupReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail)
transitOperatorV2CreateDutyGroup merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createDutyGroup" API.createDutyGroupAPI (API.createDutyGroup ctx.gtfsId req)

transitOperatorV2GetDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail)
transitOperatorV2GetDutyGroup merchantShortId opCity dutyGroupId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getDutyGroup" API.getDutyGroupAPI (API.getDutyGroup ctx.gtfsId dutyGroupId)

transitOperatorV2DeleteDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDutyGroup merchantShortId opCity dutyGroupId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteDutyGroup" API.deleteDutyGroupAPI (API.deleteDutyGroup ctx.gtfsId dutyGroupId)

transitOperatorV2ListDuties :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Lib.BmsBackend.Transit.V2.Types.Duty])
transitOperatorV2ListDuties merchantShortId opCity dutyGroupId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listDuties" API.listDutiesAPI (API.listDuties ctx.gtfsId dutyGroupId)

transitOperatorV2GetDutyGroupLogs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Lib.BmsBackend.Transit.V2.Types.DutyEventLog])
transitOperatorV2GetDutyGroupLogs merchantShortId opCity dutyGroupId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getDutyGroupLogs" API.getDutyGroupLogsAPI (API.getDutyGroupLogs ctx.gtfsId dutyGroupId)

transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateVehicleReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.VehicleChange)
transitOperatorV2UpdateDutyGroupVehicle merchantShortId opCity dutyGroupId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateDutyGroupVehicle" API.updateDutyGroupVehicleAPI (API.updateDutyGroupVehicle ctx.gtfsId dutyGroupId req)

transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail)
transitOperatorV2UpdateDutyGroupCrew merchantShortId opCity dutyGroupId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateDutyGroupCrew" API.updateDutyGroupCrewAPI (API.updateDutyGroupCrew ctx.gtfsId dutyGroupId req)

transitOperatorV2SetDutyGroupActive :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.SetActiveReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyGroup)
transitOperatorV2SetDutyGroupActive merchantShortId opCity dutyGroupId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "setDutyGroupActive" API.setDutyGroupActiveAPI (API.setDutyGroupActive ctx.gtfsId dutyGroupId req)

transitOperatorV2UpdateDutyCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.Duty)
transitOperatorV2UpdateDutyCrew merchantShortId opCity dutyId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateDutyCrew" API.updateDutyCrewAPI (API.updateDutyCrew ctx.gtfsId dutyId req)

transitOperatorV2ListGenerationFailures :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyEventLogList)
transitOperatorV2ListGenerationFailures merchantShortId opCity limit offset resolved requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listGenerationFailures" API.listGenerationFailuresAPI (API.listGenerationFailures ctx.gtfsId limit offset resolved)

transitOperatorV2ResolveGenerationFailure :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V2.Types.DutyEventLog)
transitOperatorV2ResolveGenerationFailure merchantShortId opCity failureId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "resolveGenerationFailure" API.resolveGenerationFailureAPI (API.resolveGenerationFailure ctx.gtfsId failureId)
