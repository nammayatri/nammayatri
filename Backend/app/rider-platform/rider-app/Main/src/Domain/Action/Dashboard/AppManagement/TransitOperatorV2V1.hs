{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1
  ( transitV1ListSchedules,
    transitV1CreateSchedule,
    transitV1GetSchedule,
    transitV1UpdateSchedule,
    transitV1DeleteSchedule,
    transitV1ListScheduleTypes,
    transitV1ListScheduleTrips,
    transitV1CreateScheduleTrip,
    transitV1GetScheduleTrip,
    transitV1UpdateScheduleTrip,
    transitV1DeleteScheduleTrip,
    transitV1GetRepeatConfig,
    transitV1SetRepeatConfig,
    transitV1ListTripDetails,
    transitV1ReplaceTripDetails,
    transitV1UpdateTripDetail,
    transitV1DeleteTripDetail,
    transitV1ListWaybills,
    transitV1CreateWaybill,
    transitV1GetWaybill,
    transitV1UpdateWaybill,
    transitV1DeleteWaybill,
    transitV1PreviewWaybillRepeats,
    transitV1GenerateWaybillRepeats,
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
import qualified "bms-backend" Lib.BmsBackend.Transit.V1.API as API
import qualified "bms-backend" Lib.BmsBackend.Transit.V1.Types
import Servant
import SharedLogic.TransitOperatorV2 (tms, tmsUnit, transitCtx)
import Tools.Auth

transitV1ListSchedules :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.ScheduleList)
transitV1ListSchedules merchantShortId opCity depotId limit offset routeId search status requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listSchedules" API.listSchedulesAPI (API.listSchedules ctx.gtfsId search depotId routeId status limit offset)

transitV1CreateSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateScheduleReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.Schedule)
transitV1CreateSchedule merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createSchedule" API.createScheduleAPI (API.createSchedule ctx.gtfsId req)

transitV1GetSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.Schedule)
transitV1GetSchedule merchantShortId opCity scheduleId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getSchedule" API.getScheduleAPI (API.getSchedule ctx.gtfsId scheduleId)

transitV1UpdateSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateScheduleReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.Schedule)
transitV1UpdateSchedule merchantShortId opCity scheduleId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateSchedule" API.updateScheduleAPI (API.updateSchedule ctx.gtfsId scheduleId req)

transitV1DeleteSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitV1DeleteSchedule merchantShortId opCity scheduleId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteSchedule" API.deleteScheduleAPI (API.deleteSchedule ctx.gtfsId scheduleId)

transitV1ListScheduleTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.ScheduleTypeItems)
transitV1ListScheduleTypes merchantShortId opCity requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listScheduleTypes" API.listScheduleTypesAPI (API.listScheduleTypes ctx.gtfsId)

transitV1ListScheduleTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.ScheduleTripList)
transitV1ListScheduleTrips merchantShortId opCity depotId limit offset repeatStatus scheduleId search requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listScheduleTrips" API.listScheduleTripsAPI (API.listScheduleTrips ctx.gtfsId scheduleId depotId search repeatStatus limit offset)

transitV1CreateScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateScheduleTripReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.ScheduleTrip)
transitV1CreateScheduleTrip merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createScheduleTrip" API.createScheduleTripAPI (API.createScheduleTrip ctx.gtfsId req)

transitV1GetScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.ScheduleTrip)
transitV1GetScheduleTrip merchantShortId opCity scheduleTripId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getScheduleTrip" API.getScheduleTripAPI (API.getScheduleTrip ctx.gtfsId scheduleTripId)

transitV1UpdateScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateScheduleTripReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.ScheduleTrip)
transitV1UpdateScheduleTrip merchantShortId opCity scheduleTripId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateScheduleTrip" API.updateScheduleTripAPI (API.updateScheduleTrip ctx.gtfsId scheduleTripId req)

transitV1DeleteScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitV1DeleteScheduleTrip merchantShortId opCity scheduleTripId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteScheduleTrip" API.deleteScheduleTripAPI (API.deleteScheduleTrip ctx.gtfsId scheduleTripId)

transitV1GetRepeatConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.RepeatConfig)
transitV1GetRepeatConfig merchantShortId opCity scheduleTripId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getRepeatConfig" API.getRepeatConfigAPI (API.getRepeatConfig ctx.gtfsId scheduleTripId)

transitV1SetRepeatConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.SetRepeatConfigReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.RepeatConfig)
transitV1SetRepeatConfig merchantShortId opCity scheduleTripId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "setRepeatConfig" API.setRepeatConfigAPI (API.setRepeatConfig ctx.gtfsId scheduleTripId req)

transitV1ListTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.TripDetailItems)
transitV1ListTripDetails merchantShortId opCity scheduleTripId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listTripDetails" API.listTripDetailsAPI (API.listTripDetails ctx.gtfsId scheduleTripId)

transitV1ReplaceTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.ReplaceTripDetailsReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.TripDetailItems)
transitV1ReplaceTripDetails merchantShortId opCity scheduleTripId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "replaceTripDetails" API.replaceTripDetailsAPI (API.replaceTripDetails ctx.gtfsId scheduleTripId req)

transitV1UpdateTripDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateTripDetailReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.TripDetail)
transitV1UpdateTripDetail merchantShortId opCity tripDetailId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateTripDetail" API.updateTripDetailAPI (API.updateTripDetail ctx.gtfsId tripDetailId req)

transitV1DeleteTripDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitV1DeleteTripDetail merchantShortId opCity tripDetailId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteTripDetail" API.deleteTripDetailAPI (API.deleteTripDetail ctx.gtfsId tripDetailId)

transitV1ListWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.WaybillList)
transitV1ListWaybills merchantShortId opCity conductorToken depotId driverToken dutyDate dutyDateFrom dutyDateTo limit offset scheduleNo scheduleTripId search sortKey status vehicleNo waybillNo requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listWaybills" API.listWaybillsAPI (API.listWaybills ctx.gtfsId vehicleNo dutyDate dutyDateFrom dutyDateTo status waybillNo driverToken conductorToken depotId scheduleNo scheduleTripId search sortKey limit offset)

transitV1CreateWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateWaybillReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.Waybill)
transitV1CreateWaybill merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createWaybill" API.createWaybillAPI (API.createWaybill ctx.gtfsId req)

transitV1GetWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.Waybill)
transitV1GetWaybill merchantShortId opCity waybillId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getWaybill" API.getWaybillAPI (API.getWaybill ctx.gtfsId waybillId)

transitV1UpdateWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateWaybillReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.Waybill)
transitV1UpdateWaybill merchantShortId opCity waybillId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateWaybill" API.updateWaybillAPI (API.updateWaybill ctx.gtfsId waybillId req)

transitV1DeleteWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitV1DeleteWaybill merchantShortId opCity waybillId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteWaybill" API.deleteWaybillAPI (API.deleteWaybill ctx.gtfsId waybillId)

transitV1PreviewWaybillRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse)
transitV1PreviewWaybillRepeats merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "previewWaybillRepeats" API.previewWaybillRepeatsAPI (API.previewWaybillRepeats ctx.gtfsId req)

transitV1GenerateWaybillRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq -> Environment.Flow Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse)
transitV1GenerateWaybillRepeats merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "generateWaybillRepeats" API.generateWaybillRepeatsAPI (API.generateWaybillRepeats ctx.gtfsId req)
