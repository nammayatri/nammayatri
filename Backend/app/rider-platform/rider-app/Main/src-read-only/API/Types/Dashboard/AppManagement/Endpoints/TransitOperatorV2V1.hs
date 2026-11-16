{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TransitOperatorV2V1 where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.V1.Types
import Servant
import Servant.Client

type API = ("transitOperatorV2" :> (TransitV1ListSchedulesHelper :<|> TransitV1CreateScheduleHelper :<|> TransitV1GetScheduleHelper :<|> TransitV1UpdateScheduleHelper :<|> TransitV1DeleteScheduleHelper :<|> TransitV1ListScheduleTypesHelper :<|> TransitV1ListScheduleTripsHelper :<|> TransitV1CreateScheduleTripHelper :<|> TransitV1GetScheduleTripHelper :<|> TransitV1UpdateScheduleTripHelper :<|> TransitV1DeleteScheduleTripHelper :<|> TransitV1GetRepeatConfigHelper :<|> TransitV1SetRepeatConfigHelper :<|> TransitV1ListTripDetailsHelper :<|> TransitV1ReplaceTripDetailsHelper :<|> TransitV1UpdateTripDetailHelper :<|> TransitV1DeleteTripDetailHelper :<|> TransitV1ListWaybillsHelper :<|> TransitV1CreateWaybillHelper :<|> TransitV1GetWaybillHelper :<|> TransitV1UpdateWaybillHelper :<|> TransitV1DeleteWaybillHelper :<|> TransitV1PreviewWaybillRepeatsHelper :<|> TransitV1GenerateWaybillRepeatsHelper))

type TransitV1ListSchedules =
  ( "v1" :> "schedules" :> QueryParam "depotId" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "routeId" Kernel.Prelude.Text
      :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleList
  )

type TransitV1ListSchedulesHelper =
  ( "v1" :> "schedules" :> QueryParam "depotId" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "routeId" Kernel.Prelude.Text
      :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleList
  )

type TransitV1CreateSchedule =
  ( "v1" :> "schedules" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.CreateScheduleReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Schedule
  )

type TransitV1CreateScheduleHelper =
  ( "v1" :> "schedules" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.CreateScheduleReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Schedule
  )

type TransitV1GetSchedule =
  ( "v1" :> "schedules" :> Capture "scheduleId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.Schedule
  )

type TransitV1GetScheduleHelper =
  ( "v1" :> "schedules" :> Capture "scheduleId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Schedule
  )

type TransitV1UpdateSchedule =
  ( "v1" :> "schedules" :> Capture "scheduleId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.UpdateScheduleReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Schedule
  )

type TransitV1UpdateScheduleHelper =
  ( "v1" :> "schedules" :> Capture "scheduleId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.UpdateScheduleReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.Schedule
  )

type TransitV1DeleteSchedule =
  ( "v1" :> "schedules" :> Capture "scheduleId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitV1DeleteScheduleHelper =
  ( "v1" :> "schedules" :> Capture "scheduleId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitV1ListScheduleTypes =
  ( "v1" :> "scheduleTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleTypeItems
  )

type TransitV1ListScheduleTypesHelper =
  ( "v1" :> "scheduleTypes" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.ScheduleTypeItems
  )

type TransitV1ListScheduleTrips =
  ( "v1" :> "scheduleTrips" :> QueryParam "depotId" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "repeatStatus" Kernel.Prelude.Text
      :> QueryParam "scheduleId" Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleTripList
  )

type TransitV1ListScheduleTripsHelper =
  ( "v1" :> "scheduleTrips" :> QueryParam "depotId" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "repeatStatus" Kernel.Prelude.Text
      :> QueryParam
           "scheduleId"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleTripList
  )

type TransitV1CreateScheduleTrip =
  ( "v1" :> "scheduleTrips" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.CreateScheduleTripReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V1.Types.ScheduleTrip
  )

type TransitV1CreateScheduleTripHelper =
  ( "v1" :> "scheduleTrips" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.CreateScheduleTripReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleTrip
  )

type TransitV1GetScheduleTrip =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.ScheduleTrip
  )

type TransitV1GetScheduleTripHelper =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.ScheduleTrip
  )

type TransitV1UpdateScheduleTrip =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.UpdateScheduleTripReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleTrip
  )

type TransitV1UpdateScheduleTripHelper =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.UpdateScheduleTripReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ScheduleTrip
  )

type TransitV1DeleteScheduleTrip =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitV1DeleteScheduleTripHelper =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitV1GetRepeatConfig =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "repeat"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.RepeatConfig
  )

type TransitV1GetRepeatConfigHelper =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "repeat"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.RepeatConfig
  )

type TransitV1SetRepeatConfig =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "repeat"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.SetRepeatConfigReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.RepeatConfig
  )

type TransitV1SetRepeatConfigHelper =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "repeat"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.SetRepeatConfigReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.RepeatConfig
  )

type TransitV1ListTripDetails =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "details"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.TripDetailItems
  )

type TransitV1ListTripDetailsHelper =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "details"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.TripDetailItems
  )

type TransitV1ReplaceTripDetails =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "details"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.ReplaceTripDetailsReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.TripDetailItems
  )

type TransitV1ReplaceTripDetailsHelper =
  ( "v1" :> "scheduleTrips" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "details"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.ReplaceTripDetailsReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.TripDetailItems
  )

type TransitV1UpdateTripDetail =
  ( "v1" :> "tripDetails" :> Capture "tripDetailId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.UpdateTripDetailReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.V1.Types.TripDetail
  )

type TransitV1UpdateTripDetailHelper =
  ( "v1" :> "tripDetails" :> Capture "tripDetailId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.UpdateTripDetailReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.TripDetail
  )

type TransitV1DeleteTripDetail =
  ( "v1" :> "tripDetails" :> Capture "tripDetailId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitV1DeleteTripDetailHelper =
  ( "v1" :> "tripDetails" :> Capture "tripDetailId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitV1ListWaybills =
  ( "v1" :> "waybills" :> QueryParam "conductorToken" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text
      :> QueryParam
           "driverToken"
           Kernel.Prelude.Text
      :> QueryParam "dutyDate" Data.Time.Day
      :> QueryParam "dutyDateFrom" Data.Time.Day
      :> QueryParam
           "dutyDateTo"
           Data.Time.Day
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "scheduleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "scheduleTripId"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "sort"
           Kernel.Prelude.Text
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "waybillNo"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.WaybillList
  )

type TransitV1ListWaybillsHelper =
  ( "v1" :> "waybills" :> QueryParam "conductorToken" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text
      :> QueryParam
           "driverToken"
           Kernel.Prelude.Text
      :> QueryParam "dutyDate" Data.Time.Day
      :> QueryParam "dutyDateFrom" Data.Time.Day
      :> QueryParam
           "dutyDateTo"
           Data.Time.Day
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "scheduleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "scheduleTripId"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "sort"
           Kernel.Prelude.Text
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> QueryParam
           "waybillNo"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.WaybillList
  )

type TransitV1CreateWaybill =
  ( "v1" :> "waybills" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.CreateWaybillReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Waybill
  )

type TransitV1CreateWaybillHelper =
  ( "v1" :> "waybills" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.CreateWaybillReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Waybill
  )

type TransitV1GetWaybill =
  ( "v1" :> "waybills" :> Capture "waybillId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.Waybill
  )

type TransitV1GetWaybillHelper =
  ( "v1" :> "waybills" :> Capture "waybillId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Waybill
  )

type TransitV1UpdateWaybill =
  ( "v1" :> "waybills" :> Capture "waybillId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.UpdateWaybillReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.V1.Types.Waybill
  )

type TransitV1UpdateWaybillHelper =
  ( "v1" :> "waybills" :> Capture "waybillId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.UpdateWaybillReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.Waybill
  )

type TransitV1DeleteWaybill =
  ( "v1" :> "waybills" :> Capture "waybillId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitV1DeleteWaybillHelper =
  ( "v1" :> "waybills" :> Capture "waybillId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitV1PreviewWaybillRepeats =
  ( "v1" :> "waybillRepeats" :> "preview" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse
  )

type TransitV1PreviewWaybillRepeatsHelper =
  ( "v1" :> "waybillRepeats" :> "preview" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse
  )

type TransitV1GenerateWaybillRepeats =
  ( "v1" :> "waybillRepeats" :> "generate" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse
  )

type TransitV1GenerateWaybillRepeatsHelper =
  ( "v1" :> "waybillRepeats" :> "generate" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse
  )

data TransitOperatorV2V1APIs = TransitOperatorV2V1APIs
  { transitV1ListSchedules :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.ScheduleList),
    transitV1CreateSchedule :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateScheduleReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.Schedule),
    transitV1GetSchedule :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.Schedule),
    transitV1UpdateSchedule :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateScheduleReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.Schedule),
    transitV1DeleteSchedule :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitV1ListScheduleTypes :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.ScheduleTypeItems),
    transitV1ListScheduleTrips :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.ScheduleTripList),
    transitV1CreateScheduleTrip :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateScheduleTripReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.ScheduleTrip),
    transitV1GetScheduleTrip :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.ScheduleTrip),
    transitV1UpdateScheduleTrip :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateScheduleTripReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.ScheduleTrip),
    transitV1DeleteScheduleTrip :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitV1GetRepeatConfig :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.RepeatConfig),
    transitV1SetRepeatConfig :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.SetRepeatConfigReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.RepeatConfig),
    transitV1ListTripDetails :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.TripDetailItems),
    transitV1ReplaceTripDetails :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.ReplaceTripDetailsReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.TripDetailItems),
    transitV1UpdateTripDetail :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateTripDetailReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.TripDetail),
    transitV1DeleteTripDetail :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitV1ListWaybills :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.WaybillList),
    transitV1CreateWaybill :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateWaybillReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.Waybill),
    transitV1GetWaybill :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.Waybill),
    transitV1UpdateWaybill :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateWaybillReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.Waybill),
    transitV1DeleteWaybill :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitV1PreviewWaybillRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse),
    transitV1GenerateWaybillRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse)
  }

mkTransitOperatorV2V1APIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorV2V1APIs)
mkTransitOperatorV2V1APIs transitOperatorV2V1Client = (TransitOperatorV2V1APIs {..})
  where
    transitV1ListSchedules :<|> transitV1CreateSchedule :<|> transitV1GetSchedule :<|> transitV1UpdateSchedule :<|> transitV1DeleteSchedule :<|> transitV1ListScheduleTypes :<|> transitV1ListScheduleTrips :<|> transitV1CreateScheduleTrip :<|> transitV1GetScheduleTrip :<|> transitV1UpdateScheduleTrip :<|> transitV1DeleteScheduleTrip :<|> transitV1GetRepeatConfig :<|> transitV1SetRepeatConfig :<|> transitV1ListTripDetails :<|> transitV1ReplaceTripDetails :<|> transitV1UpdateTripDetail :<|> transitV1DeleteTripDetail :<|> transitV1ListWaybills :<|> transitV1CreateWaybill :<|> transitV1GetWaybill :<|> transitV1UpdateWaybill :<|> transitV1DeleteWaybill :<|> transitV1PreviewWaybillRepeats :<|> transitV1GenerateWaybillRepeats = transitOperatorV2V1Client

data TransitOperatorV2V1UserActionType
  = TRANSIT_V1_LIST_SCHEDULES
  | TRANSIT_V1_CREATE_SCHEDULE
  | TRANSIT_V1_GET_SCHEDULE
  | TRANSIT_V1_UPDATE_SCHEDULE
  | TRANSIT_V1_DELETE_SCHEDULE
  | TRANSIT_V1_LIST_SCHEDULE_TYPES
  | TRANSIT_V1_LIST_SCHEDULE_TRIPS
  | TRANSIT_V1_CREATE_SCHEDULE_TRIP
  | TRANSIT_V1_GET_SCHEDULE_TRIP
  | TRANSIT_V1_UPDATE_SCHEDULE_TRIP
  | TRANSIT_V1_DELETE_SCHEDULE_TRIP
  | TRANSIT_V1_GET_REPEAT_CONFIG
  | TRANSIT_V1_SET_REPEAT_CONFIG
  | TRANSIT_V1_LIST_TRIP_DETAILS
  | TRANSIT_V1_REPLACE_TRIP_DETAILS
  | TRANSIT_V1_UPDATE_TRIP_DETAIL
  | TRANSIT_V1_DELETE_TRIP_DETAIL
  | TRANSIT_V1_LIST_WAYBILLS
  | TRANSIT_V1_CREATE_WAYBILL
  | TRANSIT_V1_GET_WAYBILL
  | TRANSIT_V1_UPDATE_WAYBILL
  | TRANSIT_V1_DELETE_WAYBILL
  | TRANSIT_V1_PREVIEW_WAYBILL_REPEATS
  | TRANSIT_V1_GENERATE_WAYBILL_REPEATS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorV2V1UserActionType)])
