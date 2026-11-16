{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.TransitOperatorV2V1
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperatorV2V1
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Control.Lens
import qualified Data.Time
import qualified Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.V1.Types
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("transitOperatorV2" :> (TransitV1ListSchedules :<|> TransitV1CreateSchedule :<|> TransitV1GetSchedule :<|> TransitV1UpdateSchedule :<|> TransitV1DeleteSchedule :<|> TransitV1ListScheduleTypes :<|> TransitV1ListScheduleTrips :<|> TransitV1CreateScheduleTrip :<|> TransitV1GetScheduleTrip :<|> TransitV1UpdateScheduleTrip :<|> TransitV1DeleteScheduleTrip :<|> TransitV1GetRepeatConfig :<|> TransitV1SetRepeatConfig :<|> TransitV1ListTripDetails :<|> TransitV1ReplaceTripDetails :<|> TransitV1UpdateTripDetail :<|> TransitV1DeleteTripDetail :<|> TransitV1ListWaybills :<|> TransitV1CreateWaybill :<|> TransitV1GetWaybill :<|> TransitV1UpdateWaybill :<|> TransitV1DeleteWaybill :<|> TransitV1PreviewWaybillRepeats :<|> TransitV1GenerateWaybillRepeats))

type TransitV1ListSchedules =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_SCHEDULES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1ListSchedules
  )

type TransitV1CreateSchedule =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_SCHEDULE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1CreateSchedule
  )

type TransitV1GetSchedule =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_SCHEDULE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1GetSchedule
  )

type TransitV1UpdateSchedule =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_SCHEDULE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1UpdateSchedule
  )

type TransitV1DeleteSchedule =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_SCHEDULE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1DeleteSchedule
  )

type TransitV1ListScheduleTypes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_SCHEDULE_TYPES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1ListScheduleTypes
  )

type TransitV1ListScheduleTrips =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_SCHEDULE_TRIPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1ListScheduleTrips
  )

type TransitV1CreateScheduleTrip =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_SCHEDULE_TRIP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1CreateScheduleTrip
  )

type TransitV1GetScheduleTrip =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_SCHEDULE_TRIP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1GetScheduleTrip
  )

type TransitV1UpdateScheduleTrip =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_SCHEDULE_TRIP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1UpdateScheduleTrip
  )

type TransitV1DeleteScheduleTrip =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_SCHEDULE_TRIP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1DeleteScheduleTrip
  )

type TransitV1GetRepeatConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_REPEAT_CONFIG"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1GetRepeatConfig
  )

type TransitV1SetRepeatConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_SET_REPEAT_CONFIG"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1SetRepeatConfig
  )

type TransitV1ListTripDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_TRIP_DETAILS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1ListTripDetails
  )

type TransitV1ReplaceTripDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_REPLACE_TRIP_DETAILS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1ReplaceTripDetails
  )

type TransitV1UpdateTripDetail =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_TRIP_DETAIL"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1UpdateTripDetail
  )

type TransitV1DeleteTripDetail =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_TRIP_DETAIL"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1DeleteTripDetail
  )

type TransitV1ListWaybills =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_WAYBILLS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1ListWaybills
  )

type TransitV1CreateWaybill =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_WAYBILL"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1CreateWaybill
  )

type TransitV1GetWaybill =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_WAYBILL"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1GetWaybill
  )

type TransitV1UpdateWaybill =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_WAYBILL"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1UpdateWaybill
  )

type TransitV1DeleteWaybill =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_WAYBILL"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1DeleteWaybill
  )

type TransitV1PreviewWaybillRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_PREVIEW_WAYBILL_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1PreviewWaybillRepeats
  )

type TransitV1GenerateWaybillRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GENERATE_WAYBILL_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2V1.TransitV1GenerateWaybillRepeats
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitV1ListSchedules merchantId city :<|> transitV1CreateSchedule merchantId city :<|> transitV1GetSchedule merchantId city :<|> transitV1UpdateSchedule merchantId city :<|> transitV1DeleteSchedule merchantId city :<|> transitV1ListScheduleTypes merchantId city :<|> transitV1ListScheduleTrips merchantId city :<|> transitV1CreateScheduleTrip merchantId city :<|> transitV1GetScheduleTrip merchantId city :<|> transitV1UpdateScheduleTrip merchantId city :<|> transitV1DeleteScheduleTrip merchantId city :<|> transitV1GetRepeatConfig merchantId city :<|> transitV1SetRepeatConfig merchantId city :<|> transitV1ListTripDetails merchantId city :<|> transitV1ReplaceTripDetails merchantId city :<|> transitV1UpdateTripDetail merchantId city :<|> transitV1DeleteTripDetail merchantId city :<|> transitV1ListWaybills merchantId city :<|> transitV1CreateWaybill merchantId city :<|> transitV1GetWaybill merchantId city :<|> transitV1UpdateWaybill merchantId city :<|> transitV1DeleteWaybill merchantId city :<|> transitV1PreviewWaybillRepeats merchantId city :<|> transitV1GenerateWaybillRepeats merchantId city

transitV1ListSchedules :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.ScheduleList)
transitV1ListSchedules a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a8 a10 a9
        Tools.ActorInfo.withDashboardUserActorInfo a8 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1ListSchedules a10 a9 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a8)) a1
    )

transitV1CreateSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateScheduleReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.Schedule)
transitV1CreateSchedule a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_SCHEDULE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1CreateSchedule a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitV1GetSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.Schedule)
transitV1GetSchedule a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1GetSchedule a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1UpdateSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateScheduleReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.Schedule)
transitV1UpdateSchedule a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_SCHEDULE" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1UpdateSchedule a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitV1DeleteSchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitV1DeleteSchedule a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_SCHEDULE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1DeleteSchedule a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1ListScheduleTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.ScheduleTypeItems)
transitV1ListScheduleTypes a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1ListScheduleTypes a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
    )

transitV1ListScheduleTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.ScheduleTripList)
transitV1ListScheduleTrips a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a8 a10 a9
        Tools.ActorInfo.withDashboardUserActorInfo a8 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1ListScheduleTrips a10 a9 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a8)) a1
    )

transitV1CreateScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateScheduleTripReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.ScheduleTrip)
transitV1CreateScheduleTrip a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_SCHEDULE_TRIP" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1CreateScheduleTrip a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitV1GetScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.ScheduleTrip)
transitV1GetScheduleTrip a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1GetScheduleTrip a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1UpdateScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateScheduleTripReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.ScheduleTrip)
transitV1UpdateScheduleTrip a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_SCHEDULE_TRIP" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1UpdateScheduleTrip a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitV1DeleteScheduleTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitV1DeleteScheduleTrip a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_SCHEDULE_TRIP" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1DeleteScheduleTrip a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1GetRepeatConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.RepeatConfig)
transitV1GetRepeatConfig a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1GetRepeatConfig a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1SetRepeatConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.SetRepeatConfigReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.RepeatConfig)
transitV1SetRepeatConfig a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_SET_REPEAT_CONFIG" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1SetRepeatConfig a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitV1ListTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.TripDetailItems)
transitV1ListTripDetails a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1ListTripDetails a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1ReplaceTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.ReplaceTripDetailsReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.TripDetailItems)
transitV1ReplaceTripDetails a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_REPLACE_TRIP_DETAILS" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1ReplaceTripDetails a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitV1UpdateTripDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateTripDetailReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.TripDetail)
transitV1UpdateTripDetail a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_TRIP_DETAIL" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1UpdateTripDetail a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitV1DeleteTripDetail :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitV1DeleteTripDetail a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_TRIP_DETAIL" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1DeleteTripDetail a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1ListWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.WaybillList)
transitV1ListWaybills a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a17 a19 a18
        Tools.ActorInfo.withDashboardUserActorInfo a17 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1ListWaybills a19 a18 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a17)) a1
    )

transitV1CreateWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.CreateWaybillReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.Waybill)
transitV1CreateWaybill a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_WAYBILL" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1CreateWaybill a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitV1GetWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.Waybill)
transitV1GetWaybill a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1GetWaybill a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1UpdateWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.UpdateWaybillReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.Waybill)
transitV1UpdateWaybill a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_WAYBILL" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1UpdateWaybill a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitV1DeleteWaybill :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitV1DeleteWaybill a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_WAYBILL" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1DeleteWaybill a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitV1PreviewWaybillRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse)
transitV1PreviewWaybillRepeats a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_PREVIEW_WAYBILL_REPEATS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1PreviewWaybillRepeats a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitV1GenerateWaybillRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V1.Types.RepeatRangeReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V1.Types.RepeatWalkResponse)
transitV1GenerateWaybillRepeats a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GENERATE_WAYBILL_REPEATS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2V1.transitV1GenerateWaybillRepeats a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )
