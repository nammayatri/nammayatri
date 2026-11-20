{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.TransitOperatorV2Schedules
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Control.Lens
import qualified Data.Time
import qualified Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.V2.Types
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("transitOperatorV2" :> (TransitOperatorV2ListTripGroups :<|> TransitOperatorV2CreateTripGroup :<|> TransitOperatorV2GetTripGroup :<|> TransitOperatorV2UpdateTripGroup :<|> TransitOperatorV2DeleteTripGroup :<|> TransitOperatorV2ListTrips :<|> TransitOperatorV2ReplaceTrips :<|> TransitOperatorV2ListDutyRepeats :<|> TransitOperatorV2CreateDutyRepeat :<|> TransitOperatorV2PreviewDutyRepeats :<|> TransitOperatorV2GenerateDutyRepeats :<|> TransitOperatorV2UpdateDutyRepeat :<|> TransitOperatorV2DeleteDutyRepeat :<|> TransitOperatorV2ListDutyGroups :<|> TransitOperatorV2CreateDutyGroup :<|> TransitOperatorV2GetDutyGroup :<|> TransitOperatorV2DeleteDutyGroup :<|> TransitOperatorV2ListDuties :<|> TransitOperatorV2GetDutyGroupLogs :<|> TransitOperatorV2UpdateDutyGroupVehicle :<|> TransitOperatorV2UpdateDutyGroupCrew :<|> TransitOperatorV2SetDutyGroupActive :<|> TransitOperatorV2UpdateDutyCrew :<|> TransitOperatorV2ListGenerationFailures :<|> TransitOperatorV2ResolveGenerationFailure))

type TransitOperatorV2ListTripGroups =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_LIST_TRIP_GROUPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ListTripGroups
  )

type TransitOperatorV2CreateTripGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_CREATE_TRIP_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2CreateTripGroup
  )

type TransitOperatorV2GetTripGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_GET_TRIP_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2GetTripGroup
  )

type TransitOperatorV2UpdateTripGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_TRIP_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2UpdateTripGroup
  )

type TransitOperatorV2DeleteTripGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2DeleteTripGroup
  )

type TransitOperatorV2ListTrips =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_LIST_TRIPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ListTrips
  )

type TransitOperatorV2ReplaceTrips =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_REPLACE_TRIPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ReplaceTrips
  )

type TransitOperatorV2ListDutyRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_LIST_DUTY_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ListDutyRepeats
  )

type TransitOperatorV2CreateDutyRepeat =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_CREATE_DUTY_REPEAT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2CreateDutyRepeat
  )

type TransitOperatorV2PreviewDutyRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2PreviewDutyRepeats
  )

type TransitOperatorV2GenerateDutyRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2GenerateDutyRepeats
  )

type TransitOperatorV2UpdateDutyRepeat =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_REPEAT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2UpdateDutyRepeat
  )

type TransitOperatorV2DeleteDutyRepeat =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2DeleteDutyRepeat
  )

type TransitOperatorV2ListDutyGroups =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_LIST_DUTY_GROUPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ListDutyGroups
  )

type TransitOperatorV2CreateDutyGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2CreateDutyGroup
  )

type TransitOperatorV2GetDutyGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_GET_DUTY_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2GetDutyGroup
  )

type TransitOperatorV2DeleteDutyGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2DeleteDutyGroup
  )

type TransitOperatorV2ListDuties =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_LIST_DUTIES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ListDuties
  )

type TransitOperatorV2GetDutyGroupLogs =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_GET_DUTY_GROUP_LOGS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2GetDutyGroupLogs
  )

type TransitOperatorV2UpdateDutyGroupVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2UpdateDutyGroupVehicle
  )

type TransitOperatorV2UpdateDutyGroupCrew =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2UpdateDutyGroupCrew
  )

type TransitOperatorV2SetDutyGroupActive =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2SetDutyGroupActive
  )

type TransitOperatorV2UpdateDutyCrew =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2UpdateDutyCrew
  )

type TransitOperatorV2ListGenerationFailures =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_LIST_GENERATION_FAILURES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ListGenerationFailures
  )

type TransitOperatorV2ResolveGenerationFailure =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Schedules.TransitOperatorV2ResolveGenerationFailure
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitOperatorV2ListTripGroups merchantId city :<|> transitOperatorV2CreateTripGroup merchantId city :<|> transitOperatorV2GetTripGroup merchantId city :<|> transitOperatorV2UpdateTripGroup merchantId city :<|> transitOperatorV2DeleteTripGroup merchantId city :<|> transitOperatorV2ListTrips merchantId city :<|> transitOperatorV2ReplaceTrips merchantId city :<|> transitOperatorV2ListDutyRepeats merchantId city :<|> transitOperatorV2CreateDutyRepeat merchantId city :<|> transitOperatorV2PreviewDutyRepeats merchantId city :<|> transitOperatorV2GenerateDutyRepeats merchantId city :<|> transitOperatorV2UpdateDutyRepeat merchantId city :<|> transitOperatorV2DeleteDutyRepeat merchantId city :<|> transitOperatorV2ListDutyGroups merchantId city :<|> transitOperatorV2CreateDutyGroup merchantId city :<|> transitOperatorV2GetDutyGroup merchantId city :<|> transitOperatorV2DeleteDutyGroup merchantId city :<|> transitOperatorV2ListDuties merchantId city :<|> transitOperatorV2GetDutyGroupLogs merchantId city :<|> transitOperatorV2UpdateDutyGroupVehicle merchantId city :<|> transitOperatorV2UpdateDutyGroupCrew merchantId city :<|> transitOperatorV2SetDutyGroupActive merchantId city :<|> transitOperatorV2UpdateDutyCrew merchantId city :<|> transitOperatorV2ListGenerationFailures merchantId city :<|> transitOperatorV2ResolveGenerationFailure merchantId city

transitOperatorV2ListTripGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.TripGroupList)
transitOperatorV2ListTripGroups a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a12 a14 a13
        Tools.ActorInfo.withDashboardUserActorInfo a12 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ListTripGroups a14 a13 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a12)) a1
    )

transitOperatorV2CreateTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateTripGroupReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.TripGroup)
transitOperatorV2CreateTripGroup a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_CREATE_TRIP_GROUP" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2CreateTripGroup a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2GetTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.TripGroup)
transitOperatorV2GetTripGroup a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2GetTripGroup a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2UpdateTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateTripGroupReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.TripGroup)
transitOperatorV2UpdateTripGroup a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_TRIP_GROUP" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2UpdateTripGroup a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteTripGroup a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2DeleteTripGroup a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ListTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Lib.BmsBackend.Transit.V2.Types.Trip])
transitOperatorV2ListTrips a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ListTrips a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ReplaceTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.ReplaceTripsReq -> Environment.FlowHandler [Lib.BmsBackend.Transit.V2.Types.Trip])
transitOperatorV2ReplaceTrips a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_REPLACE_TRIPS" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ReplaceTrips a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2ListDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyRepeatViewList)
transitOperatorV2ListDutyRepeats a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a10 a12 a11
        Tools.ActorInfo.withDashboardUserActorInfo a10 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ListDutyRepeats a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a10)) a1
    )

transitOperatorV2CreateDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateDutyRepeatReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp)
transitOperatorV2CreateDutyRepeat a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_CREATE_DUTY_REPEAT" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2CreateDutyRepeat a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2PreviewDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq -> Environment.FlowHandler [Lib.BmsBackend.Transit.V2.Types.GenerateEntry])
transitOperatorV2PreviewDutyRepeats a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2PreviewDutyRepeats a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2GenerateDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq -> Environment.FlowHandler [Lib.BmsBackend.Transit.V2.Types.GenerateEntry])
transitOperatorV2GenerateDutyRepeats a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2GenerateDutyRepeats a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2UpdateDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateDutyRepeatReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp)
transitOperatorV2UpdateDutyRepeat a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_REPEAT" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2UpdateDutyRepeat a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDutyRepeat a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2DeleteDutyRepeat a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ListDutyGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyGroupListItemList)
transitOperatorV2ListDutyGroups a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a13 a15 a14
        Tools.ActorInfo.withDashboardUserActorInfo a13 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ListDutyGroups a15 a14 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a13)) a1
    )

transitOperatorV2CreateDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateDutyGroupReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail)
transitOperatorV2CreateDutyGroup a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2CreateDutyGroup a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2GetDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail)
transitOperatorV2GetDutyGroup a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2GetDutyGroup a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2DeleteDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDutyGroup a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2DeleteDutyGroup a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ListDuties :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Lib.BmsBackend.Transit.V2.Types.Duty])
transitOperatorV2ListDuties a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ListDuties a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2GetDutyGroupLogs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Lib.BmsBackend.Transit.V2.Types.DutyEventLog])
transitOperatorV2GetDutyGroupLogs a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2GetDutyGroupLogs a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateVehicleReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.VehicleChange)
transitOperatorV2UpdateDutyGroupVehicle a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2UpdateDutyGroupVehicle a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail)
transitOperatorV2UpdateDutyGroupCrew a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2UpdateDutyGroupCrew a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2SetDutyGroupActive :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.SetActiveReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyGroup)
transitOperatorV2SetDutyGroupActive a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2SetDutyGroupActive a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2UpdateDutyCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.Duty)
transitOperatorV2UpdateDutyCrew a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2UpdateDutyCrew a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2ListGenerationFailures :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyEventLogList)
transitOperatorV2ListGenerationFailures a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a5 a7 a6
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ListGenerationFailures a7 a6 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a1
    )

transitOperatorV2ResolveGenerationFailure :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.V2.Types.DutyEventLog)
transitOperatorV2ResolveGenerationFailure a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_SCHEDULES/TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Schedules.transitOperatorV2ResolveGenerationFailure a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )
