{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TransitOperatorV2Schedules where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.V2.Types
import Servant
import Servant.Client

type API = ("transitOperatorV2" :> (TransitOperatorV2ListTripGroupsHelper :<|> TransitOperatorV2CreateTripGroupHelper :<|> TransitOperatorV2GetTripGroupHelper :<|> TransitOperatorV2UpdateTripGroupHelper :<|> TransitOperatorV2DeleteTripGroupHelper :<|> TransitOperatorV2ListTripsHelper :<|> TransitOperatorV2ReplaceTripsHelper :<|> TransitOperatorV2ListDutyRepeatsHelper :<|> TransitOperatorV2CreateDutyRepeatHelper :<|> TransitOperatorV2PreviewDutyRepeatsHelper :<|> TransitOperatorV2GenerateDutyRepeatsHelper :<|> TransitOperatorV2UpdateDutyRepeatHelper :<|> TransitOperatorV2DeleteDutyRepeatHelper :<|> TransitOperatorV2ListDutyGroupsHelper :<|> TransitOperatorV2CreateDutyGroupHelper :<|> TransitOperatorV2GetDutyGroupHelper :<|> TransitOperatorV2DeleteDutyGroupHelper :<|> TransitOperatorV2ListDutiesHelper :<|> TransitOperatorV2GetDutyGroupLogsHelper :<|> TransitOperatorV2UpdateDutyGroupVehicleHelper :<|> TransitOperatorV2UpdateDutyGroupCrewHelper :<|> TransitOperatorV2SetDutyGroupActiveHelper :<|> TransitOperatorV2UpdateDutyCrewHelper :<|> TransitOperatorV2ListGenerationFailuresHelper :<|> TransitOperatorV2ResolveGenerationFailureHelper))

type TransitOperatorV2ListTripGroups =
  ( "tripGroups" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "zone" Kernel.Prelude.Text
      :> QueryParam
           "shift"
           Kernel.Prelude.Text
      :> QueryParam "tripType" Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam
           "code"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "crew"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicle"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.TripGroupList
  )

type TransitOperatorV2ListTripGroupsHelper =
  ( "tripGroups" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "zone"
           Kernel.Prelude.Text
      :> QueryParam "shift" Kernel.Prelude.Text
      :> QueryParam "tripType" Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam
           "code"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "crew"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicle"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.TripGroupList
  )

type TransitOperatorV2CreateTripGroup =
  ( "tripGroups" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.CreateTripGroupReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V2.Types.TripGroup
  )

type TransitOperatorV2CreateTripGroupHelper =
  ( "tripGroups" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.CreateTripGroupReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.TripGroup
  )

type TransitOperatorV2GetTripGroup =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.TripGroup
  )

type TransitOperatorV2GetTripGroupHelper =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V2.Types.TripGroup
  )

type TransitOperatorV2UpdateTripGroup =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpdateTripGroupReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.V2.Types.TripGroup
  )

type TransitOperatorV2UpdateTripGroupHelper =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpdateTripGroupReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.TripGroup
  )

type TransitOperatorV2DeleteTripGroup =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteTripGroupHelper =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2ListTrips =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [Lib.BmsBackend.Transit.V2.Types.Trip]
  )

type TransitOperatorV2ListTripsHelper =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.Trip]
  )

type TransitOperatorV2ReplaceTrips =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.ReplaceTripsReq
      :> Put ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.Trip]
  )

type TransitOperatorV2ReplaceTripsHelper =
  ( "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.ReplaceTripsReq
      :> Put
           ('[JSON])
           [Lib.BmsBackend.Transit.V2.Types.Trip]
  )

type TransitOperatorV2ListDutyRepeats =
  ( "dutyRepeats" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam "code" Kernel.Prelude.Text
      :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam
           "vehicle"
           Kernel.Prelude.Text
      :> QueryParam
           "crew"
           Kernel.Prelude.Text
      :> QueryParam
           "repeatStatus"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyRepeatViewList
  )

type TransitOperatorV2ListDutyRepeatsHelper =
  ( "dutyRepeats" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam "code" Kernel.Prelude.Text
      :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam
           "vehicle"
           Kernel.Prelude.Text
      :> QueryParam
           "crew"
           Kernel.Prelude.Text
      :> QueryParam
           "repeatStatus"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyRepeatViewList
  )

type TransitOperatorV2CreateDutyRepeat =
  ( "dutyRepeats" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.CreateDutyRepeatReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp
  )

type TransitOperatorV2CreateDutyRepeatHelper =
  ( "dutyRepeats" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.CreateDutyRepeatReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp
  )

type TransitOperatorV2PreviewDutyRepeats =
  ( "dutyRepeats" :> "preview" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq
      :> Post ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.GenerateEntry]
  )

type TransitOperatorV2PreviewDutyRepeatsHelper =
  ( "dutyRepeats" :> "preview" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq
      :> Post
           ('[JSON])
           [Lib.BmsBackend.Transit.V2.Types.GenerateEntry]
  )

type TransitOperatorV2GenerateDutyRepeats =
  ( "dutyRepeats" :> "generate" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq
      :> Post ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.GenerateEntry]
  )

type TransitOperatorV2GenerateDutyRepeatsHelper =
  ( "dutyRepeats" :> "generate" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq
      :> Post
           ('[JSON])
           [Lib.BmsBackend.Transit.V2.Types.GenerateEntry]
  )

type TransitOperatorV2UpdateDutyRepeat =
  ( "dutyRepeats" :> Capture "dutyRepeatId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.UpdateDutyRepeatReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp
  )

type TransitOperatorV2UpdateDutyRepeatHelper =
  ( "dutyRepeats" :> Capture "dutyRepeatId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpdateDutyRepeatReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp
  )

type TransitOperatorV2DeleteDutyRepeat =
  ( "dutyRepeats" :> Capture "dutyRepeatId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteDutyRepeatHelper =
  ( "dutyRepeats" :> Capture "dutyRepeatId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2ListDutyGroups =
  ( "dutyGroups" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "date" Data.Time.Day
      :> QueryParam
           "from"
           Data.Time.Day
      :> QueryParam "to" Data.Time.Day
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicle"
           Kernel.Prelude.Text
      :> QueryParam
           "crew"
           Kernel.Prelude.Text
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyGroupListItemList
  )

type TransitOperatorV2ListDutyGroupsHelper =
  ( "dutyGroups" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "date" Data.Time.Day
      :> QueryParam
           "from"
           Data.Time.Day
      :> QueryParam "to" Data.Time.Day
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicle"
           Kernel.Prelude.Text
      :> QueryParam
           "crew"
           Kernel.Prelude.Text
      :> QueryParam
           "status"
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
           Lib.BmsBackend.Transit.V2.Types.DutyGroupListItemList
  )

type TransitOperatorV2CreateDutyGroup =
  ( "dutyGroups" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.CreateDutyGroupReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail
  )

type TransitOperatorV2CreateDutyGroupHelper =
  ( "dutyGroups" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.CreateDutyGroupReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail
  )

type TransitOperatorV2GetDutyGroup =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail
  )

type TransitOperatorV2GetDutyGroupHelper =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail
  )

type TransitOperatorV2DeleteDutyGroup =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteDutyGroupHelper =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2ListDuties =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "duties"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.Duty]
  )

type TransitOperatorV2ListDutiesHelper =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "duties" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.Duty]
  )

type TransitOperatorV2GetDutyGroupLogs =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "logs"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.DutyEventLog]
  )

type TransitOperatorV2GetDutyGroupLogsHelper =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "logs"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.V2.Types.DutyEventLog]
  )

type TransitOperatorV2UpdateDutyGroupVehicle =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "vehicle"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.UpdateVehicleReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.VehicleChange
  )

type TransitOperatorV2UpdateDutyGroupVehicleHelper =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "vehicle"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpdateVehicleReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.VehicleChange
  )

type TransitOperatorV2UpdateDutyGroupCrew =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "crew"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail
  )

type TransitOperatorV2UpdateDutyGroupCrewHelper =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "crew"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail
  )

type TransitOperatorV2SetDutyGroupActive =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "active"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.SetActiveReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyGroup
  )

type TransitOperatorV2SetDutyGroupActiveHelper =
  ( "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "active"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.SetActiveReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyGroup
  )

type TransitOperatorV2UpdateDutyCrew =
  ( "duties" :> Capture "dutyId" Kernel.Prelude.Text :> "crew" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.V2.Types.Duty
  )

type TransitOperatorV2UpdateDutyCrewHelper =
  ( "duties" :> Capture "dutyId" Kernel.Prelude.Text :> "crew" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.Duty
  )

type TransitOperatorV2ListGenerationFailures =
  ( "generationFailures" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "resolved"
           Kernel.Prelude.Bool
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyEventLogList
  )

type TransitOperatorV2ListGenerationFailuresHelper =
  ( "generationFailures" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "resolved"
           Kernel.Prelude.Bool
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyEventLogList
  )

type TransitOperatorV2ResolveGenerationFailure =
  ( "generationFailures" :> Capture "failureId" Kernel.Prelude.Text :> "resolve"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post ('[JSON]) Lib.BmsBackend.Transit.V2.Types.DutyEventLog
  )

type TransitOperatorV2ResolveGenerationFailureHelper =
  ( "generationFailures" :> Capture "failureId" Kernel.Prelude.Text :> "resolve"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.V2.Types.DutyEventLog
  )

data TransitOperatorV2SchedulesAPIs = TransitOperatorV2SchedulesAPIs
  { transitOperatorV2ListTripGroups :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.TripGroupList),
    transitOperatorV2CreateTripGroup :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateTripGroupReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.TripGroup),
    transitOperatorV2GetTripGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.TripGroup),
    transitOperatorV2UpdateTripGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateTripGroupReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.TripGroup),
    transitOperatorV2DeleteTripGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2ListTrips :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.V2.Types.Trip]),
    transitOperatorV2ReplaceTrips :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.ReplaceTripsReq -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.V2.Types.Trip]),
    transitOperatorV2ListDutyRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyRepeatViewList),
    transitOperatorV2CreateDutyRepeat :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateDutyRepeatReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp),
    transitOperatorV2PreviewDutyRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.V2.Types.GenerateEntry]),
    transitOperatorV2GenerateDutyRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.GenerateDutiesReq -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.V2.Types.GenerateEntry]),
    transitOperatorV2UpdateDutyRepeat :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateDutyRepeatReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.UpsertDutyRepeatResp),
    transitOperatorV2DeleteDutyRepeat :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2ListDutyGroups :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyGroupListItemList),
    transitOperatorV2CreateDutyGroup :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.CreateDutyGroupReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail),
    transitOperatorV2GetDutyGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail),
    transitOperatorV2DeleteDutyGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2ListDuties :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.V2.Types.Duty]),
    transitOperatorV2GetDutyGroupLogs :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.V2.Types.DutyEventLog]),
    transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateVehicleReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.VehicleChange),
    transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyGroupDetail),
    transitOperatorV2SetDutyGroupActive :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.SetActiveReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyGroup),
    transitOperatorV2UpdateDutyCrew :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.V2.Types.UpdateCrewReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.Duty),
    transitOperatorV2ListGenerationFailures :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyEventLogList),
    transitOperatorV2ResolveGenerationFailure :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.V2.Types.DutyEventLog)
  }

mkTransitOperatorV2SchedulesAPIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorV2SchedulesAPIs)
mkTransitOperatorV2SchedulesAPIs transitOperatorV2SchedulesClient = (TransitOperatorV2SchedulesAPIs {..})
  where
    transitOperatorV2ListTripGroups :<|> transitOperatorV2CreateTripGroup :<|> transitOperatorV2GetTripGroup :<|> transitOperatorV2UpdateTripGroup :<|> transitOperatorV2DeleteTripGroup :<|> transitOperatorV2ListTrips :<|> transitOperatorV2ReplaceTrips :<|> transitOperatorV2ListDutyRepeats :<|> transitOperatorV2CreateDutyRepeat :<|> transitOperatorV2PreviewDutyRepeats :<|> transitOperatorV2GenerateDutyRepeats :<|> transitOperatorV2UpdateDutyRepeat :<|> transitOperatorV2DeleteDutyRepeat :<|> transitOperatorV2ListDutyGroups :<|> transitOperatorV2CreateDutyGroup :<|> transitOperatorV2GetDutyGroup :<|> transitOperatorV2DeleteDutyGroup :<|> transitOperatorV2ListDuties :<|> transitOperatorV2GetDutyGroupLogs :<|> transitOperatorV2UpdateDutyGroupVehicle :<|> transitOperatorV2UpdateDutyGroupCrew :<|> transitOperatorV2SetDutyGroupActive :<|> transitOperatorV2UpdateDutyCrew :<|> transitOperatorV2ListGenerationFailures :<|> transitOperatorV2ResolveGenerationFailure = transitOperatorV2SchedulesClient

data TransitOperatorV2SchedulesUserActionType
  = TRANSIT_OPERATOR_V2_LIST_TRIP_GROUPS
  | TRANSIT_OPERATOR_V2_CREATE_TRIP_GROUP
  | TRANSIT_OPERATOR_V2_GET_TRIP_GROUP
  | TRANSIT_OPERATOR_V2_UPDATE_TRIP_GROUP
  | TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP
  | TRANSIT_OPERATOR_V2_LIST_TRIPS
  | TRANSIT_OPERATOR_V2_REPLACE_TRIPS
  | TRANSIT_OPERATOR_V2_LIST_DUTY_REPEATS
  | TRANSIT_OPERATOR_V2_CREATE_DUTY_REPEAT
  | TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS
  | TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS
  | TRANSIT_OPERATOR_V2_UPDATE_DUTY_REPEAT
  | TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT
  | TRANSIT_OPERATOR_V2_LIST_DUTY_GROUPS
  | TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP
  | TRANSIT_OPERATOR_V2_GET_DUTY_GROUP
  | TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP
  | TRANSIT_OPERATOR_V2_LIST_DUTIES
  | TRANSIT_OPERATOR_V2_GET_DUTY_GROUP_LOGS
  | TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE
  | TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW
  | TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE
  | TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW
  | TRANSIT_OPERATOR_V2_LIST_GENERATION_FAILURES
  | TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorV2SchedulesUserActionType)])
