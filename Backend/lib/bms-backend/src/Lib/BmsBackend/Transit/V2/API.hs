module Lib.BmsBackend.Transit.V2.API where

import Data.Time (Day)
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.BmsBackend.Transit.Common
import Lib.BmsBackend.Transit.V2.Types
import Servant

type ListTripGroupsAPI = Transit ("trip-groups" :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "zone" Text :> QueryParam "shift" Text :> QueryParam "tripType" Text :> QueryParam "depotId" Text :> QueryParam "code" Text :> QueryParam "search" Text :> QueryParam "crew" Text :> QueryParam "vehicle" Text :> Scoped (Get '[JSON] (ListResponse TripGroup)))

listTripGroupsAPI :: Proxy ListTripGroupsAPI
listTripGroupsAPI = Proxy

listTripGroups :: Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse TripGroup)
listTripGroups = ET.client listTripGroupsAPI

type CreateTripGroupAPI = Transit ("trip-groups" :> ReqBody '[JSON] CreateTripGroupReq :> Scoped (Post '[JSON] TripGroup))

createTripGroupAPI :: Proxy CreateTripGroupAPI
createTripGroupAPI = Proxy

createTripGroup :: Text -> CreateTripGroupReq -> Maybe Text -> Maybe Text -> ET.EulerClient TripGroup
createTripGroup = ET.client createTripGroupAPI

type GetTripGroupAPI = Transit ("trip-groups" :> Capture "id" Text :> Scoped (Get '[JSON] TripGroup))

getTripGroupAPI :: Proxy GetTripGroupAPI
getTripGroupAPI = Proxy

getTripGroup :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient TripGroup
getTripGroup = ET.client getTripGroupAPI

type UpdateTripGroupAPI = Transit ("trip-groups" :> Capture "id" Text :> ReqBody '[JSON] UpdateTripGroupReq :> Scoped (Patch '[JSON] TripGroup))

updateTripGroupAPI :: Proxy UpdateTripGroupAPI
updateTripGroupAPI = Proxy

updateTripGroup :: Text -> Text -> UpdateTripGroupReq -> Maybe Text -> Maybe Text -> ET.EulerClient TripGroup
updateTripGroup = ET.client updateTripGroupAPI

type DeleteTripGroupAPI = Transit ("trip-groups" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteTripGroupAPI :: Proxy DeleteTripGroupAPI
deleteTripGroupAPI = Proxy

deleteTripGroup :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteTripGroup = ET.client deleteTripGroupAPI

type ListTripsAPI = Transit ("trip-groups" :> Capture "id" Text :> "trips" :> Scoped (Get '[JSON] [Trip]))

listTripsAPI :: Proxy ListTripsAPI
listTripsAPI = Proxy

listTrips :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient [Trip]
listTrips = ET.client listTripsAPI

type ReplaceTripsAPI = Transit ("trip-groups" :> Capture "id" Text :> "trips" :> ReqBody '[JSON] ReplaceTripsReq :> Scoped (Put '[JSON] [Trip]))

replaceTripsAPI :: Proxy ReplaceTripsAPI
replaceTripsAPI = Proxy

replaceTrips :: Text -> Text -> ReplaceTripsReq -> Maybe Text -> Maybe Text -> ET.EulerClient [Trip]
replaceTrips = ET.client replaceTripsAPI

type ListDutyRepeatsAPI = Transit ("duty-repeats" :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "tripGroupId" Text :> QueryParam "code" Text :> QueryParam "search" Text :> QueryParam "vehicle" Text :> QueryParam "crew" Text :> QueryParam "repeatStatus" Text :> Scoped (Get '[JSON] (ListResponse DutyRepeatView)))

listDutyRepeatsAPI :: Proxy ListDutyRepeatsAPI
listDutyRepeatsAPI = Proxy

listDutyRepeats :: Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse DutyRepeatView)
listDutyRepeats = ET.client listDutyRepeatsAPI

type CreateDutyRepeatAPI = Transit ("duty-repeats" :> ReqBody '[JSON] CreateDutyRepeatReq :> Scoped (Post '[JSON] UpsertDutyRepeatResp))

createDutyRepeatAPI :: Proxy CreateDutyRepeatAPI
createDutyRepeatAPI = Proxy

createDutyRepeat :: Text -> CreateDutyRepeatReq -> Maybe Text -> Maybe Text -> ET.EulerClient UpsertDutyRepeatResp
createDutyRepeat = ET.client createDutyRepeatAPI

type PreviewDutyRepeatsAPI = Transit ("duty-repeats" :> "preview" :> ReqBody '[JSON] GenerateDutiesReq :> Scoped (Post '[JSON] [GenerateEntry]))

previewDutyRepeatsAPI :: Proxy PreviewDutyRepeatsAPI
previewDutyRepeatsAPI = Proxy

previewDutyRepeats :: Text -> GenerateDutiesReq -> Maybe Text -> Maybe Text -> ET.EulerClient [GenerateEntry]
previewDutyRepeats = ET.client previewDutyRepeatsAPI

type GenerateDutyRepeatsAPI = Transit ("duty-repeats" :> "generate" :> ReqBody '[JSON] GenerateDutiesReq :> Scoped (Post '[JSON] [GenerateEntry]))

generateDutyRepeatsAPI :: Proxy GenerateDutyRepeatsAPI
generateDutyRepeatsAPI = Proxy

generateDutyRepeats :: Text -> GenerateDutiesReq -> Maybe Text -> Maybe Text -> ET.EulerClient [GenerateEntry]
generateDutyRepeats = ET.client generateDutyRepeatsAPI

type UpdateDutyRepeatAPI = Transit ("duty-repeats" :> Capture "id" Text :> ReqBody '[JSON] UpdateDutyRepeatReq :> Scoped (Patch '[JSON] UpsertDutyRepeatResp))

updateDutyRepeatAPI :: Proxy UpdateDutyRepeatAPI
updateDutyRepeatAPI = Proxy

updateDutyRepeat :: Text -> Text -> UpdateDutyRepeatReq -> Maybe Text -> Maybe Text -> ET.EulerClient UpsertDutyRepeatResp
updateDutyRepeat = ET.client updateDutyRepeatAPI

type DeleteDutyRepeatAPI = Transit ("duty-repeats" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteDutyRepeatAPI :: Proxy DeleteDutyRepeatAPI
deleteDutyRepeatAPI = Proxy

deleteDutyRepeat :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteDutyRepeat = ET.client deleteDutyRepeatAPI

type ListDutyGroupsAPI = Transit ("duty-groups" :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "date" Day :> QueryParam "from" Day :> QueryParam "to" Day :> QueryParam "tripGroupId" Text :> QueryParam "depotId" Text :> QueryParam "vehicle" Text :> QueryParam "crew" Text :> QueryParam "status" Text :> QueryParam "search" Text :> Scoped (Get '[JSON] (ListResponse DutyGroupListItem)))

listDutyGroupsAPI :: Proxy ListDutyGroupsAPI
listDutyGroupsAPI = Proxy

listDutyGroups :: Text -> Maybe Int -> Maybe Int -> Maybe Day -> Maybe Day -> Maybe Day -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse DutyGroupListItem)
listDutyGroups = ET.client listDutyGroupsAPI

type CreateDutyGroupAPI = Transit ("duty-groups" :> ReqBody '[JSON] CreateDutyGroupReq :> Scoped (Post '[JSON] DutyGroupDetail))

createDutyGroupAPI :: Proxy CreateDutyGroupAPI
createDutyGroupAPI = Proxy

createDutyGroup :: Text -> CreateDutyGroupReq -> Maybe Text -> Maybe Text -> ET.EulerClient DutyGroupDetail
createDutyGroup = ET.client createDutyGroupAPI

type GetDutyGroupAPI = Transit ("duty-groups" :> Capture "id" Text :> Scoped (Get '[JSON] DutyGroupDetail))

getDutyGroupAPI :: Proxy GetDutyGroupAPI
getDutyGroupAPI = Proxy

getDutyGroup :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient DutyGroupDetail
getDutyGroup = ET.client getDutyGroupAPI

type DeleteDutyGroupAPI = Transit ("duty-groups" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteDutyGroupAPI :: Proxy DeleteDutyGroupAPI
deleteDutyGroupAPI = Proxy

deleteDutyGroup :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteDutyGroup = ET.client deleteDutyGroupAPI

type ListDutiesAPI = Transit ("duty-groups" :> Capture "id" Text :> "duties" :> Scoped (Get '[JSON] [Duty]))

listDutiesAPI :: Proxy ListDutiesAPI
listDutiesAPI = Proxy

listDuties :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient [Duty]
listDuties = ET.client listDutiesAPI

type GetDutyGroupLogsAPI = Transit ("duty-groups" :> Capture "id" Text :> "logs" :> Scoped (Get '[JSON] [DutyEventLog]))

getDutyGroupLogsAPI :: Proxy GetDutyGroupLogsAPI
getDutyGroupLogsAPI = Proxy

getDutyGroupLogs :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient [DutyEventLog]
getDutyGroupLogs = ET.client getDutyGroupLogsAPI

type UpdateDutyGroupVehicleAPI = Transit ("duty-groups" :> Capture "id" Text :> "vehicle" :> ReqBody '[JSON] UpdateVehicleReq :> Scoped (Put '[JSON] VehicleChange))

updateDutyGroupVehicleAPI :: Proxy UpdateDutyGroupVehicleAPI
updateDutyGroupVehicleAPI = Proxy

updateDutyGroupVehicle :: Text -> Text -> UpdateVehicleReq -> Maybe Text -> Maybe Text -> ET.EulerClient VehicleChange
updateDutyGroupVehicle = ET.client updateDutyGroupVehicleAPI

type UpdateDutyGroupCrewAPI = Transit ("duty-groups" :> Capture "id" Text :> "crew" :> ReqBody '[JSON] UpdateCrewReq :> Scoped (Put '[JSON] DutyGroupDetail))

updateDutyGroupCrewAPI :: Proxy UpdateDutyGroupCrewAPI
updateDutyGroupCrewAPI = Proxy

updateDutyGroupCrew :: Text -> Text -> UpdateCrewReq -> Maybe Text -> Maybe Text -> ET.EulerClient DutyGroupDetail
updateDutyGroupCrew = ET.client updateDutyGroupCrewAPI

type SetDutyGroupActiveAPI = Transit ("duty-groups" :> Capture "id" Text :> "active" :> ReqBody '[JSON] SetActiveReq :> Scoped (Put '[JSON] DutyGroup))

setDutyGroupActiveAPI :: Proxy SetDutyGroupActiveAPI
setDutyGroupActiveAPI = Proxy

setDutyGroupActive :: Text -> Text -> SetActiveReq -> Maybe Text -> Maybe Text -> ET.EulerClient DutyGroup
setDutyGroupActive = ET.client setDutyGroupActiveAPI

type UpdateDutyCrewAPI = Transit ("duties" :> Capture "id" Text :> "crew" :> ReqBody '[JSON] UpdateCrewReq :> Scoped (Put '[JSON] Duty))

updateDutyCrewAPI :: Proxy UpdateDutyCrewAPI
updateDutyCrewAPI = Proxy

updateDutyCrew :: Text -> Text -> UpdateCrewReq -> Maybe Text -> Maybe Text -> ET.EulerClient Duty
updateDutyCrew = ET.client updateDutyCrewAPI

type ListGenerationFailuresAPI = Transit ("generation-failures" :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "resolved" Bool :> Scoped (Get '[JSON] (ListResponse DutyEventLog)))

listGenerationFailuresAPI :: Proxy ListGenerationFailuresAPI
listGenerationFailuresAPI = Proxy

listGenerationFailures :: Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse DutyEventLog)
listGenerationFailures = ET.client listGenerationFailuresAPI

type ResolveGenerationFailureAPI = Transit ("generation-failures" :> Capture "id" Text :> "resolve" :> Scoped (Post '[JSON] DutyEventLog))

resolveGenerationFailureAPI :: Proxy ResolveGenerationFailureAPI
resolveGenerationFailureAPI = Proxy

resolveGenerationFailure :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient DutyEventLog
resolveGenerationFailure = ET.client resolveGenerationFailureAPI
