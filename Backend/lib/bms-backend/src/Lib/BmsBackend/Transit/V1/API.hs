module Lib.BmsBackend.Transit.V1.API where

import Data.Time (Day)
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.BmsBackend.Transit.Common
import Lib.BmsBackend.Transit.V1.Types
import Servant

type ListSchedulesAPI =
  Transit ("schedules" :> QueryParam "search" Text :> QueryParam "depotId" Text :> QueryParam "routeId" Text :> QueryParam "status" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse Schedule)))

listSchedulesAPI :: Proxy ListSchedulesAPI
listSchedulesAPI = Proxy

listSchedules :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse Schedule)
listSchedules = ET.client listSchedulesAPI

type GetScheduleAPI = Transit ("schedules" :> Capture "id" Text :> Scoped (Get '[JSON] Schedule))

getScheduleAPI :: Proxy GetScheduleAPI
getScheduleAPI = Proxy

getSchedule :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient Schedule
getSchedule = ET.client getScheduleAPI

type CreateScheduleAPI = Transit ("schedules" :> ReqBody '[JSON] CreateScheduleReq :> Scoped (Post '[JSON] Schedule))

createScheduleAPI :: Proxy CreateScheduleAPI
createScheduleAPI = Proxy

createSchedule :: Text -> CreateScheduleReq -> Maybe Text -> Maybe Text -> ET.EulerClient Schedule
createSchedule = ET.client createScheduleAPI

type UpdateScheduleAPI = Transit ("schedules" :> Capture "id" Text :> ReqBody '[JSON] UpdateScheduleReq :> Scoped (Patch '[JSON] Schedule))

updateScheduleAPI :: Proxy UpdateScheduleAPI
updateScheduleAPI = Proxy

updateSchedule :: Text -> Text -> UpdateScheduleReq -> Maybe Text -> Maybe Text -> ET.EulerClient Schedule
updateSchedule = ET.client updateScheduleAPI

type DeleteScheduleAPI = Transit ("schedules" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteScheduleAPI :: Proxy DeleteScheduleAPI
deleteScheduleAPI = Proxy

deleteSchedule :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteSchedule = ET.client deleteScheduleAPI

type ListScheduleTypesAPI = Transit ("schedule-types" :> Scoped (Get '[JSON] (ItemsResponse ScheduleType)))

listScheduleTypesAPI :: Proxy ListScheduleTypesAPI
listScheduleTypesAPI = Proxy

listScheduleTypes :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient (ItemsResponse ScheduleType)
listScheduleTypes = ET.client listScheduleTypesAPI

type ListScheduleTripsAPI =
  Transit ("schedule-trips" :> QueryParam "scheduleId" Text :> QueryParam "depotId" Text :> QueryParam "search" Text :> QueryParam "repeatStatus" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse ScheduleTrip)))

listScheduleTripsAPI :: Proxy ListScheduleTripsAPI
listScheduleTripsAPI = Proxy

listScheduleTrips :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse ScheduleTrip)
listScheduleTrips = ET.client listScheduleTripsAPI

type GetScheduleTripAPI = Transit ("schedule-trips" :> Capture "id" Text :> Scoped (Get '[JSON] ScheduleTrip))

getScheduleTripAPI :: Proxy GetScheduleTripAPI
getScheduleTripAPI = Proxy

getScheduleTrip :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient ScheduleTrip
getScheduleTrip = ET.client getScheduleTripAPI

type CreateScheduleTripAPI = Transit ("schedule-trips" :> ReqBody '[JSON] CreateScheduleTripReq :> Scoped (Post '[JSON] ScheduleTrip))

createScheduleTripAPI :: Proxy CreateScheduleTripAPI
createScheduleTripAPI = Proxy

createScheduleTrip :: Text -> CreateScheduleTripReq -> Maybe Text -> Maybe Text -> ET.EulerClient ScheduleTrip
createScheduleTrip = ET.client createScheduleTripAPI

type UpdateScheduleTripAPI = Transit ("schedule-trips" :> Capture "id" Text :> ReqBody '[JSON] UpdateScheduleTripReq :> Scoped (Patch '[JSON] ScheduleTrip))

updateScheduleTripAPI :: Proxy UpdateScheduleTripAPI
updateScheduleTripAPI = Proxy

updateScheduleTrip :: Text -> Text -> UpdateScheduleTripReq -> Maybe Text -> Maybe Text -> ET.EulerClient ScheduleTrip
updateScheduleTrip = ET.client updateScheduleTripAPI

type DeleteScheduleTripAPI = Transit ("schedule-trips" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteScheduleTripAPI :: Proxy DeleteScheduleTripAPI
deleteScheduleTripAPI = Proxy

deleteScheduleTrip :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteScheduleTrip = ET.client deleteScheduleTripAPI

type GetRepeatConfigAPI = Transit ("schedule-trips" :> Capture "id" Text :> "repeat" :> Scoped (Get '[JSON] RepeatConfig))

getRepeatConfigAPI :: Proxy GetRepeatConfigAPI
getRepeatConfigAPI = Proxy

getRepeatConfig :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient RepeatConfig
getRepeatConfig = ET.client getRepeatConfigAPI

type SetRepeatConfigAPI = Transit ("schedule-trips" :> Capture "id" Text :> "repeat" :> ReqBody '[JSON] SetRepeatConfigReq :> Scoped (Put '[JSON] RepeatConfig))

setRepeatConfigAPI :: Proxy SetRepeatConfigAPI
setRepeatConfigAPI = Proxy

setRepeatConfig :: Text -> Text -> SetRepeatConfigReq -> Maybe Text -> Maybe Text -> ET.EulerClient RepeatConfig
setRepeatConfig = ET.client setRepeatConfigAPI

type ListTripDetailsAPI = Transit ("schedule-trips" :> Capture "id" Text :> "details" :> Scoped (Get '[JSON] (ItemsResponse TripDetail)))

listTripDetailsAPI :: Proxy ListTripDetailsAPI
listTripDetailsAPI = Proxy

listTripDetails :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient (ItemsResponse TripDetail)
listTripDetails = ET.client listTripDetailsAPI

type ReplaceTripDetailsAPI = Transit ("schedule-trips" :> Capture "id" Text :> "details" :> ReqBody '[JSON] ReplaceTripDetailsReq :> Scoped (Put '[JSON] (ItemsResponse TripDetail)))

replaceTripDetailsAPI :: Proxy ReplaceTripDetailsAPI
replaceTripDetailsAPI = Proxy

replaceTripDetails :: Text -> Text -> ReplaceTripDetailsReq -> Maybe Text -> Maybe Text -> ET.EulerClient (ItemsResponse TripDetail)
replaceTripDetails = ET.client replaceTripDetailsAPI

type UpdateTripDetailAPI = Transit ("trip-details" :> Capture "id" Text :> ReqBody '[JSON] UpdateTripDetailReq :> Scoped (Patch '[JSON] TripDetail))

updateTripDetailAPI :: Proxy UpdateTripDetailAPI
updateTripDetailAPI = Proxy

updateTripDetail :: Text -> Text -> UpdateTripDetailReq -> Maybe Text -> Maybe Text -> ET.EulerClient TripDetail
updateTripDetail = ET.client updateTripDetailAPI

type DeleteTripDetailAPI = Transit ("trip-details" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteTripDetailAPI :: Proxy DeleteTripDetailAPI
deleteTripDetailAPI = Proxy

deleteTripDetail :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteTripDetail = ET.client deleteTripDetailAPI

type ListWaybillsAPI =
  Transit ("waybills" :> QueryParam "vehicleNo" Text :> QueryParam "dutyDate" Day :> QueryParam "dutyDateFrom" Day :> QueryParam "dutyDateTo" Day :> QueryParam "status" Text :> QueryParam "waybillNo" Text :> QueryParam "driverToken" Text :> QueryParam "conductorToken" Text :> QueryParam "depotId" Text :> QueryParam "scheduleNo" Text :> QueryParam "scheduleTripId" Text :> QueryParam "search" Text :> QueryParam "sort" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse Waybill)))

listWaybillsAPI :: Proxy ListWaybillsAPI
listWaybillsAPI = Proxy

listWaybills :: Text -> Maybe Text -> Maybe Day -> Maybe Day -> Maybe Day -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse Waybill)
listWaybills = ET.client listWaybillsAPI

type GetWaybillAPI = Transit ("waybills" :> Capture "id" Text :> Scoped (Get '[JSON] Waybill))

getWaybillAPI :: Proxy GetWaybillAPI
getWaybillAPI = Proxy

getWaybill :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient Waybill
getWaybill = ET.client getWaybillAPI

type CreateWaybillAPI = Transit ("waybills" :> ReqBody '[JSON] CreateWaybillReq :> Scoped (Post '[JSON] Waybill))

createWaybillAPI :: Proxy CreateWaybillAPI
createWaybillAPI = Proxy

createWaybill :: Text -> CreateWaybillReq -> Maybe Text -> Maybe Text -> ET.EulerClient Waybill
createWaybill = ET.client createWaybillAPI

type UpdateWaybillAPI = Transit ("waybills" :> Capture "id" Text :> ReqBody '[JSON] UpdateWaybillReq :> Scoped (Patch '[JSON] Waybill))

updateWaybillAPI :: Proxy UpdateWaybillAPI
updateWaybillAPI = Proxy

updateWaybill :: Text -> Text -> UpdateWaybillReq -> Maybe Text -> Maybe Text -> ET.EulerClient Waybill
updateWaybill = ET.client updateWaybillAPI

type DeleteWaybillAPI = Transit ("waybills" :> Capture "id" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteWaybillAPI :: Proxy DeleteWaybillAPI
deleteWaybillAPI = Proxy

deleteWaybill :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteWaybill = ET.client deleteWaybillAPI

type PreviewWaybillRepeatsAPI = Transit ("waybill-repeats" :> "preview" :> ReqBody '[JSON] RepeatRangeReq :> Scoped (Post '[JSON] RepeatWalkResponse))

previewWaybillRepeatsAPI :: Proxy PreviewWaybillRepeatsAPI
previewWaybillRepeatsAPI = Proxy

previewWaybillRepeats :: Text -> RepeatRangeReq -> Maybe Text -> Maybe Text -> ET.EulerClient RepeatWalkResponse
previewWaybillRepeats = ET.client previewWaybillRepeatsAPI

type GenerateWaybillRepeatsAPI = Transit ("waybill-repeats" :> "generate" :> ReqBody '[JSON] RepeatRangeReq :> Scoped (Post '[JSON] RepeatWalkResponse))

generateWaybillRepeatsAPI :: Proxy GenerateWaybillRepeatsAPI
generateWaybillRepeatsAPI = Proxy

generateWaybillRepeats :: Text -> RepeatRangeReq -> Maybe Text -> Maybe Text -> ET.EulerClient RepeatWalkResponse
generateWaybillRepeats = ET.client generateWaybillRepeatsAPI
