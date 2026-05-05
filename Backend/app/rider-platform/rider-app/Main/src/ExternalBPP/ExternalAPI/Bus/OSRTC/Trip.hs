module ExternalBPP.ExternalAPI.Bus.OSRTC.Trip where

import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.OSRTC.Auth
import ExternalBPP.ExternalAPI.Bus.OSRTC.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant
import Tools.Error

type OSRTCFlow m r = (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m)

-- Servant API types

type SearchTripAPI =
  "api"
    :> "ServiceTrip"
    :> "SearchTrip"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCSearchTripReq
    :> Post '[JSON] (OSRTCResponse OSRTCSearchTripRes)

type GetStationListAPI =
  "api"
    :> "List"
    :> "GetStationList"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCGetStationListReq
    :> Post '[JSON] (OSRTCResponse OSRTCGetStationListRes)

type GetSeatAvailabilityAPI =
  "api"
    :> "ServiceTrip"
    :> "GetSeatAvailability"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCSeatAvailabilityReq
    :> Post '[JSON] (OSRTCResponse OSRTCSeatAvailabilityRes)

type TicketFareCalculationAPI =
  "api"
    :> "TicketBooking"
    :> "TicketFareCalculation"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCFareCalcReq
    :> Post '[JSON] (OSRTCResponse OSRTCFareCalcResList)

type InsertTicketBookingAPI =
  "api"
    :> "TicketBooking"
    :> "InsertTicketBooking"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCInsertBookingReq
    :> Post '[JSON] (OSRTCResponse OSRTCInsertBookingResList)

type UpdateTicketBookingResponseAPI =
  "api"
    :> "TicketBooking"
    :> "UpdateTicketBookingResponse"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCUpdateBookingReq
    :> Post '[JSON] (OSRTCResponse OSRTCUpdateBookingRes)

type GetTrackingDataAPI =
  "api"
    :> "Tracking"
    :> "GetTrackingData"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCGetTrackingReq
    :> Post '[JSON] (OSRTCResponse Value)

type GetServiceListAPI =
  "api"
    :> "ServiceList"
    :> "GetServiceList"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCGetServiceListReq
    :> Post '[JSON] (OSRTCResponse Value)

type GetTripCrewListAPI =
  "api"
    :> "ServiceList"
    :> "getTripCrewList"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCGetTripCrewListReq
    :> Post '[JSON] (OSRTCResponse Value)

searchTripAPI :: Proxy SearchTripAPI
searchTripAPI = Proxy

getStationListAPI :: Proxy GetStationListAPI
getStationListAPI = Proxy

getSeatAvailabilityAPI :: Proxy GetSeatAvailabilityAPI
getSeatAvailabilityAPI = Proxy

ticketFareCalculationAPI :: Proxy TicketFareCalculationAPI
ticketFareCalculationAPI = Proxy

insertTicketBookingAPI :: Proxy InsertTicketBookingAPI
insertTicketBookingAPI = Proxy

updateTicketBookingResponseAPI :: Proxy UpdateTicketBookingResponseAPI
updateTicketBookingResponseAPI = Proxy

getTrackingDataAPI :: Proxy GetTrackingDataAPI
getTrackingDataAPI = Proxy

getServiceListAPI :: Proxy GetServiceListAPI
getServiceListAPI = Proxy

getTripCrewListAPI :: Proxy GetTripCrewListAPI
getTripCrewListAPI = Proxy

bearerAuth :: Text -> Maybe Text
bearerAuth token = Just $ "Bearer " <> token

-- API call functions

searchTrip :: OSRTCFlow m r => OSRTCConfig -> OSRTCSearchTripReq -> m (OSRTCResponse OSRTCSearchTripRes)
searchTrip config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client searchTripAPI (bearerAuth token) req) "OSRTC:SearchTrip" searchTripAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_SEARCH_TRIP_API") config.baseUrl)

getStationList :: OSRTCFlow m r => OSRTCConfig -> m (OSRTCResponse OSRTCGetStationListRes)
getStationList config = do
  token <- getAuthToken config
  let req = OSRTCGetStationListReq {intPlatformID = config.platformId}
  callAPI config.baseUrl (ET.client getStationListAPI (bearerAuth token) req) "OSRTC:GetStationList" getStationListAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_GET_STATION_LIST_API") config.baseUrl)

getSeatAvailability :: OSRTCFlow m r => OSRTCConfig -> OSRTCSeatAvailabilityReq -> m (OSRTCResponse OSRTCSeatAvailabilityRes)
getSeatAvailability config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client getSeatAvailabilityAPI (bearerAuth token) req) "OSRTC:GetSeatAvailability" getSeatAvailabilityAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_GET_SEAT_AVAILABILITY_API") config.baseUrl)

ticketFareCalculation :: OSRTCFlow m r => OSRTCConfig -> OSRTCFareCalcReq -> m (OSRTCResponse OSRTCFareCalcResList)
ticketFareCalculation config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client ticketFareCalculationAPI (bearerAuth token) req) "OSRTC:TicketFareCalculation" ticketFareCalculationAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_TICKET_FARE_CALCULATION_API") config.baseUrl)

insertTicketBooking :: OSRTCFlow m r => OSRTCConfig -> OSRTCInsertBookingReq -> m (OSRTCResponse OSRTCInsertBookingResList)
insertTicketBooking config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client insertTicketBookingAPI (bearerAuth token) req) "OSRTC:InsertTicketBooking" insertTicketBookingAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_INSERT_TICKET_BOOKING_API") config.baseUrl)

updateTicketBookingResponse :: OSRTCFlow m r => OSRTCConfig -> OSRTCUpdateBookingReq -> m (OSRTCResponse OSRTCUpdateBookingRes)
updateTicketBookingResponse config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client updateTicketBookingResponseAPI (bearerAuth token) req) "OSRTC:UpdateTicketBookingResponse" updateTicketBookingResponseAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_UPDATE_TICKET_BOOKING_RESPONSE_API") config.baseUrl)

getTrackingData :: OSRTCFlow m r => OSRTCConfig -> OSRTCGetTrackingReq -> m (OSRTCResponse Value)
getTrackingData config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client getTrackingDataAPI (bearerAuth token) req) "OSRTC:GetTrackingData" getTrackingDataAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_GET_TRACKING_DATA_API") config.baseUrl)

getServiceList :: OSRTCFlow m r => OSRTCConfig -> m (OSRTCResponse Value)
getServiceList config = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client getServiceListAPI (bearerAuth token) OSRTCGetServiceListReq) "OSRTC:GetServiceList" getServiceListAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_GET_SERVICE_LIST_API") config.baseUrl)

getTripCrewList :: OSRTCFlow m r => OSRTCConfig -> OSRTCGetTripCrewListReq -> m (OSRTCResponse Value)
getTripCrewList config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client getTripCrewListAPI (bearerAuth token) req) "OSRTC:GetTripCrewList" getTripCrewListAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_GET_TRIP_CREW_LIST_API") config.baseUrl)
