{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.FRFSTicketService
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.FRFSTicketService
import qualified "this" API.Types.UI.FRFSTicketService
import qualified "this" API.Types.UI.MultimodalConfirm
import qualified BecknV2.FRFS.Enums
import qualified Domain.Action.Dashboard.AppManagement.FRFSTicketService
import qualified "this" Domain.Types.FRFSQuote
import qualified "this" Domain.Types.FRFSSearch
import qualified "this" Domain.Types.FRFSTicketBooking
import qualified "this" Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("fRFSTicketService" :> (GetFRFSTicketServiceCustomerFrfsConfig :<|> GetFRFSTicketServiceCustomerFrfsAutocomplete :<|> GetFRFSTicketServiceCustomerFrfsRoutes :<|> GetFRFSTicketServiceCustomerFrfsStations :<|> PostFRFSTicketServiceCustomerFrfsStationsPossibleStops :<|> GetFRFSTicketServiceCustomerFrfsRoute :<|> PostFRFSTicketServiceCustomerFrfsSearch :<|> GetFRFSTicketServiceCustomerFrfsSearchQuote :<|> PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm :<|> GetFRFSTicketServiceCustomerFrfsBookingStatus :<|> GetFRFSTicketServiceCustomerFrfsBookingPaymentAttempts :<|> GetFRFSTicketServiceCustomerFrfsPaymentAttempts :<|> GetFRFSTicketServiceCustomerFrfsRouteSeatLayout :<|> GetFRFSTicketServiceCustomerFrfsTripRouteSeats :<|> PostFRFSTicketServiceCustomerFrfsRouteServiceability :<|> PostFRFSTicketServiceCustomerFrfsFleetOperatorTripAction :<|> PostFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation))

type GetFRFSTicketServiceCustomerFrfsConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_CONFIG"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsConfig
  )

type GetFRFSTicketServiceCustomerFrfsAutocomplete =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_AUTOCOMPLETE"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsAutocomplete
  )

type GetFRFSTicketServiceCustomerFrfsRoutes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTES"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsRoutes
  )

type GetFRFSTicketServiceCustomerFrfsStations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsStations
  )

type PostFRFSTicketServiceCustomerFrfsStationsPossibleStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS_POSSIBLE_STOPS"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsStationsPossibleStops
  )

type GetFRFSTicketServiceCustomerFrfsRoute =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsRoute
  )

type PostFRFSTicketServiceCustomerFrfsSearch =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsSearch
  )

type GetFRFSTicketServiceCustomerFrfsSearchQuote =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH_QUOTE"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsSearchQuote
  )

type PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_QUOTE_V2_CONFIRM"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm
  )

type GetFRFSTicketServiceCustomerFrfsBookingStatus =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_STATUS"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsBookingStatus
  )

type GetFRFSTicketServiceCustomerFrfsBookingPaymentAttempts =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_PAYMENT_ATTEMPTS"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsBookingPaymentAttempts
  )

type GetFRFSTicketServiceCustomerFrfsPaymentAttempts =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_PAYMENT_ATTEMPTS"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsPaymentAttempts
  )

type GetFRFSTicketServiceCustomerFrfsRouteSeatLayout =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SEAT_LAYOUT"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsRouteSeatLayout
  )

type GetFRFSTicketServiceCustomerFrfsTripRouteSeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_TRIP_ROUTE_SEATS"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsTripRouteSeats
  )

type PostFRFSTicketServiceCustomerFrfsRouteServiceability =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SERVICEABILITY"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsRouteServiceability
  )

type PostFRFSTicketServiceCustomerFrfsFleetOperatorTripAction =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_FLEET_OPERATOR_TRIP_ACTION"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsFleetOperatorTripAction
  )

type PostFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_FLEET_OPERATOR_CURRENT_OPERATION"
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFRFSTicketServiceCustomerFrfsConfig merchantId city :<|> getFRFSTicketServiceCustomerFrfsAutocomplete merchantId city :<|> getFRFSTicketServiceCustomerFrfsRoutes merchantId city :<|> getFRFSTicketServiceCustomerFrfsStations merchantId city :<|> postFRFSTicketServiceCustomerFrfsStationsPossibleStops merchantId city :<|> getFRFSTicketServiceCustomerFrfsRoute merchantId city :<|> postFRFSTicketServiceCustomerFrfsSearch merchantId city :<|> getFRFSTicketServiceCustomerFrfsSearchQuote merchantId city :<|> postFRFSTicketServiceCustomerFrfsQuoteV2Confirm merchantId city :<|> getFRFSTicketServiceCustomerFrfsBookingStatus merchantId city :<|> getFRFSTicketServiceCustomerFrfsBookingPaymentAttempts merchantId city :<|> getFRFSTicketServiceCustomerFrfsPaymentAttempts merchantId city :<|> getFRFSTicketServiceCustomerFrfsRouteSeatLayout merchantId city :<|> getFRFSTicketServiceCustomerFrfsTripRouteSeats merchantId city :<|> postFRFSTicketServiceCustomerFrfsRouteServiceability merchantId city :<|> postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction merchantId city :<|> postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation merchantId city

getFRFSTicketServiceCustomerFrfsConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSConfigAPIRes)
getFRFSTicketServiceCustomerFrfsConfig a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsConfig a5 a4 a2 a1

getFRFSTicketServiceCustomerFrfsAutocomplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> Kernel.Types.Beckn.Context.City -> Kernel.External.Maps.Types.LatLong -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.UI.FRFSTicketService.AutocompleteRes)
getFRFSTicketServiceCustomerFrfsAutocomplete a11 a10 _a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsAutocomplete a11 a10 a8 a7 a6 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSRouteAPI])
getFRFSTicketServiceCustomerFrfsRoutes a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRoutes a8 a7 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI])
getFRFSTicketServiceCustomerFrfsStations a12 a11 _a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsStations a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1

postFRFSTicketServiceCustomerFrfsStationsPossibleStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI])
postFRFSTicketServiceCustomerFrfsStationsPossibleStops a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsStationsPossibleStops a8 a7 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSRouteAPI)
getFRFSTicketServiceCustomerFrfsRoute a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRoute a9 a8 a6 a5 a4 a3 a2 a1

postFRFSTicketServiceCustomerFrfsSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe ([BecknV2.FRFS.Enums.ServiceTierType]) -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSSearchAPIRes)
postFRFSTicketServiceCustomerFrfsSearch a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsSearch a9 a8 a6 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsSearchQuote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes])
getFRFSTicketServiceCustomerFrfsSearchQuote a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsSearchQuote a5 a4 a2 a1

postFRFSTicketServiceCustomerFrfsQuoteV2Confirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
postFRFSTicketServiceCustomerFrfsQuoteV2Confirm a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsQuoteV2Confirm a7 a6 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a1

getFRFSTicketServiceCustomerFrfsBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
getFRFSTicketServiceCustomerFrfsBookingStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsBookingStatus a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

getFRFSTicketServiceCustomerFrfsBookingPaymentAttempts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSBookingPaymentAttemptsAPIRes)
getFRFSTicketServiceCustomerFrfsBookingPaymentAttempts a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsBookingPaymentAttempts a5 a4 a2 a1

getFRFSTicketServiceCustomerFrfsPaymentAttempts :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSPaymentAttemptsListAPIRes)
getFRFSTicketServiceCustomerFrfsPaymentAttempts a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsPaymentAttempts a6 a5 a3 a2 a1

getFRFSTicketServiceCustomerFrfsRouteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.UI.FRFSTicketService.SeatLayoutDetailsResp)
getFRFSTicketServiceCustomerFrfsRouteSeatLayout a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRouteSeatLayout a6 a5 a3 a2 a1

getFRFSTicketServiceCustomerFrfsTripRouteSeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.UI.FRFSTicketService.SeatLayoutResp)
getFRFSTicketServiceCustomerFrfsTripRouteSeats a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsTripRouteSeats a9 a8 a6 a5 a4 a3 a2 a1

postFRFSTicketServiceCustomerFrfsRouteServiceability :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> API.Types.UI.FRFSTicketService.FRFSRouteServiceabilityReq -> Environment.FlowHandler API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle)
postFRFSTicketServiceCustomerFrfsRouteServiceability a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsRouteServiceability a6 a5 a3 a2 a1

postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.FRFSTicketService.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorTripActionResp)
postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction a5 a4 a2 a1

postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationResp)
postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation a5 a4 a2 a1
