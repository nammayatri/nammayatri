{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.FRFSTicketService
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.FRFSTicketService
import qualified "rider-app" API.Types.UI.FRFSTicketService
import qualified "rider-app" API.Types.UI.MultimodalConfirm
import qualified BecknV2.FRFS.Enums
import qualified Domain.Action.RiderPlatform.AppManagement.FRFSTicketService
import qualified "rider-app" Domain.Types.FRFSQuote
import qualified "rider-app" Domain.Types.FRFSSearch
import qualified "rider-app" Domain.Types.FRFSTicketBooking
import qualified "rider-app" Domain.Types.IntegratedBPPConfig
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("fRFSTicketService" :> (GetFRFSTicketServiceCustomerFrfsConfig :<|> GetFRFSTicketServiceCustomerFrfsAutocomplete :<|> GetFRFSTicketServiceCustomerFrfsRoutes :<|> GetFRFSTicketServiceCustomerFrfsStations :<|> PostFRFSTicketServiceCustomerFrfsStationsPossibleStops :<|> GetFRFSTicketServiceCustomerFrfsRoute :<|> PostFRFSTicketServiceCustomerFrfsSearch :<|> GetFRFSTicketServiceCustomerFrfsSearchQuote :<|> PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm :<|> GetFRFSTicketServiceCustomerFrfsBookingStatus :<|> GetFRFSTicketServiceCustomerFrfsRouteSeatLayout :<|> GetFRFSTicketServiceCustomerFrfsTripRouteSeats :<|> PostFRFSTicketServiceCustomerFrfsRouteServiceability :<|> PostFRFSTicketServiceCustomerFrfsFleetOperatorTripAction :<|> PostFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFRFSTicketServiceCustomerFrfsConfig merchantId city :<|> getFRFSTicketServiceCustomerFrfsAutocomplete merchantId city :<|> getFRFSTicketServiceCustomerFrfsRoutes merchantId city :<|> getFRFSTicketServiceCustomerFrfsStations merchantId city :<|> postFRFSTicketServiceCustomerFrfsStationsPossibleStops merchantId city :<|> getFRFSTicketServiceCustomerFrfsRoute merchantId city :<|> postFRFSTicketServiceCustomerFrfsSearch merchantId city :<|> getFRFSTicketServiceCustomerFrfsSearchQuote merchantId city :<|> postFRFSTicketServiceCustomerFrfsQuoteV2Confirm merchantId city :<|> getFRFSTicketServiceCustomerFrfsBookingStatus merchantId city :<|> getFRFSTicketServiceCustomerFrfsRouteSeatLayout merchantId city :<|> getFRFSTicketServiceCustomerFrfsTripRouteSeats merchantId city :<|> postFRFSTicketServiceCustomerFrfsRouteServiceability merchantId city :<|> postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction merchantId city :<|> postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation merchantId city

type GetFRFSTicketServiceCustomerFrfsConfig =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_CONFIG)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsConfig
  )

type GetFRFSTicketServiceCustomerFrfsAutocomplete =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_AUTOCOMPLETE)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsAutocomplete
  )

type GetFRFSTicketServiceCustomerFrfsRoutes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTES)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsRoutes
  )

type GetFRFSTicketServiceCustomerFrfsStations =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsStations
  )

type PostFRFSTicketServiceCustomerFrfsStationsPossibleStops =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS_POSSIBLE_STOPS)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsStationsPossibleStops
  )

type GetFRFSTicketServiceCustomerFrfsRoute =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsRoute
  )

type PostFRFSTicketServiceCustomerFrfsSearch =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsSearch
  )

type GetFRFSTicketServiceCustomerFrfsSearchQuote =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH_QUOTE)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsSearchQuote
  )

type PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_QUOTE_V2_CONFIRM)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm
  )

type GetFRFSTicketServiceCustomerFrfsBookingStatus =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_STATUS)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsBookingStatus
  )

type GetFRFSTicketServiceCustomerFrfsRouteSeatLayout =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SEAT_LAYOUT)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsRouteSeatLayout
  )

type GetFRFSTicketServiceCustomerFrfsTripRouteSeats =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_TRIP_ROUTE_SEATS)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.GetFRFSTicketServiceCustomerFrfsTripRouteSeats
  )

type PostFRFSTicketServiceCustomerFrfsRouteServiceability =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SERVICEABILITY)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsRouteServiceability
  )

type PostFRFSTicketServiceCustomerFrfsFleetOperatorTripAction =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_FLEET_OPERATOR_TRIP_ACTION)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsFleetOperatorTripAction
  )

type PostFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_TICKET_SERVICE / 'API.Types.Dashboard.AppManagement.FRFSTicketService.POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_FLEET_OPERATOR_CURRENT_OPERATION)
      :> API.Types.Dashboard.AppManagement.FRFSTicketService.PostFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation
  )

getFRFSTicketServiceCustomerFrfsConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSConfigAPIRes)
getFRFSTicketServiceCustomerFrfsConfig merchantShortId opCity apiTokenInfo customerId city = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsConfig merchantShortId opCity apiTokenInfo customerId city

getFRFSTicketServiceCustomerFrfsAutocomplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> Kernel.External.Maps.Types.LatLong -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.UI.FRFSTicketService.AutocompleteRes)
getFRFSTicketServiceCustomerFrfsAutocomplete merchantShortId opCity apiTokenInfo customerId input limit offset platformType city location vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsAutocomplete merchantShortId opCity apiTokenInfo customerId input limit offset platformType city location vehicleType

getFRFSTicketServiceCustomerFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSRouteAPI])
getFRFSTicketServiceCustomerFrfsRoutes merchantShortId opCity apiTokenInfo customerId endStationCode startStationCode city vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRoutes merchantShortId opCity apiTokenInfo customerId endStationCode startStationCode city vehicleType

getFRFSTicketServiceCustomerFrfsStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI])
getFRFSTicketServiceCustomerFrfsStations merchantShortId opCity apiTokenInfo customerId city endStationCode location minimalData platformType routeCode startStationCode vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsStations merchantShortId opCity apiTokenInfo customerId city endStationCode location minimalData platformType routeCode startStationCode vehicleType

postFRFSTicketServiceCustomerFrfsStationsPossibleStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI])
postFRFSTicketServiceCustomerFrfsStationsPossibleStops merchantShortId opCity apiTokenInfo customerId city platformType vehicleType req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsStationsPossibleStops merchantShortId opCity apiTokenInfo customerId city platformType vehicleType req

getFRFSTicketServiceCustomerFrfsRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSRouteAPI)
getFRFSTicketServiceCustomerFrfsRoute merchantShortId opCity apiTokenInfo customerId routeCode integratedBppConfigId platformType city vehicleType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRoute merchantShortId opCity apiTokenInfo customerId routeCode integratedBppConfigId platformType city vehicleType

postFRFSTicketServiceCustomerFrfsSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType] -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSSearchAPIRes)
postFRFSTicketServiceCustomerFrfsSearch merchantShortId opCity apiTokenInfo customerId city integratedBppConfigId newServiceTiers vehicleType req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsSearch merchantShortId opCity apiTokenInfo customerId city integratedBppConfigId newServiceTiers vehicleType req

getFRFSTicketServiceCustomerFrfsSearchQuote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes])
getFRFSTicketServiceCustomerFrfsSearchQuote merchantShortId opCity apiTokenInfo customerId searchId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsSearchQuote merchantShortId opCity apiTokenInfo customerId searchId

postFRFSTicketServiceCustomerFrfsQuoteV2Confirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
postFRFSTicketServiceCustomerFrfsQuoteV2Confirm merchantShortId opCity apiTokenInfo customerId quoteId isMockPayment req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsQuoteV2Confirm merchantShortId opCity apiTokenInfo customerId quoteId isMockPayment req

getFRFSTicketServiceCustomerFrfsBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
getFRFSTicketServiceCustomerFrfsBookingStatus merchantShortId opCity apiTokenInfo customerId bookingId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsBookingStatus merchantShortId opCity apiTokenInfo customerId bookingId

getFRFSTicketServiceCustomerFrfsRouteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.FRFSTicketService.SeatLayoutDetailsResp)
getFRFSTicketServiceCustomerFrfsRouteSeatLayout merchantShortId opCity apiTokenInfo customerId routeId vehicleNumber = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRouteSeatLayout merchantShortId opCity apiTokenInfo customerId routeId vehicleNumber

getFRFSTicketServiceCustomerFrfsTripRouteSeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.FRFSTicketService.SeatLayoutResp)
getFRFSTicketServiceCustomerFrfsTripRouteSeats merchantShortId opCity apiTokenInfo customerId tripId routeId fromStopCode toStopCode vehicleNumber = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsTripRouteSeats merchantShortId opCity apiTokenInfo customerId tripId routeId fromStopCode toStopCode vehicleNumber

postFRFSTicketServiceCustomerFrfsRouteServiceability :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> API.Types.UI.FRFSTicketService.FRFSRouteServiceabilityReq -> Environment.FlowHandler API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle)
postFRFSTicketServiceCustomerFrfsRouteServiceability merchantShortId opCity apiTokenInfo customerId routeId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsRouteServiceability merchantShortId opCity apiTokenInfo customerId routeId req

postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.FRFSTicketService.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorTripActionResp)
postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo customerId req

postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationResp)
postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo customerId req
