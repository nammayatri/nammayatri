{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.FRFSTicketService where

import qualified "this" API.Types.UI.FRFSTicketService
import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.FRFSQuote
import qualified "this" Domain.Types.FRFSSearch
import qualified "this" Domain.Types.FRFSTicketBooking
import qualified "this" Domain.Types.IntegratedBPPConfig
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("fRFSTicketService" :> (GetFRFSTicketServiceCustomerFrfsConfig :<|> GetFRFSTicketServiceCustomerFrfsAutocomplete :<|> GetFRFSTicketServiceCustomerFrfsRoutes :<|> GetFRFSTicketServiceCustomerFrfsStations :<|> PostFRFSTicketServiceCustomerFrfsStationsPossibleStops :<|> GetFRFSTicketServiceCustomerFrfsRoute :<|> PostFRFSTicketServiceCustomerFrfsSearch :<|> GetFRFSTicketServiceCustomerFrfsSearchQuote :<|> PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm :<|> GetFRFSTicketServiceCustomerFrfsBookingStatus :<|> GetFRFSTicketServiceCustomerFrfsRouteSeatLayout))

type GetFRFSTicketServiceCustomerFrfsConfig =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "config"
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> Get ('[JSON]) API.Types.UI.FRFSTicketService.FRFSConfigAPIRes
  )

type GetFRFSTicketServiceCustomerFrfsAutocomplete =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "autocomplete"
      :> QueryParam
           "input"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "location"
           Kernel.External.Maps.Types.LatLong
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.AutocompleteRes
  )

type GetFRFSTicketServiceCustomerFrfsRoutes =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "routes"
      :> QueryParam
           "endStationCode"
           Kernel.Prelude.Text
      :> QueryParam "startStationCode" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [API.Types.UI.FRFSTicketService.FRFSRouteAPI]
  )

type GetFRFSTicketServiceCustomerFrfsStations =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "stations"
      :> QueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> QueryParam "endStationCode" Kernel.Prelude.Text
      :> QueryParam
           "location"
           Kernel.External.Maps.Types.LatLong
      :> QueryParam
           "minimalData"
           Kernel.Prelude.Bool
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> QueryParam
           "routeCode"
           Kernel.Prelude.Text
      :> QueryParam
           "startStationCode"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [API.Types.UI.FRFSTicketService.FRFSStationAPI]
  )

type PostFRFSTicketServiceCustomerFrfsStationsPossibleStops =
  ( "customer"
      :> Capture
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> "frfs"
      :> "stations"
      :> "possibleStops"
      :> QueryParam "city" Kernel.Types.Beckn.Context.City
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq
      :> Post
           ('[JSON])
           [API.Types.UI.FRFSTicketService.FRFSStationAPI]
  )

type GetFRFSTicketServiceCustomerFrfsRoute =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "route"
      :> Capture
           "routeCode"
           Kernel.Prelude.Text
      :> QueryParam
           "integratedBppConfigId"
           (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSRouteAPI
  )

type PostFRFSTicketServiceCustomerFrfsSearch =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "search"
      :> QueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> QueryParam
           "integratedBppConfigId"
           (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> QueryParam
           "newServiceTiers"
           [BecknV2.FRFS.Enums.ServiceTierType]
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSSearchAPIReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
  )

type GetFRFSTicketServiceCustomerFrfsSearchQuote =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "search"
      :> Capture
           "searchId"
           (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch)
      :> "quote"
      :> Get
           ('[JSON])
           [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
  )

type PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "quote" :> "v2"
      :> Capture
           "quoteId"
           (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote)
      :> "confirm"
      :> QueryParam
           "isMockPayment"
           Kernel.Prelude.Bool
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
  )

type GetFRFSTicketServiceCustomerFrfsBookingStatus =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "status"
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
  )

type GetFRFSTicketServiceCustomerFrfsRouteSeatLayout =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "frfs" :> "route"
      :> Capture
           "routeId"
           Kernel.Prelude.Text
      :> "seatLayout"
      :> QueryParam "fromStopIndex" Kernel.Prelude.Int
      :> QueryParam
           "toStopIndex"
           Kernel.Prelude.Int
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.SeatLayoutDetailsResp
  )

data FRFSTicketServiceAPIs = FRFSTicketServiceAPIs
  { getFRFSTicketServiceCustomerFrfsConfig :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Beckn.Context.City -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.FRFSConfigAPIRes),
    getFRFSTicketServiceCustomerFrfsAutocomplete :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> Kernel.Types.Beckn.Context.City -> Kernel.External.Maps.Types.LatLong -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.AutocompleteRes),
    getFRFSTicketServiceCustomerFrfsRoutes :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [API.Types.UI.FRFSTicketService.FRFSRouteAPI]),
    getFRFSTicketServiceCustomerFrfsStations :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient [API.Types.UI.FRFSTicketService.FRFSStationAPI]),
    postFRFSTicketServiceCustomerFrfsStationsPossibleStops :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq -> EulerHS.Types.EulerClient [API.Types.UI.FRFSTicketService.FRFSStationAPI]),
    getFRFSTicketServiceCustomerFrfsRoute :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe (Domain.Types.IntegratedBPPConfig.PlatformType) -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.FRFSRouteAPI),
    postFRFSTicketServiceCustomerFrfsSearch :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe ([BecknV2.FRFS.Enums.ServiceTierType]) -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.FRFSSearchAPIRes),
    getFRFSTicketServiceCustomerFrfsSearchQuote :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> EulerHS.Types.EulerClient [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]),
    postFRFSTicketServiceCustomerFrfsQuoteV2Confirm :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes),
    getFRFSTicketServiceCustomerFrfsBookingStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes),
    getFRFSTicketServiceCustomerFrfsRouteSeatLayout :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.SeatLayoutDetailsResp)
  }

mkFRFSTicketServiceAPIs :: (Client EulerHS.Types.EulerClient API -> FRFSTicketServiceAPIs)
mkFRFSTicketServiceAPIs fRFSTicketServiceClient = (FRFSTicketServiceAPIs {..})
  where
    getFRFSTicketServiceCustomerFrfsConfig :<|> getFRFSTicketServiceCustomerFrfsAutocomplete :<|> getFRFSTicketServiceCustomerFrfsRoutes :<|> getFRFSTicketServiceCustomerFrfsStations :<|> postFRFSTicketServiceCustomerFrfsStationsPossibleStops :<|> getFRFSTicketServiceCustomerFrfsRoute :<|> postFRFSTicketServiceCustomerFrfsSearch :<|> getFRFSTicketServiceCustomerFrfsSearchQuote :<|> postFRFSTicketServiceCustomerFrfsQuoteV2Confirm :<|> getFRFSTicketServiceCustomerFrfsBookingStatus :<|> getFRFSTicketServiceCustomerFrfsRouteSeatLayout = fRFSTicketServiceClient

data FRFSTicketServiceUserActionType
  = GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_CONFIG
  | GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_AUTOCOMPLETE
  | GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTES
  | GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS
  | POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS_POSSIBLE_STOPS
  | GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE
  | POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH
  | GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH_QUOTE
  | POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_QUOTE_V2_CONFIRM
  | GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_STATUS
  | GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SEAT_LAYOUT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''FRFSTicketServiceUserActionType)])
