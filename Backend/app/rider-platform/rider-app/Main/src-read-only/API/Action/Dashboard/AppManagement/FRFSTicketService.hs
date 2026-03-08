{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.FRFSTicketService
  ( API.Types.Dashboard.AppManagement.FRFSTicketService.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.FRFSTicketService.API)
handler merchantId city = getFRFSTicketServiceCustomerFrfsConfig merchantId city :<|> getFRFSTicketServiceCustomerFrfsAutocomplete merchantId city :<|> getFRFSTicketServiceCustomerFrfsRoutes merchantId city :<|> getFRFSTicketServiceCustomerFrfsStations merchantId city :<|> postFRFSTicketServiceCustomerFrfsStationsPossibleStops merchantId city :<|> getFRFSTicketServiceCustomerFrfsRoute merchantId city :<|> postFRFSTicketServiceCustomerFrfsSearch merchantId city :<|> getFRFSTicketServiceCustomerFrfsSearchQuote merchantId city :<|> postFRFSTicketServiceCustomerFrfsQuoteV2Confirm merchantId city :<|> getFRFSTicketServiceCustomerFrfsBookingStatus merchantId city :<|> getFRFSTicketServiceCustomerFrfsRouteSeatLayout merchantId city :<|> getFRFSTicketServiceCustomerFrfsTripRouteSeats merchantId city :<|> postFRFSTicketServiceCustomerFrfsRouteServiceability merchantId city

getFRFSTicketServiceCustomerFrfsConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSConfigAPIRes)
getFRFSTicketServiceCustomerFrfsConfig a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsConfig a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsAutocomplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> Kernel.External.Maps.Types.LatLong -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.UI.FRFSTicketService.AutocompleteRes)
getFRFSTicketServiceCustomerFrfsAutocomplete a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsAutocomplete a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSRouteAPI])
getFRFSTicketServiceCustomerFrfsRoutes a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRoutes a7 a6 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI])
getFRFSTicketServiceCustomerFrfsStations a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsStations a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postFRFSTicketServiceCustomerFrfsStationsPossibleStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI])
postFRFSTicketServiceCustomerFrfsStationsPossibleStops a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsStationsPossibleStops a7 a6 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSRouteAPI)
getFRFSTicketServiceCustomerFrfsRoute a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRoute a8 a7 a6 a5 a4 a3 a2 a1

postFRFSTicketServiceCustomerFrfsSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType] -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSSearchAPIRes)
postFRFSTicketServiceCustomerFrfsSearch a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsSearch a8 a7 a6 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsSearchQuote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes])
getFRFSTicketServiceCustomerFrfsSearchQuote a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsSearchQuote a4 a3 a2 a1

postFRFSTicketServiceCustomerFrfsQuoteV2Confirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
postFRFSTicketServiceCustomerFrfsQuoteV2Confirm a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsQuoteV2Confirm a6 a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
getFRFSTicketServiceCustomerFrfsBookingStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsBookingStatus a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsRouteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.FRFSTicketService.SeatLayoutDetailsResp)
getFRFSTicketServiceCustomerFrfsRouteSeatLayout a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsRouteSeatLayout a5 a4 a3 a2 a1

getFRFSTicketServiceCustomerFrfsTripRouteSeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.FRFSTicketService.SeatLayoutResp)
getFRFSTicketServiceCustomerFrfsTripRouteSeats a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.getFRFSTicketServiceCustomerFrfsTripRouteSeats a8 a7 a6 a5 a4 a3 a2 a1

postFRFSTicketServiceCustomerFrfsRouteServiceability :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> API.Types.UI.FRFSTicketService.FRFSRouteServiceabilityReq -> Environment.FlowHandler API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle)
postFRFSTicketServiceCustomerFrfsRouteServiceability a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FRFSTicketService.postFRFSTicketServiceCustomerFrfsRouteServiceability a5 a4 a3 a2 a1
