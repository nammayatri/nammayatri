module Domain.Action.Dashboard.AppManagement.FRFSTicketService
  ( getFRFSTicketServiceCustomerFrfsConfig,
    getFRFSTicketServiceCustomerFrfsAutocomplete,
    getFRFSTicketServiceCustomerFrfsRoutes,
    getFRFSTicketServiceCustomerFrfsStations,
    postFRFSTicketServiceCustomerFrfsStationsPossibleStops,
    getFRFSTicketServiceCustomerFrfsRoute,
    postFRFSTicketServiceCustomerFrfsSearch,
    getFRFSTicketServiceCustomerFrfsSearchQuote,
    postFRFSTicketServiceCustomerFrfsQuoteV2Confirm,
    getFRFSTicketServiceCustomerFrfsBookingStatus,
    getFRFSTicketServiceCustomerFrfsRouteSeatLayout,
    postFRFSTicketServiceCustomerFrfsRouteServiceability,
  )
where

import qualified "this" API.Types.UI.FRFSTicketService
import qualified "this" API.Types.UI.MultimodalConfirm
import qualified BecknV2.FRFS.Enums
import qualified Domain.Action.UI.FRFSTicketService as DFrfs
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import qualified Storage.CachedQueries.Merchant as QM

getFRFSTicketServiceCustomerFrfsConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Beckn.Context.City -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSConfigAPIRes)
getFRFSTicketServiceCustomerFrfsConfig merchantShortId _opCity customerId city = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsConfig (Just customerId, merchant.id) city

getFRFSTicketServiceCustomerFrfsAutocomplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> Kernel.External.Maps.Types.LatLong -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow API.Types.UI.FRFSTicketService.AutocompleteRes)
getFRFSTicketServiceCustomerFrfsAutocomplete merchantShortId _opCity customerId input limit offset platformType city location vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsAutocomplete (Just customerId, merchant.id) input limit offset platformType city location vehicleType

getFRFSTicketServiceCustomerFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSRouteAPI])
getFRFSTicketServiceCustomerFrfsRoutes merchantShortId _opCity customerId endStationCode startStationCode city vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsRoutes (Just customerId, merchant.id) endStationCode startStationCode city vehicleType

getFRFSTicketServiceCustomerFrfsStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI])
getFRFSTicketServiceCustomerFrfsStations merchantShortId _opCity customerId city endStationCode location minimalData platformType routeCode startStationCode vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsStations (Just customerId, merchant.id) city endStationCode location minimalData platformType routeCode startStationCode vehicleType

postFRFSTicketServiceCustomerFrfsStationsPossibleStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI])
postFRFSTicketServiceCustomerFrfsStationsPossibleStops merchantShortId _opCity customerId city platformType vehicleType req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.postFrfsStationsPossibleStops (Just customerId, merchant.id) city platformType vehicleType req

getFRFSTicketServiceCustomerFrfsRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSRouteAPI)
getFRFSTicketServiceCustomerFrfsRoute merchantShortId _opCity customerId routeCode integratedBppConfigId platformType city vehicleType = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsRoute (Just customerId, merchant.id) routeCode integratedBppConfigId platformType city vehicleType

postFRFSTicketServiceCustomerFrfsSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType] -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes)
postFRFSTicketServiceCustomerFrfsSearch merchantShortId _opCity customerId city integratedBppConfigId newServiceTiers vehicleType req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.postFrfsSearch (Just customerId, merchant.id) city integratedBppConfigId newServiceTiers vehicleType req

getFRFSTicketServiceCustomerFrfsSearchQuote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes])
getFRFSTicketServiceCustomerFrfsSearchQuote merchantShortId _opCity customerId searchId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsSearchQuote (Just customerId, merchant.id) searchId

postFRFSTicketServiceCustomerFrfsQuoteV2Confirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
postFRFSTicketServiceCustomerFrfsQuoteV2Confirm merchantShortId _opCity customerId quoteId isMockPayment req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.postFrfsQuoteV2Confirm (Just customerId, merchant.id) quoteId isMockPayment req

getFRFSTicketServiceCustomerFrfsBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
getFRFSTicketServiceCustomerFrfsBookingStatus merchantShortId _opCity customerId bookingId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsBookingStatus (Just customerId, merchant.id) bookingId

getFRFSTicketServiceCustomerFrfsRouteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.UI.FRFSTicketService.SeatLayoutDetailsResp)
getFRFSTicketServiceCustomerFrfsRouteSeatLayout merchantShortId _opCity customerId routeId vehicleNumber = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.getFrfsRouteSeatLayout (Just customerId, merchant.id) routeId vehicleNumber

postFRFSTicketServiceCustomerFrfsRouteServiceability :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> API.Types.UI.FRFSTicketService.FRFSRouteServiceabilityReq -> Environment.Flow API.Types.UI.MultimodalConfirm.RouteWithLiveVehicle)
postFRFSTicketServiceCustomerFrfsRouteServiceability merchantShortId _opCity customerId routeId req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  DFrfs.postFrfsRouteServiceability (Just customerId, merchant.id) routeId req
