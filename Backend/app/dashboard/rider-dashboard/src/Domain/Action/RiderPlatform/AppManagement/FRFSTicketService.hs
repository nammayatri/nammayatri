{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.AppManagement.FRFSTicketService
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
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified "rider-app" API.Types.UI.FRFSTicketService
import qualified BecknV2.FRFS.Enums
import qualified "rider-app" Domain.Types.FRFSQuote
import qualified "rider-app" Domain.Types.FRFSSearch
import qualified "rider-app" Domain.Types.FRFSTicketBooking
import qualified "rider-app" Domain.Types.IntegratedBPPConfig
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getFRFSTicketServiceCustomerFrfsConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Beckn.Context.City -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSConfigAPIRes)
getFRFSTicketServiceCustomerFrfsConfig merchantShortId opCity apiTokenInfo customerId city = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsConfig) customerId city

getFRFSTicketServiceCustomerFrfsAutocomplete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> Kernel.External.Maps.Types.LatLong -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow API.Types.UI.FRFSTicketService.AutocompleteRes)
getFRFSTicketServiceCustomerFrfsAutocomplete merchantShortId opCity apiTokenInfo customerId input limit offset platformType city location vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsAutocomplete) customerId input limit offset platformType city location vehicleType

getFRFSTicketServiceCustomerFrfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSRouteAPI])
getFRFSTicketServiceCustomerFrfsRoutes merchantShortId opCity apiTokenInfo customerId endStationCode startStationCode city vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsRoutes) customerId endStationCode startStationCode city vehicleType

getFRFSTicketServiceCustomerFrfsStations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI])
getFRFSTicketServiceCustomerFrfsStations merchantShortId opCity apiTokenInfo customerId city endStationCode location minimalData platformType routeCode startStationCode vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsStations) customerId city endStationCode location minimalData platformType routeCode startStationCode vehicleType

postFRFSTicketServiceCustomerFrfsStationsPossibleStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI])
postFRFSTicketServiceCustomerFrfsStationsPossibleStops merchantShortId opCity apiTokenInfo customerId city platformType vehicleType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.postFRFSTicketServiceCustomerFrfsStationsPossibleStops) customerId city platformType vehicleType req

getFRFSTicketServiceCustomerFrfsRoute :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType -> Kernel.Types.Beckn.Context.City -> BecknV2.FRFS.Enums.VehicleCategory -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSRouteAPI)
getFRFSTicketServiceCustomerFrfsRoute merchantShortId opCity apiTokenInfo customerId routeCode integratedBppConfigId platformType city vehicleType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsRoute) customerId routeCode integratedBppConfigId platformType city vehicleType

postFRFSTicketServiceCustomerFrfsSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) -> Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType] -> BecknV2.FRFS.Enums.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes)
postFRFSTicketServiceCustomerFrfsSearch merchantShortId opCity apiTokenInfo customerId city integratedBppConfigId newServiceTiers vehicleType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.postFRFSTicketServiceCustomerFrfsSearch) customerId city integratedBppConfigId newServiceTiers vehicleType req)

getFRFSTicketServiceCustomerFrfsSearchQuote :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes])
getFRFSTicketServiceCustomerFrfsSearchQuote merchantShortId opCity apiTokenInfo customerId searchId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsSearchQuote) customerId searchId

postFRFSTicketServiceCustomerFrfsQuoteV2Confirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
postFRFSTicketServiceCustomerFrfsQuoteV2Confirm merchantShortId opCity apiTokenInfo customerId quoteId isMockPayment req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.postFRFSTicketServiceCustomerFrfsQuoteV2Confirm) customerId quoteId isMockPayment req)

getFRFSTicketServiceCustomerFrfsBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes)
getFRFSTicketServiceCustomerFrfsBookingStatus merchantShortId opCity apiTokenInfo customerId bookingId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsBookingStatus) customerId bookingId

getFRFSTicketServiceCustomerFrfsRouteSeatLayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.UI.FRFSTicketService.SeatLayoutDetailsResp)
getFRFSTicketServiceCustomerFrfsRouteSeatLayout merchantShortId opCity apiTokenInfo customerId routeId fromStopIndex toStopIndex vehicleNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.fRFSTicketServiceDSL.getFRFSTicketServiceCustomerFrfsRouteSeatLayout) customerId routeId fromStopIndex toStopIndex vehicleNumber
