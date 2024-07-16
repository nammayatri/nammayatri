module API.RiderPlatform.Tickets
  ( API,
    handler,
  )
where

-- import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV

import qualified "rider-app" API.Dashboard.Tickets as ADT
import qualified "rider-app" API.Types.UI.TicketService as DTB
import Dashboard.Common (HideSecrets)
import Data.Time
import qualified "rider-app" Domain.Action.UI.TicketService as DTB
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.TicketBooking as DTB
import qualified "rider-app" Domain.Types.TicketBookingService as DTB
import qualified "rider-app" Domain.Types.TicketPlace as DTB
import qualified "rider-app" Domain.Types.TicketService as DTB
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "tickets"
    :> VerifyBookingDetailsAPI
    :<|> GetServicesAPI
    :<|> UpdateSeatManagementAPI
    :<|> GetTicketPlacesAPI
    :<|> CancelTicketBookingServiceAPI
    :<|> CancelTicketServiceAPI
    :<|> GetTicketBookingDetails
    :<|> GetTicketBookingListAPI

type VerifyBookingDetailsAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'VERIFY_BOOKING_DETAILS
    :> ADT.VerifyBookingDetailsAPI

type GetServicesAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'GET_TICKET_SERVICES
    :> ADT.GetServicesAPI

type UpdateSeatManagementAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'UPDATE_SEAT_MANAGEMENT
    :> ADT.UpdateSeatManagementAPI

type GetTicketPlacesAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'GET_TICKET_PLACES
    :> ADT.GetTicketPlacesAPI

type CancelTicketBookingServiceAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'CANCEL_TICKET_BOOKING
    :> ADT.CancelTicketBookingServiceAPI

type CancelTicketServiceAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'CANCEL_TICKET_SERVICE
    :> ADT.CancelTicketServiceAPI

type GetTicketBookingDetails =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'GET_TICKET_BOOKING_DETAILS
    :> ADT.GetTicketBookingDetailsAPI

type GetTicketBookingListAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'GET_TICKET_BOOKING_LIST
    :> ADT.GetTicketBookingListAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  verifyBookingDetails merchantId city
    :<|> getServices merchantId city
    :<|> updateSeatManagement merchantId city
    :<|> getTicketPlaces merchantId city
    :<|> cancelTicketBookingService merchantId city
    :<|> cancelTicketService merchantId city
    :<|> getTicketBookingDetails merchantId city
    :<|> getTicketBookingList merchantId city

buildTransaction ::
  ( MonadFlow m,
    HideSecrets request
  ) =>
  ADT.TicketBookingEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo = T.buildTransaction (DT.TicketsAPI endpoint) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

verifyBookingDetails ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id DTB.TicketService ->
  ShortId DTB.TicketBookingService ->
  FlowHandler DTB.TicketServiceVerificationResp
verifyBookingDetails merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction ADT.VerifyBookingDetails apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.tickets.verifyBookingDetails) personServiceId ticketBookingShortId

getServices ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id DTB.TicketPlace ->
  Maybe Day ->
  FlowHandler [DTB.TicketServiceResp]
getServices merchantShortId opCity apiTokenInfo ticketPlaceId mbDate = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.tickets.getServices) ticketPlaceId mbDate

updateSeatManagement ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  DTB.TicketBookingUpdateSeatsReq ->
  FlowHandler APISuccess
updateSeatManagement merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.tickets.updateSeatManagement) req

getTicketPlaces :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> FlowHandler [DTB.TicketPlace]
getTicketPlaces merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.tickets.getTicketPlaces)

cancelTicketBookingService ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  DTB.TicketBookingCancelReq ->
  FlowHandler APISuccess
cancelTicketBookingService merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.tickets.cancelTicketBookingService) req

cancelTicketService ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  DTB.TicketServiceCancelReq ->
  FlowHandler APISuccess
cancelTicketService merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.tickets.cancelTicketService) req

getTicketBookingDetails ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id.ShortId DTB.TicketBooking ->
  FlowHandler DTB.TicketBookingDetails
getTicketBookingDetails merchantShortId opCity apiTokenInfo ticketBookingShortId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.tickets.getTicketBookingDetails) ticketBookingShortId

getTicketBookingList ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id DTB.TicketPlace ->
  Maybe Text ->
  Maybe (Id DTB.TicketBookingService) ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler DTB.TicketBookingListRes
getTicketBookingList merchantShortId opCity apiTokenInfo ticketPlaceId mbTicketBookinShordId mbTicketBookingServiceId mbTicketBookingStatus mbFrom mbTo mbLimit mbOffset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.tickets.getTicketBookingList) ticketPlaceId mbTicketBookinShordId mbTicketBookingServiceId mbTicketBookingStatus mbFrom mbTo mbLimit mbOffset
