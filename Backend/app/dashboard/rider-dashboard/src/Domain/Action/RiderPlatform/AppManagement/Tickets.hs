module Domain.Action.RiderPlatform.AppManagement.Tickets
  ( postTicketsVerify,
    postTicketsServices,
    getTicketsPlaces,
    postTicketsUpdate,
    postTicketsBookingsCancel,
    postTicketsServiceCancel,
    getTicketsBookingDetails,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified "rider-app" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.TicketBooking
import qualified "rider-app" Domain.Types.TicketBookingService
import qualified "rider-app" Domain.Types.TicketPlace
import qualified "rider-app" Domain.Types.TicketService
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postTicketsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.Flow API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketsVerify merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsVerify) personServiceId ticketBookingShortId)

postTicketsServices ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
  Kernel.Prelude.Maybe Data.Time.Calendar.Day ->
  Environment.Flow [API.Types.UI.TicketService.TicketServiceResp]
postTicketsServices merchantShortId opCity apiTokenInfo ticketPlaceId date = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsServices) ticketPlaceId date

getTicketsPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [Domain.Types.TicketPlace.TicketPlace])
getTicketsPlaces merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsPlaces)

postTicketsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsUpdate) req

postTicketsBookingsCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketBookingCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsBookingsCancel merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsBookingsCancel) req

postTicketsServiceCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.TicketService.TicketServiceCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTicketsServiceCancel merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.postTicketsServiceCancel) req

getTicketsBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.Flow API.Types.UI.TicketService.TicketBookingDetails)
getTicketsBookingDetails merchantShortId opCity apiTokenInfo ticketBookingShortId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.ticketsDSL.getTicketsBookingDetails) ticketBookingShortId
