module API.RiderPlatform.Tickets
  ( API,
    handler,
  )
where

-- import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV

import qualified "rider-app" API.Dashboard.Tickets as ADT
import Dashboard.Common (HideSecrets)
import qualified "rider-app" Domain.Action.UI.Tickets as DTB
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Tickets as DTB
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "tickets"
    :> VerifyBookingDetailsAPI

type VerifyBookingDetailsAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'CUSTOMERS 'VERIFY_BOOKING_DETAILS
    :> ADT.VerifyBookingDetailsAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler = verifyBookingDetails

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
verifyBookingDetails merchantShortId opCity apiTokenInfo personServiceId ticketBookingShortId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction ADT.VerifyBookingDetails apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.tickets.verifyBookingDetails) personServiceId ticketBookingShortId
