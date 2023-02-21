module API.ProviderPlatform.StaticOfferDriver.Booking
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified ProviderPlatformClient.StaticOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (DRIVER_OFFER_BPP)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "booking"
    :> StuckBookingsCancelAPI

type StuckBookingsCancelAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'RIDES -- 'WRITE_ACCESS 'BOOKINGS ?
    :> Common.StuckBookingsCancelAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler =
  stuckBookingsCancel

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.BookingEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.BookingAPI endpoint) apiTokenInfo Nothing Nothing

stuckBookingsCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Common.StuckBookingsCancelReq -> FlowHandler Common.StuckBookingsCancelRes
stuckBookingsCancel merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.StuckBookingsCancelEndpoint apiTokenInfo (Just req)
  T.withResponseTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.bookings.stuckBookingsCancel) req
