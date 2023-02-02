module API.BPP.BecknTransport.Ride
  ( API,
    handler,
  )
where

import qualified BPPClient.BecknTransport as Client
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (DRIVER_OFFER_BPP)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "ride"
    :> ( RideListAPI
           :<|> RideStartAPI
           :<|> RideEndAPI
           :<|> RideCancelAPI
           :<|> RideInfoAPI
           :<|> RideSyncAPI
       )

type RideListAPI =
  ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'RIDES
    :> Common.RideListAPI

type RideStartAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'RIDES
    :> Common.RideStartAPI

type RideEndAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'RIDES
    :> Common.RideEndAPI

type RideCancelAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'RIDES
    :> Common.RideCancelAPI

type RideInfoAPI =
  ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'RIDES
    :> Common.RideInfoAPI

type RideSyncAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'RIDES
    :> Common.RideSyncAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  rideList merchantId
    :<|> rideStart merchantId
    :<|> rideEnd merchantId
    :<|> rideCancel merchantId
    :<|> rideInfo merchantId
    :<|> rideSync merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.RideEndpoint ->
  ApiTokenInfo ->
  Id Common.Ride ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo rideId =
  T.buildTransaction (DT.RideAPI endpoint) apiTokenInfo Nothing (Just rideId)

rideList ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.RideListRes
rideList merchantShortId apiTokenInfo mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone

rideStart :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideStartEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideStart) rideId req

rideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideEndEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideEnd) rideId req

rideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideCancelEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideCancel) rideId req

rideInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideInfo) rideId

rideSync :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideSyncEndpoint apiTokenInfo rideId T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callBecknTransportBPP checkedMerchantId (.rides.rideSync) rideId
