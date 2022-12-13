module API.BPP.BecknTransport.Ride
  ( API,
    handler,
  )
where

import qualified BPPClient.BecknTransport as Client
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Servant hiding (throwError)
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "ride"
    :> ( RideListAPI
           :<|> RideStartAPI
           :<|> RideEndAPI
           :<|> RideCancelAPI
           :<|> RideInfoAPI
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

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  rideList merchantId
    :<|> rideStart merchantId
    :<|> rideEnd merchantId
    :<|> rideCancel merchantId
    :<|> rideInfo merchantId

rideList ::
  ShortId DM.Merchant ->
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (Id Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.RideListRes
rideList userMerchantId merchantId mbLimit mbOffset mbBookingStatus mbRideId mbCustomerPhone mbDriverPhone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbRideId mbCustomerPhone mbDriverPhone

rideStart :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart userMerchantId merchantId rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideStart) rideId req

rideEnd :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd userMerchantId merchantId rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideEnd) rideId req

rideCancel :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel userMerchantId merchantId rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideCancel) rideId req

rideInfo :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo userMerchantId merchantId rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.rides.rideInfo) rideId
