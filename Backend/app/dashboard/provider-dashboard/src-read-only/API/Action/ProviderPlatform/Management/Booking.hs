{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.ProviderPlatform.Management.Booking 
( API,
handler )
where
import EulerHS.Prelude hiding (sortOn)
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common hiding (INFO)
import Storage.Beam.CommonInstances ()
import qualified API.Types.ProviderPlatform.Management.Booking
import qualified API.Types.ProviderPlatform.Management
import qualified Domain.Action.ProviderPlatform.Management.Booking
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Dashboard.Common.Booking
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("booking" :> (PostBookingCancelAllStuck :<|> PostBookingSyncMultiple))
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postBookingCancelAllStuck merchantId city :<|> postBookingSyncMultiple merchantId city
type PostBookingCancelAllStuck = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                          ('DSL)
                                          (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.BOOKING) / ('API.Types.ProviderPlatform.Management.Booking.POST_BOOKING_CANCEL_ALL_STUCK)) :> API.Types.ProviderPlatform.Management.Booking.PostBookingCancelAllStuck)
type PostBookingSyncMultiple = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                        ('DSL)
                                        (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.BOOKING) / ('API.Types.ProviderPlatform.Management.Booking.POST_BOOKING_SYNC_MULTIPLE)) :> API.Types.ProviderPlatform.Management.Booking.PostBookingSyncMultiple)
postBookingCancelAllStuck :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Booking.StuckBookingsCancelReq -> Environment.FlowHandler Dashboard.Common.Booking.StuckBookingsCancelRes)
postBookingCancelAllStuck merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Booking.postBookingCancelAllStuck merchantShortId opCity apiTokenInfo req
postBookingSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Booking.MultipleBookingSyncReq -> Environment.FlowHandler Dashboard.Common.Booking.MultipleBookingSyncResp)
postBookingSyncMultiple merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Booking.postBookingSyncMultiple merchantShortId opCity apiTokenInfo req



