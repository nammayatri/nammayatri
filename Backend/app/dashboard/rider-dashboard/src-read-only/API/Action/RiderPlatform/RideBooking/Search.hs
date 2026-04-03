{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.RiderPlatform.RideBooking.Search 
( API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified "rider-app" API.Types.Dashboard.RideBooking.Search
import qualified API.Types.Dashboard.RideBooking
import qualified Domain.Action.RiderPlatform.RideBooking.Search
import qualified "rider-app" Domain.Types.Person
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified "rider-app" API.UI.Search
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("search" :> PostSearchRide)
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchRide merchantId city
type PostSearchRide = (ApiAuth ('APP_BACKEND)
                               ('DSL)
                               (('RIDER_RIDE_BOOKING) / ('API.Types.Dashboard.RideBooking.SEARCH) / ('API.Types.Dashboard.RideBooking.Search.POST_SEARCH_RIDE)) :> API.Types.Dashboard.RideBooking.Search.PostSearchRide)
postSearchRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.UI.Search.SearchReq -> Environment.FlowHandler API.UI.Search.SearchResp)
postSearchRide merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Search.postSearchRide merchantShortId opCity apiTokenInfo customerId req



