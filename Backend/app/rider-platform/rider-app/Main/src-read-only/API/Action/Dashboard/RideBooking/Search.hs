{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.RideBooking.Search 
( API.Types.Dashboard.RideBooking.Search.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.RideBooking.Search
import qualified "this" Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified "this" API.UI.Search
import qualified API.Types.Dashboard.RideBooking.Search



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Search.API)
handler merchantId city = postSearchRide merchantId city
postSearchRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.UI.Search.SearchReq -> Environment.FlowHandler API.UI.Search.SearchResp)
postSearchRide a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Search.postSearchRide a4 a3 a2 a1



