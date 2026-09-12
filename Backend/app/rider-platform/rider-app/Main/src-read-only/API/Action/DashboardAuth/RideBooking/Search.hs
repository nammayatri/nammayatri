{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Search
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Search
import qualified "this" API.UI.Search
import qualified Domain.Action.Dashboard.RideBooking.Search
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("search" :> PostSearchRide)

type PostSearchRide = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/SEARCH/POST_SEARCH_RIDE" :> API.Types.Dashboard.RideBooking.Search.PostSearchRide)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchRide merchantId city

postSearchRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.UI.Search.SearchReq -> Environment.FlowHandler API.UI.Search.SearchResp)
postSearchRide a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Search.postSearchRide a5 a4 a2 a1
