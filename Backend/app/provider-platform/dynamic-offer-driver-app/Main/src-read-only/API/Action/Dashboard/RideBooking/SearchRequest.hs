{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.SearchRequest
  ( API.Types.Dashboard.RideBooking.SearchRequest.API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.SearchRequest
import qualified Domain.Action.Dashboard.RideBooking.SearchRequest
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.SearchRequest.API)
handler merchantId city = postSearchRequestSearchrequests merchantId city :<|> getSearchRequestList merchantId city :<|> getSearchRequestInfo merchantId city

postSearchRequestSearchrequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsReq -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
postSearchRequestSearchrequests a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.SearchRequest.postSearchRequestSearchrequests a3 a2 a1

getSearchRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
getSearchRequestList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.SearchRequest.getSearchRequestList a7 a6 a5 a4 a3 a2 a1

getSearchRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchReqInfoRes)
getSearchRequestInfo a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.SearchRequest.getSearchRequestInfo a5 a4 a3 a2 a1
