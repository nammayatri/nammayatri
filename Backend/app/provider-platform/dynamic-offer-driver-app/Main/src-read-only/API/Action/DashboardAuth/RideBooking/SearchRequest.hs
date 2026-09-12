{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.SearchRequest
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("searchRequest" :> (PostSearchRequestSearchrequests :<|> GetSearchRequestList :<|> GetSearchRequestInfo))

type PostSearchRequestSearchrequests =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/POST_SEARCH_REQUEST_SEARCHREQUESTS"
      :> API.Types.Dashboard.RideBooking.SearchRequest.PostSearchRequestSearchrequests
  )

type GetSearchRequestList = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_LIST" :> API.Types.Dashboard.RideBooking.SearchRequest.GetSearchRequestList)

type GetSearchRequestInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_INFO" :> API.Types.Dashboard.RideBooking.SearchRequest.GetSearchRequestInfo)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchRequestSearchrequests merchantId city :<|> getSearchRequestList merchantId city :<|> getSearchRequestInfo merchantId city

postSearchRequestSearchrequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsReq -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
postSearchRequestSearchrequests a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.SearchRequest.postSearchRequestSearchrequests a4 a3 a1

getSearchRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
getSearchRequestList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.SearchRequest.getSearchRequestList a8 a7 a5 a4 a3 a2 a1

getSearchRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchReqInfoRes)
getSearchRequestInfo a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.SearchRequest.getSearchRequestInfo a6 a5 a3 a2 a1
