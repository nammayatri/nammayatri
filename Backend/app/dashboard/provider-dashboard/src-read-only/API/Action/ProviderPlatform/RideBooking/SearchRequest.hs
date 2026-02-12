{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.SearchRequest
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.SearchRequest
import qualified Domain.Action.ProviderPlatform.RideBooking.SearchRequest
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("searchRequest" :> (PostSearchRequestSearchrequests :<|> GetSearchRequestList :<|> GetSearchRequestInfo))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSearchRequestSearchrequests merchantId city :<|> getSearchRequestList merchantId city :<|> getSearchRequestInfo merchantId city

type PostSearchRequestSearchrequests =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.SEARCH_REQUEST / 'API.Types.Dashboard.RideBooking.SearchRequest.POST_SEARCH_REQUEST_SEARCHREQUESTS)
      :> API.Types.Dashboard.RideBooking.SearchRequest.PostSearchRequestSearchrequests
  )

type GetSearchRequestList =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.SEARCH_REQUEST / 'API.Types.Dashboard.RideBooking.SearchRequest.GET_SEARCH_REQUEST_LIST)
      :> API.Types.Dashboard.RideBooking.SearchRequest.GetSearchRequestList
  )

type GetSearchRequestInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.SEARCH_REQUEST / 'API.Types.Dashboard.RideBooking.SearchRequest.GET_SEARCH_REQUEST_INFO)
      :> API.Types.Dashboard.RideBooking.SearchRequest.GetSearchRequestInfo
  )

postSearchRequestSearchrequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsReq -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
postSearchRequestSearchrequests merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.SearchRequest.postSearchRequestSearchrequests merchantShortId opCity apiTokenInfo req

getSearchRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
getSearchRequestList merchantShortId opCity apiTokenInfo driverId fromDate toDate limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.SearchRequest.getSearchRequestList merchantShortId opCity apiTokenInfo driverId fromDate toDate limit offset

getSearchRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.RideBooking.SearchRequest.SearchReqInfoRes)
getSearchRequestInfo merchantShortId opCity apiTokenInfo fromDate toDate driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.SearchRequest.getSearchRequestInfo merchantShortId opCity apiTokenInfo fromDate toDate driverId
