{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.RideBooking.SearchRequest (postSearchRequestSearchrequests, getSearchRequestList, getSearchRequestInfo) where

import qualified API.Client.ProviderPlatform.RideBooking
import qualified API.Types.Dashboard.RideBooking.SearchRequest
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postSearchRequestSearchrequests :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsReq -> Environment.Flow API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
postSearchRequestSearchrequests merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.searchRequestDSL.postSearchRequestSearchrequests) req)

getSearchRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.Flow API.Types.Dashboard.RideBooking.SearchRequest.SearchRequestsRes)
getSearchRequestList merchantShortId opCity apiTokenInfo driverId fromDate toDate mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.searchRequestDSL.getSearchRequestList) driverId fromDate toDate mbLimit mbOffset

getSearchRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow API.Types.Dashboard.RideBooking.SearchRequest.SearchReqInfoRes)
getSearchRequestInfo merchantShortId opCity apiTokenInfo fromDate toDate driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.searchRequestDSL.getSearchRequestInfo) fromDate toDate driverId
