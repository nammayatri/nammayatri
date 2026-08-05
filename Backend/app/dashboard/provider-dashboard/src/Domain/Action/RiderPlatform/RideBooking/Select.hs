module Domain.Action.RiderPlatform.RideBooking.Select
  ( postSelectEstimate,
    getSelectQuotes,
    getSelectResult,
    postSelectCancelSearch,
  )
where

import qualified API.Client.RiderPlatform.RideBooking
import qualified "rider-app" Domain.Action.UI.Select
import qualified "rider-app" Domain.Types.Estimate
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified "rider-app" SharedLogic.Cancel
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postSelectEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Domain.Action.UI.Select.DSelectReq -> Environment.Flow Domain.Action.UI.Select.MultimodalSelectRes)
postSelectEstimate merchantShortId opCity apiTokenInfo customerId estimateId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.selectDSL.postSelectEstimate) customerId estimateId req)

getSelectQuotes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.Flow Domain.Action.UI.Select.SelectListRes)
getSelectQuotes merchantShortId opCity apiTokenInfo customerId estimateId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.selectDSL.getSelectQuotes) customerId estimateId

getSelectResult :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.Flow Domain.Action.UI.Select.QuotesResultResponse)
getSelectResult merchantShortId opCity apiTokenInfo customerId estimateId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.selectDSL.getSelectResult) customerId estimateId

postSelectCancelSearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Environment.Flow SharedLogic.Cancel.CancelAPIResponse)
postSelectCancelSearch merchantShortId opCity apiTokenInfo customerId estimateId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.selectDSL.postSelectCancelSearch) customerId estimateId)
