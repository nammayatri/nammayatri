module Domain.Action.RiderPlatform.RideBooking.Frontend
  ( getFrontendFlowStatus,
    postFrontendNotifyEvent,
  )
where

import qualified API.Client.RiderPlatform.RideBooking
import qualified "rider-app" Domain.Action.UI.Frontend
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
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

getFrontendFlowStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow Domain.Action.UI.Frontend.GetPersonFlowStatusRes)
getFrontendFlowStatus merchantShortId opCity apiTokenInfo customerId isPolling checkForActiveBooking = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.frontendDSL.getFrontendFlowStatus) customerId isPolling checkForActiveBooking

postFrontendNotifyEvent :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Frontend.NotifyEventReq -> Environment.Flow Domain.Action.UI.Frontend.NotifyEventResp)
postFrontendNotifyEvent merchantShortId opCity apiTokenInfo customerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.frontendDSL.postFrontendNotifyEvent) customerId req)
