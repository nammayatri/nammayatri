module Domain.Action.RiderPlatform.RideBooking.Search (postSearchRide) where

import qualified API.Client.RiderPlatform.RideBooking
import qualified "rider-app" API.UI.Search
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

postSearchRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.UI.Search.SearchReq -> Environment.Flow API.UI.Search.SearchResp)
postSearchRide merchantShortId opCity apiTokenInfo customerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.searchDSL.postSearchRide) customerId req)
