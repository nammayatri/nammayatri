module Domain.Action.ProviderPlatform.Operator.FrfsTripManagement
  ( postOperatorFrfsTripAction,
  )
where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Operator.FrfsTripManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Tools.Auth.Api
import Tools.Auth.Merchant

postOperatorFrfsTripAction :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FrfsTripManagement.OperatorTripActionReq -> Environment.Flow API.Types.ProviderPlatform.Operator.FrfsTripManagement.OperatorTripActionRes
postOperatorFrfsTripAction merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.frfsTripManagementDSL.postOperatorFrfsTripAction) req
