module API.Dashboard.Merchant where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified "dashboard-bpp-helper-api" Dashboard.BAP.Merchant as Common
import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Types.Merchant as DM
import Environment
import Servant hiding (Unauthorized, throwError)

type API =
  "merchant"
    :> ( Common.MerchantUpdateAPI
           :<|> Common.MerchantServiceConfigUpdateAPI
           :<|> Common.MerchantServiceConfigUsageUpdateAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> merchantServiceConfigUpdate merchantId
    :<|> merchantServiceConfigUsageUpdate merchantId

merchantUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantUpdateReq ->
  FlowHandler APISuccess
merchantUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantUpdate merchantShortId

merchantServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantServiceConfigUpdateReq ->
  FlowHandler APISuccess
merchantServiceConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantServiceConfigUpdate merchantShortId

merchantServiceConfigUsageUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
merchantServiceConfigUsageUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantServiceConfigUsageUpdate merchantShortId
