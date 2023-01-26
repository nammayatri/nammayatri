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
           :<|> Common.MapsServiceConfigUpdateAPI
           :<|> Common.MapsServiceUsageConfigUpdateAPI
           :<|> Common.SmsServiceConfigUpdateAPI
           :<|> Common.SmsServiceUsageConfigUpdateAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> mapsServiceConfigUpdate merchantId
    :<|> mapsServiceUsageConfigUpdate merchantId
    :<|> smsServiceConfigUpdate merchantId
    :<|> smsServiceUsageConfigUpdate merchantId

merchantUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantUpdateReq ->
  FlowHandler APISuccess
merchantUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantUpdate merchantShortId

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.mapsServiceConfigUpdate merchantShortId

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.mapsServiceUsageConfigUpdate merchantShortId

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.smsServiceConfigUpdate merchantShortId

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.smsServiceUsageConfigUpdate merchantShortId
