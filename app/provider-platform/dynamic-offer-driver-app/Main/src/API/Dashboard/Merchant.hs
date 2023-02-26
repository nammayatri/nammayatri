module API.Dashboard.Merchant where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

type API =
  "merchant"
    :> ( Common.MerchantUpdateAPI
           :<|> Common.MapsServiceConfigUpdateAPI
           :<|> Common.MapsServiceUsageConfigUpdateAPI
           :<|> Common.SmsServiceConfigUpdateAPI
           :<|> Common.SmsServiceUsageConfigUpdateAPI
           :<|> Common.TransporterConfigUpdateAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> mapsServiceConfigUpdate merchantId
    :<|> mapsServiceUsageConfigUpdate merchantId
    :<|> smsServiceConfigUpdate merchantId
    :<|> smsServiceUsageConfigUpdate merchantId
    :<|> transporterConfigUpdate merchantId

merchantUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantUpdateReq ->
  FlowHandler Common.MerchantUpdateRes
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

transporterConfigUpdate ::
  ShortId DM.Merchant ->
  Common.TransporterConfigUpdateAPIReq ->
  FlowHandler APISuccess
transporterConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.transporterConfigUpdate merchantShortId
