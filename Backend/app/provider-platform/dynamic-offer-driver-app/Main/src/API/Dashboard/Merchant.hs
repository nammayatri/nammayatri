{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Merchant where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common (Meters, withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

type API =
  "merchant"
    :> ( Common.MerchantUpdateAPI
           :<|> Common.MerchantCommonConfigAPI
           :<|> Common.MerchantCommonConfigUpdateAPI
           :<|> Common.DriverPoolConfigAPI
           :<|> Common.DriverPoolConfigUpdateAPI
           :<|> Common.DriverPoolConfigCreateAPI
           :<|> Common.DriverIntelligentPoolConfigAPI
           :<|> Common.DriverIntelligentPoolConfigUpdateAPI
           :<|> Common.OnboardingDocumentConfigAPI
           :<|> Common.OnboardingDocumentConfigUpdateAPI
           :<|> Common.OnboardingDocumentConfigCreateAPI
           :<|> Common.ServiceUsageConfigAPI
           :<|> Common.MapsServiceConfigUpdateAPI
           :<|> Common.MapsServiceUsageConfigUpdateAPI
           :<|> Common.SmsServiceConfigUpdateAPI
           :<|> Common.SmsServiceUsageConfigUpdateAPI
           :<|> Common.VerificationServiceConfigUpdateAPI
           :<|> Common.CreateFPDriverExtraFee
           :<|> Common.UpdateFPDriverExtraFee
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> merchantCommonConfig merchantId
    :<|> merchantCommonConfigUpdate merchantId
    :<|> driverPoolConfig merchantId
    :<|> driverPoolConfigUpdate merchantId
    :<|> driverPoolConfigCreate merchantId
    :<|> driverIntelligentPoolConfig merchantId
    :<|> driverIntelligentPoolConfigUpdate merchantId
    :<|> onboardingDocumentConfig merchantId
    :<|> onboardingDocumentConfigUpdate merchantId
    :<|> onboardingDocumentConfigCreate merchantId
    :<|> serviceUsageConfig merchantId
    :<|> mapsServiceConfigUpdate merchantId
    :<|> mapsServiceUsageConfigUpdate merchantId
    :<|> smsServiceConfigUpdate merchantId
    :<|> smsServiceUsageConfigUpdate merchantId
    :<|> verificationServiceConfigUpdate merchantId
    :<|> createFPDriverExtraFee merchantId
    :<|> updateFPDriverExtraFee merchantId

merchantUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantUpdateReq ->
  FlowHandler Common.MerchantUpdateRes
merchantUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantUpdate merchantShortId

merchantCommonConfig ::
  ShortId DM.Merchant ->
  FlowHandler Common.MerchantCommonConfigRes
merchantCommonConfig = withFlowHandlerAPI . DMerchant.merchantCommonConfig

merchantCommonConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantCommonConfigUpdateReq ->
  FlowHandler APISuccess
merchantCommonConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantCommonConfigUpdate merchantShortId

driverPoolConfig ::
  ShortId DM.Merchant ->
  Maybe Meters ->
  FlowHandler Common.DriverPoolConfigRes
driverPoolConfig merchantShortId = withFlowHandlerAPI . DMerchant.driverPoolConfig merchantShortId

driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Meters ->
  Maybe Common.Variant ->
  Common.DriverPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverPoolConfigUpdate merchantShortId tripDistance variant = withFlowHandlerAPI . DMerchant.driverPoolConfigUpdate merchantShortId tripDistance variant

driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  FlowHandler APISuccess
driverPoolConfigCreate merchantShortId tripDistance = withFlowHandlerAPI . DMerchant.driverPoolConfigCreate merchantShortId tripDistance

driverIntelligentPoolConfig ::
  ShortId DM.Merchant ->
  FlowHandler Common.DriverIntelligentPoolConfigRes
driverIntelligentPoolConfig = withFlowHandlerAPI . DMerchant.driverIntelligentPoolConfig

driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverIntelligentPoolConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.driverIntelligentPoolConfigUpdate merchantShortId

onboardingDocumentConfig ::
  ShortId DM.Merchant ->
  Maybe Common.DocumentType ->
  FlowHandler Common.OnboardingDocumentConfigRes
onboardingDocumentConfig merchantShortId = withFlowHandlerAPI . DMerchant.onboardingDocumentConfig merchantShortId

onboardingDocumentConfigUpdate ::
  ShortId DM.Merchant ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigUpdateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigUpdate merchantShortId documentType = withFlowHandlerAPI . DMerchant.onboardingDocumentConfigUpdate merchantShortId documentType

onboardingDocumentConfigCreate ::
  ShortId DM.Merchant ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigCreate merchantShortId documentType = withFlowHandlerAPI . DMerchant.onboardingDocumentConfigCreate merchantShortId documentType

serviceUsageConfig ::
  ShortId DM.Merchant ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig = withFlowHandlerAPI . DMerchant.serviceUsageConfig

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

verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.VerificationServiceConfigUpdateReq ->
  FlowHandler APISuccess
verificationServiceConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.verificationServiceConfigUpdate merchantShortId

createFPDriverExtraFee :: ShortId DM.Merchant -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
createFPDriverExtraFee merchantShortId farePolicyId startDistance req = withFlowHandlerAPI $ DMerchant.createFPDriverExtraFee merchantShortId (cast farePolicyId) startDistance req

updateFPDriverExtraFee :: ShortId DM.Merchant -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
updateFPDriverExtraFee merchantShortId farePolicyId startDistance req = withFlowHandlerAPI $ DMerchant.updateFPDriverExtraFee merchantShortId (cast farePolicyId) startDistance req
