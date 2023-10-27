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
import qualified Kernel.Types.Beckn.City as City
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

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  merchantUpdate merchantId city
    :<|> merchantCommonConfig merchantId city
    :<|> merchantCommonConfigUpdate merchantId city
    :<|> driverPoolConfig merchantId city
    :<|> driverPoolConfigUpdate merchantId city
    :<|> driverPoolConfigCreate merchantId city
    :<|> driverIntelligentPoolConfig merchantId city
    :<|> driverIntelligentPoolConfigUpdate merchantId city
    :<|> onboardingDocumentConfig merchantId city
    :<|> onboardingDocumentConfigUpdate merchantId city
    :<|> onboardingDocumentConfigCreate merchantId city
    :<|> serviceUsageConfig merchantId city
    :<|> mapsServiceConfigUpdate merchantId city
    :<|> mapsServiceUsageConfigUpdate merchantId city
    :<|> smsServiceConfigUpdate merchantId city
    :<|> smsServiceUsageConfigUpdate merchantId city
    :<|> verificationServiceConfigUpdate merchantId city
    :<|> createFPDriverExtraFee merchantId city
    :<|> updateFPDriverExtraFee merchantId city

merchantUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.MerchantUpdateReq ->
  FlowHandler Common.MerchantUpdateRes
merchantUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.merchantUpdate merchantShortId opCity

merchantCommonConfig ::
  ShortId DM.Merchant ->
  City.City ->
  FlowHandler Common.MerchantCommonConfigRes
merchantCommonConfig merchantShortId = withFlowHandlerAPI . DMerchant.merchantCommonConfig merchantShortId

merchantCommonConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.MerchantCommonConfigUpdateReq ->
  FlowHandler APISuccess
merchantCommonConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.merchantCommonConfigUpdate merchantShortId opCity

driverPoolConfig ::
  ShortId DM.Merchant ->
  City.City ->
  Maybe Meters ->
  FlowHandler Common.DriverPoolConfigRes
driverPoolConfig merchantShortId opCity = withFlowHandlerAPI . DMerchant.driverPoolConfig merchantShortId opCity

driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Meters ->
  Common.DriverPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverPoolConfigUpdate merchantShortId opCity tripDistance = withFlowHandlerAPI . DMerchant.driverPoolConfigUpdate merchantShortId opCity tripDistance

driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  City.City ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  FlowHandler APISuccess
driverPoolConfigCreate merchantShortId opCity tripDistance = withFlowHandlerAPI . DMerchant.driverPoolConfigCreate merchantShortId opCity tripDistance

driverIntelligentPoolConfig ::
  ShortId DM.Merchant ->
  City.City ->
  FlowHandler Common.DriverIntelligentPoolConfigRes
driverIntelligentPoolConfig merchantShortId = withFlowHandlerAPI . DMerchant.driverIntelligentPoolConfig merchantShortId

driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverIntelligentPoolConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.driverIntelligentPoolConfigUpdate merchantShortId opCity

onboardingDocumentConfig ::
  ShortId DM.Merchant ->
  City.City ->
  Maybe Common.DocumentType ->
  FlowHandler Common.OnboardingDocumentConfigRes
onboardingDocumentConfig merchantShortId opCity = withFlowHandlerAPI . DMerchant.onboardingDocumentConfig merchantShortId opCity

onboardingDocumentConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigUpdateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigUpdate merchantShortId opCity documentType = withFlowHandlerAPI . DMerchant.onboardingDocumentConfigUpdate merchantShortId opCity documentType

onboardingDocumentConfigCreate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigCreate merchantShortId opCity documentType = withFlowHandlerAPI . DMerchant.onboardingDocumentConfigCreate merchantShortId opCity documentType

serviceUsageConfig ::
  ShortId DM.Merchant ->
  City.City ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId = withFlowHandlerAPI . DMerchant.serviceUsageConfig merchantShortId

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.mapsServiceConfigUpdate merchantShortId opCity

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.mapsServiceUsageConfigUpdate merchantShortId opCity

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.smsServiceConfigUpdate merchantShortId opCity

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.smsServiceUsageConfigUpdate merchantShortId opCity

verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  Common.VerificationServiceConfigUpdateReq ->
  FlowHandler APISuccess
verificationServiceConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.verificationServiceConfigUpdate merchantShortId opCity

createFPDriverExtraFee :: ShortId DM.Merchant -> City.City -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
createFPDriverExtraFee merchantShortId opCity farePolicyId startDistance req = withFlowHandlerAPI $ DMerchant.createFPDriverExtraFee merchantShortId opCity (cast farePolicyId) startDistance req

updateFPDriverExtraFee :: ShortId DM.Merchant -> City.City -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
updateFPDriverExtraFee merchantShortId opCity farePolicyId startDistance req = withFlowHandlerAPI $ DMerchant.updateFPDriverExtraFee merchantShortId opCity (cast farePolicyId) startDistance req
