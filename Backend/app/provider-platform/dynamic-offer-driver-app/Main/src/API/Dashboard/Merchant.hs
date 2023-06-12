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
           :<|> Common.MerchantCommonConfigUpdateAPI
           :<|> Common.DriverPoolConfigUpdateAPI
           :<|> Common.DriverPoolConfigCreateAPI
           :<|> Common.DriverIntelligentPoolConfigUpdateAPI
           :<|> Common.OnboardingDocumentConfigUpdateAPI
           :<|> Common.OnboardingDocumentConfigCreateAPI
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
    :<|> merchantCommonConfigUpdate merchantId
    :<|> driverPoolConfigUpdate merchantId
    :<|> driverPoolConfigCreate merchantId
    :<|> driverIntelligentPoolConfigUpdate merchantId
    :<|> onboardingDocumentConfigUpdate merchantId
    :<|> onboardingDocumentConfigCreate merchantId
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

merchantCommonConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantCommonConfigUpdateReq ->
  FlowHandler APISuccess
merchantCommonConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantCommonConfigUpdate merchantShortId

driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverPoolConfigUpdate merchantShortId tripDistance = withFlowHandlerAPI . DMerchant.driverPoolConfigUpdate merchantShortId tripDistance

driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  FlowHandler APISuccess
driverPoolConfigCreate merchantShortId tripDistance = withFlowHandlerAPI . DMerchant.driverPoolConfigCreate merchantShortId tripDistance

driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverIntelligentPoolConfigUpdate merchantShortId = withFlowHandlerAPI . DMerchant.driverIntelligentPoolConfigUpdate merchantShortId

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
