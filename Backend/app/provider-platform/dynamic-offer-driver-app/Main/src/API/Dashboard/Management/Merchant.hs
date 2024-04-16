{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Merchant where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (Meters, withFlowHandlerAPI)
import qualified Lib.Types.SpecialLocation as SL
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.SystemConfigs ()

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
           :<|> Common.DocumentVerificationConfigAPI
           :<|> Common.DocumentVerificationConfigUpdateAPI
           :<|> Common.DocumentVerificationConfigCreateAPI
           :<|> Common.ServiceUsageConfigAPI
           :<|> Common.MapsServiceConfigUpdateAPI
           :<|> Common.MapsServiceUsageConfigUpdateAPI
           :<|> Common.SmsServiceConfigUpdateAPI
           :<|> Common.SmsServiceUsageConfigUpdateAPI
           :<|> Common.VerificationServiceConfigUpdateAPI
           :<|> Common.CreateFPDriverExtraFee
           :<|> Common.UpdateFPDriverExtraFee
           :<|> Common.UpdateFPPerExtraKmRate
           :<|> Common.UpdateFarePolicy
           :<|> Common.CreateMerchantOperatingCityAPIT
           :<|> Common.SchedulerTriggerAPI
           :<|> Common.UpdateOnboardingVehicleVariantMappingAPI
           :<|> Common.UpsertSpecialLocationAPIT
           :<|> Common.DeleteSpecialLocationAPI
           :<|> Common.UpsertSpecialLocationGateAPIT
           :<|> Common.DeleteSpecialLocationGateAPI
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  merchantUpdate merchantId city
    :<|> merchantCommonConfig merchantId city
    :<|> merchantCommonConfigUpdate merchantId city
    :<|> driverPoolConfig merchantId city
    :<|> driverPoolConfigUpdate merchantId city
    :<|> driverPoolConfigCreate merchantId city
    :<|> driverIntelligentPoolConfig merchantId city
    :<|> driverIntelligentPoolConfigUpdate merchantId city
    :<|> documentVerificationConfig merchantId city
    :<|> documentVerificationConfigUpdate merchantId city
    :<|> documentVerificationConfigCreate merchantId city
    :<|> serviceUsageConfig merchantId city
    :<|> mapsServiceConfigUpdate merchantId city
    :<|> mapsServiceUsageConfigUpdate merchantId city
    :<|> smsServiceConfigUpdate merchantId city
    :<|> smsServiceUsageConfigUpdate merchantId city
    :<|> verificationServiceConfigUpdate merchantId city
    :<|> createFPDriverExtraFee merchantId city
    :<|> updateFPDriverExtraFee merchantId city
    :<|> updateFPPerExtraKmRate merchantId city
    :<|> updateFarePolicy merchantId city
    :<|> createMerchantOperatingCity merchantId city
    :<|> schedulerTrigger merchantId city
    :<|> updateOnboardingVehicleVariantMapping merchantId city
    :<|> upsertSpecialLocation merchantId city
    :<|> deleteSpecialLocation merchantId city
    :<|> upsertSpecialLocationGate merchantId city
    :<|> deleteSpecialLocationGate merchantId city

merchantUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MerchantUpdateReq ->
  FlowHandler Common.MerchantUpdateRes
merchantUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.merchantUpdate merchantShortId opCity

merchantCommonConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  FlowHandler Common.MerchantCommonConfigRes
merchantCommonConfig merchantShortId = withFlowHandlerAPI . DMerchant.merchantCommonConfig merchantShortId

merchantCommonConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MerchantCommonConfigUpdateReq ->
  FlowHandler APISuccess
merchantCommonConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.merchantCommonConfigUpdate merchantShortId opCity

schedulerTrigger ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SchedulerTriggerReq ->
  FlowHandler APISuccess
schedulerTrigger merchantShortId opCity req = withFlowHandlerAPI $ DMerchant.schedulerTrigger merchantShortId opCity req

driverPoolConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Meters ->
  FlowHandler Common.DriverPoolConfigRes
driverPoolConfig merchantShortId opCity = withFlowHandlerAPI . DMerchant.driverPoolConfig merchantShortId opCity

driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Meters ->
  SL.Area ->
  Maybe Common.Variant ->
  Maybe Text ->
  Common.DriverPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverPoolConfigUpdate merchantShortId opCity tripDistance area variant tripCategory = withFlowHandlerAPI . DMerchant.driverPoolConfigUpdate merchantShortId opCity tripDistance area variant tripCategory

driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Meters ->
  SL.Area ->
  Maybe Common.Variant ->
  Maybe Text ->
  Common.DriverPoolConfigCreateReq ->
  FlowHandler APISuccess
driverPoolConfigCreate merchantShortId opCity tripDistance area variant tripCategory = withFlowHandlerAPI . DMerchant.driverPoolConfigCreate merchantShortId opCity tripDistance area variant tripCategory

driverIntelligentPoolConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  FlowHandler Common.DriverIntelligentPoolConfigRes
driverIntelligentPoolConfig merchantShortId = withFlowHandlerAPI . DMerchant.driverIntelligentPoolConfig merchantShortId

driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverIntelligentPoolConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.driverIntelligentPoolConfigUpdate merchantShortId opCity

documentVerificationConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Common.DocumentType ->
  Maybe Common.Category ->
  FlowHandler Common.DocumentVerificationConfigRes
documentVerificationConfig merchantShortId opCity category = withFlowHandlerAPI . DMerchant.documentVerificationConfig merchantShortId opCity category

documentVerificationConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.DocumentType ->
  Common.Category ->
  Common.DocumentVerificationConfigUpdateReq ->
  FlowHandler APISuccess
documentVerificationConfigUpdate merchantShortId opCity documentType category = withFlowHandlerAPI . DMerchant.documentVerificationConfigUpdate merchantShortId opCity documentType category

documentVerificationConfigCreate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.DocumentType ->
  Common.Category ->
  Common.DocumentVerificationConfigCreateReq ->
  FlowHandler APISuccess
documentVerificationConfigCreate merchantShortId opCity documentType category = withFlowHandlerAPI . DMerchant.documentVerificationConfigCreate merchantShortId opCity documentType category

serviceUsageConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId = withFlowHandlerAPI . DMerchant.serviceUsageConfig merchantShortId

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.mapsServiceConfigUpdate merchantShortId opCity

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.mapsServiceUsageConfigUpdate merchantShortId opCity

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.smsServiceConfigUpdate merchantShortId opCity

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.smsServiceUsageConfigUpdate merchantShortId opCity

verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.VerificationServiceConfigUpdateReq ->
  FlowHandler APISuccess
verificationServiceConfigUpdate merchantShortId opCity = withFlowHandlerAPI . DMerchant.verificationServiceConfigUpdate merchantShortId opCity

createFPDriverExtraFee :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
createFPDriverExtraFee merchantShortId opCity farePolicyId startDistance req = withFlowHandlerAPI $ DMerchant.createFPDriverExtraFee merchantShortId opCity (cast farePolicyId) startDistance req

updateFPDriverExtraFee :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Meters -> Common.CreateFPDriverExtraFeeReq -> FlowHandler APISuccess
updateFPDriverExtraFee merchantShortId opCity farePolicyId startDistance req = withFlowHandlerAPI $ DMerchant.updateFPDriverExtraFee merchantShortId opCity (cast farePolicyId) startDistance req

updateFPPerExtraKmRate :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Meters -> Common.UpdateFPPerExtraKmRateReq -> FlowHandler APISuccess
updateFPPerExtraKmRate merchantShortId opCity farePolicyId startDistance req = withFlowHandlerAPI $ DMerchant.updateFPPerExtraKmRate merchantShortId opCity (cast farePolicyId) startDistance req

updateFarePolicy :: ShortId DM.Merchant -> Context.City -> Id Common.FarePolicy -> Common.UpdateFarePolicyReq -> FlowHandler APISuccess
updateFarePolicy merchantShortId opCity farePolicyId req = withFlowHandlerAPI $ DMerchant.updateFarePolicy merchantShortId opCity (cast farePolicyId) req

createMerchantOperatingCity :: ShortId DM.Merchant -> Context.City -> Common.CreateMerchantOperatingCityReqT -> FlowHandler Common.CreateMerchantOperatingCityRes
createMerchantOperatingCity merchantShortId opCity req = withFlowHandlerAPI $ DMerchant.createMerchantOperatingCity merchantShortId opCity req

updateOnboardingVehicleVariantMapping :: ShortId DM.Merchant -> Context.City -> Common.UpdateOnboardingVehicleVariantMappingReq -> FlowHandler APISuccess
updateOnboardingVehicleVariantMapping merchantShortId opCity req = withFlowHandlerAPI $ DMerchant.updateOnboardingVehicleVariantMapping merchantShortId opCity req

upsertSpecialLocation :: ShortId DM.Merchant -> Context.City -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReqT -> FlowHandler APISuccess
upsertSpecialLocation merchantShortId opCity mbSpecialLocationId = withFlowHandlerAPI . DMerchant.upsertSpecialLocation merchantShortId opCity mbSpecialLocationId

deleteSpecialLocation :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> FlowHandler APISuccess
deleteSpecialLocation merchantShortId opCity = withFlowHandlerAPI . DMerchant.deleteSpecialLocation merchantShortId opCity

upsertSpecialLocationGate :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> FlowHandler APISuccess
upsertSpecialLocationGate merchantShortId opCity specialLocationId req = withFlowHandlerAPI $ DMerchant.upsertSpecialLocationGate merchantShortId opCity specialLocationId req

deleteSpecialLocationGate :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Text -> FlowHandler APISuccess
deleteSpecialLocationGate merchantShortId opCity specialLocationId gateName = withFlowHandlerAPI $ DMerchant.deleteSpecialLocationGate merchantShortId opCity specialLocationId gateName
