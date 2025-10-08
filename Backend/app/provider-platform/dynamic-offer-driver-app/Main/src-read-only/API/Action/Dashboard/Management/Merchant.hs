{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Merchant
  ( API.Types.ProviderPlatform.Management.Merchant.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Merchant
import qualified Dashboard.Common
import qualified Dashboard.Common.Merchant
import qualified Domain.Action.Dashboard.Management.Merchant
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Merchant.API)
handler merchantId city = postMerchantUpdate merchantId city :<|> getMerchantConfigCommon merchantId city :<|> postMerchantConfigCommonUpdate merchantId city :<|> getMerchantConfigDriverPool merchantId city :<|> postMerchantConfigDriverPoolUpdate merchantId city :<|> postMerchantConfigDriverPoolCreate merchantId city :<|> getMerchantConfigDriverIntelligentPool merchantId city :<|> postMerchantConfigDriverIntelligentPoolUpdate merchantId city :<|> getMerchantConfigOnboardingDocument merchantId city :<|> postMerchantConfigOnboardingDocumentUpdate merchantId city :<|> postMerchantConfigOnboardingDocumentCreate merchantId city :<|> getMerchantServiceUsageConfig merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city :<|> postMerchantServiceUsageConfigMapsUpdate merchantId city :<|> postMerchantServiceConfigSmsUpdate merchantId city :<|> postMerchantServiceUsageConfigSmsUpdate merchantId city :<|> postMerchantServiceConfigVerificationUpdate merchantId city :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate merchantId city :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate merchantId city :<|> postMerchantConfigFarePolicyPerExtraKmRateUpdate merchantId city :<|> postMerchantConfigFarePolicyUpdate merchantId city :<|> postMerchantConfigFarePolicyUpsert merchantId city :<|> postMerchantConfigOperatingCityCreate merchantId city :<|> postMerchantSchedulerTrigger merchantId city :<|> postMerchantUpdateOnboardingVehicleVariantMapping merchantId city :<|> postMerchantVehicleServiceTierUpsert merchantId city :<|> postMerchantConfigSpecialLocationUpsert merchantId city :<|> postMerchantSpecialLocationUpsert merchantId city :<|> deleteMerchantSpecialLocationDelete merchantId city :<|> postMerchantSpecialLocationGatesUpsert merchantId city :<|> deleteMerchantSpecialLocationGatesDelete merchantId city :<|> postMerchantConfigClearCacheSubscription merchantId city :<|> postMerchantConfigUpsertPlanAndConfigSubscription merchantId city :<|> postMerchantConfigFailover merchantId city :<|> postMerchantPayoutConfigUpdate merchantId city :<|> postMerchantConfigOperatingCityWhiteList merchantId city :<|> postMerchantConfigMerchantCreate merchantId city

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes)
postMerchantUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantUpdate a3 a2 a1

getMerchantConfigCommon :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes)
getMerchantConfigCommon a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigCommon a2 a1

postMerchantConfigCommonUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigCommonUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigCommonUpdate a3 a2 a1

getMerchantConfigDriverPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes)
getMerchantConfigDriverPool a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigDriverPool a5 a4 a3 a2 a1

postMerchantConfigDriverPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolUpdate a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDriverPoolUpdate a9 a8 a7 a6 a5 a4 a3 a2 a1

postMerchantConfigDriverPoolCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolCreate a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDriverPoolCreate a9 a8 a7 a6 a5 a4 a3 a2 a1

getMerchantConfigDriverIntelligentPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes)
getMerchantConfigDriverIntelligentPool a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigDriverIntelligentPool a2 a1

postMerchantConfigDriverIntelligentPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverIntelligentPoolUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDriverIntelligentPoolUpdate a3 a2 a1

getMerchantConfigOnboardingDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Kernel.Prelude.Maybe Dashboard.Common.VehicleCategory -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes)
getMerchantConfigOnboardingDocument a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigOnboardingDocument a4 a3 a2 a1

postMerchantConfigOnboardingDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Dashboard.Common.VehicleCategory -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentUpdate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOnboardingDocumentUpdate a5 a4 a3 a2 a1

postMerchantConfigOnboardingDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Dashboard.Common.VehicleCategory -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentCreate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOnboardingDocumentCreate a5 a4 a3 a2 a1

getMerchantServiceUsageConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Dashboard.Common.Merchant.ServiceUsageConfigRes)
getMerchantServiceUsageConfig a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantServiceUsageConfig a2 a1

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceConfigMapsUpdate a3 a2 a1

postMerchantServiceUsageConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigMapsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceUsageConfigMapsUpdate a3 a2 a1

postMerchantServiceConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigSmsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceConfigSmsUpdate a3 a2 a1

postMerchantServiceUsageConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigSmsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceUsageConfigSmsUpdate a3 a2 a1

postMerchantServiceConfigVerificationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.VerificationServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigVerificationUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceConfigVerificationUpdate a3 a2 a1

postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate a7 a6 a5 a4 a3 a2 a1

postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate a7 a6 a5 a4 a3 a2 a1

postMerchantConfigFarePolicyPerExtraKmRateUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyPerExtraKmRateUpdate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyPerExtraKmRateUpdate a5 a4 a3 a2 a1

postMerchantConfigFarePolicyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyUpdate a4 a3 a2 a1

postMerchantConfigFarePolicyUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyResp)
postMerchantConfigFarePolicyUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyUpsert a3 a2 a1

postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOperatingCityCreate a3 a2 a1

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantSchedulerTrigger a3 a2 a1

postMerchantUpdateOnboardingVehicleVariantMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.UpdateOnboardingVehicleVariantMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdateOnboardingVehicleVariantMapping a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantUpdateOnboardingVehicleVariantMapping a3 a2 a1

postMerchantVehicleServiceTierUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.UpdateVehicleServiceTierReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantVehicleServiceTierUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantVehicleServiceTierUpsert a3 a2 a1

postMerchantConfigSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigSpecialLocationUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigSpecialLocationUpsert a3 a2 a1

postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantSpecialLocationUpsert a4 a3 a2 a1

deleteMerchantSpecialLocationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.deleteMerchantSpecialLocationDelete a3 a2 a1

postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantSpecialLocationGatesUpsert a4 a3 a2 a1

deleteMerchantSpecialLocationGatesDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationGatesDelete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.deleteMerchantSpecialLocationGatesDelete a4 a3 a2 a1

postMerchantConfigClearCacheSubscription :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.ClearCacheSubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigClearCacheSubscription a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigClearCacheSubscription a3 a2 a1

postMerchantConfigUpsertPlanAndConfigSubscription :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.UpsertPlanAndConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.UpsertPlanAndConfigResp)
postMerchantConfigUpsertPlanAndConfigSubscription a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigUpsertPlanAndConfigSubscription a3 a2 a1

postMerchantConfigFailover :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFailover a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFailover a4 a3 a2 a1

postMerchantPayoutConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.PayoutConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantPayoutConfigUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantPayoutConfigUpdate a3 a2 a1

postMerchantConfigOperatingCityWhiteList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.WhiteListOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.WhiteListOperatingCityRes)
postMerchantConfigOperatingCityWhiteList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOperatingCityWhiteList a3 a2 a1

postMerchantConfigMerchantCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigMerchantCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOperatingCityCreate a3 a2 a1
