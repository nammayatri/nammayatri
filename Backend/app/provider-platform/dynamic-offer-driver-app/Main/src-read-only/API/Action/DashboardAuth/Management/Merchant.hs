{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Merchant
  ( API,
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
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import qualified Lib.Yudhishthira.Tools.DebugLog
import Servant
import qualified Toll.Domain.Types.Toll
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantConfigCommon :<|> PostMerchantConfigCommonUpdate :<|> GetMerchantConfigDriverPool :<|> PostMerchantConfigDriverPoolUpdate :<|> PostMerchantConfigDriverPoolCreate :<|> PostMerchantConfigDriverPoolUpsert :<|> GetMerchantConfigDriverPoolList :<|> GetMerchantConfigDriverIntelligentPool :<|> PostMerchantConfigDriverIntelligentPoolUpdate :<|> GetMerchantConfigOnboardingDocument :<|> PostMerchantConfigOnboardingDocumentUpdate :<|> PostMerchantConfigOnboardingDocumentCreate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantServiceConfigVerificationUpdate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :<|> PostMerchantConfigFarePolicyPerExtraKmRateUpdate :<|> PostMerchantConfigFarePolicyUpdate :<|> PostMerchantConfigFarePolicyUpsert :<|> GetMerchantConfigFarePolicyExport :<|> GetMerchantConfigFarePolicyDetails :<|> GetMerchantConfigFareProductList :<|> PostMerchantConfigFareProductSetEnabled :<|> PostMerchantConfigOperatingCityCreateHelper :<|> PostMerchantSchedulerTrigger :<|> PostMerchantUpdateOnboardingVehicleVariantMapping :<|> PostMerchantConfigSpecialLocationUpsert :<|> GetMerchantConfigSpecialLocationList :<|> GetMerchantConfigGeometryList :<|> PutMerchantConfigGeometryUpdate :<|> PostMerchantSpecialLocationUpsertHelper :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsertHelper :<|> DeleteMerchantSpecialLocationGatesDelete :<|> PostMerchantConfigTollUpsert :<|> GetMerchantConfigTollList :<|> PostMerchantTollUpsert :<|> DeleteMerchantTollDelete :<|> PostMerchantConfigClearCacheSubscription :<|> PostMerchantConfigUpsertPlanAndConfigSubscription :<|> GetMerchantConfigVendorSplitDetailsList :<|> GetMerchantConfigSubscriptionConfigList :<|> PostMerchantConfigFailover :<|> PostMerchantPayoutConfigUpdate :<|> PostMerchantConfigOperatingCityWhiteList :<|> PostMerchantConfigMerchantCreateHelper :<|> GetMerchantConfigVehicleServiceTier :<|> GetMerchantConfigVehicleServiceTierList :<|> PostMerchantConfigVehicleServiceTierUpdate :<|> PostMerchantConfigVehicleServiceTierCreate :<|> PostMerchantConfigDebugLogUpdate :<|> GetMerchantMerchantDocumentList :<|> GetMerchantMerchantDocument :<|> PostMerchantMerchantDocumentCreate :<|> PostMerchantMerchantDocumentUpdate :<|> PostMerchantMerchantDocumentDelete :<|> GetMerchantMerchantMessageCatalog :<|> PostMerchantMerchantMessageUpsert :<|> DeleteMerchantMerchantMessage :<|> GetMerchantCityList))

type PostMerchantUpdate = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE" :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantUpdate)

type GetMerchantConfigCommon =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_COMMON"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigCommon
  )

type PostMerchantConfigCommonUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_COMMON_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigCommonUpdate
  )

type GetMerchantConfigDriverPool =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_POOL"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigDriverPool
  )

type PostMerchantConfigDriverPoolUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigDriverPoolUpdate
  )

type PostMerchantConfigDriverPoolCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_CREATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigDriverPoolCreate
  )

type PostMerchantConfigDriverPoolUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigDriverPoolUpsert
  )

type GetMerchantConfigDriverPoolList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_POOL_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigDriverPoolList
  )

type GetMerchantConfigDriverIntelligentPool =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigDriverIntelligentPool
  )

type PostMerchantConfigDriverIntelligentPoolUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigDriverIntelligentPoolUpdate
  )

type GetMerchantConfigOnboardingDocument =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_ONBOARDING_DOCUMENT"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigOnboardingDocument
  )

type PostMerchantConfigOnboardingDocumentUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigOnboardingDocumentUpdate
  )

type PostMerchantConfigOnboardingDocumentCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_CREATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigOnboardingDocumentCreate
  )

type GetMerchantServiceUsageConfig =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_SERVICE_USAGE_CONFIG"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantServiceUsageConfig
  )

type PostMerchantServiceConfigMapsUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantServiceConfigMapsUpdate
  )

type PostMerchantServiceUsageConfigMapsUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantServiceUsageConfigMapsUpdate
  )

type PostMerchantServiceConfigSmsUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantServiceConfigSmsUpdate
  )

type PostMerchantServiceUsageConfigSmsUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantServiceUsageConfigSmsUpdate
  )

type PostMerchantServiceConfigVerificationUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_VERIFICATION_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantServiceConfigVerificationUpdate
  )

type PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_CREATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate
  )

type PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate
  )

type PostMerchantConfigFarePolicyPerExtraKmRateUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_PER_EXTRA_KM_RATE_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigFarePolicyPerExtraKmRateUpdate
  )

type PostMerchantConfigFarePolicyUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigFarePolicyUpdate
  )

type PostMerchantConfigFarePolicyUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigFarePolicyUpsert
  )

type GetMerchantConfigFarePolicyExport =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_FARE_POLICY_EXPORT"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigFarePolicyExport
  )

type GetMerchantConfigFarePolicyDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_FARE_POLICY_DETAILS"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigFarePolicyDetails
  )

type GetMerchantConfigFareProductList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_FARE_PRODUCT_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigFareProductList
  )

type PostMerchantConfigFareProductSetEnabled =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_PRODUCT_SET_ENABLED"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigFareProductSetEnabled
  )

type PostMerchantConfigOperatingCityCreateHelper =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigOperatingCityCreateHelper
  )

type PostMerchantSchedulerTrigger =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SCHEDULER_TRIGGER"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantSchedulerTrigger
  )

type PostMerchantUpdateOnboardingVehicleVariantMapping =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantUpdateOnboardingVehicleVariantMapping
  )

type PostMerchantConfigSpecialLocationUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigSpecialLocationUpsert
  )

type GetMerchantConfigSpecialLocationList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_SPECIAL_LOCATION_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigSpecialLocationList
  )

type GetMerchantConfigGeometryList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_GEOMETRY_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigGeometryList
  )

type PutMerchantConfigGeometryUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PutMerchantConfigGeometryUpdate
  )

type PostMerchantSpecialLocationUpsertHelper =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantSpecialLocationUpsertHelper
  )

type DeleteMerchantSpecialLocationDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE"
      :> API.Types.ProviderPlatform.Management.Merchant.DeleteMerchantSpecialLocationDelete
  )

type PostMerchantSpecialLocationGatesUpsertHelper =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantSpecialLocationGatesUpsertHelper
  )

type DeleteMerchantSpecialLocationGatesDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE"
      :> API.Types.ProviderPlatform.Management.Merchant.DeleteMerchantSpecialLocationGatesDelete
  )

type PostMerchantConfigTollUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_TOLL_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigTollUpsert
  )

type GetMerchantConfigTollList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_TOLL_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigTollList
  )

type PostMerchantTollUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TOLL_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantTollUpsert
  )

type DeleteMerchantTollDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_TOLL_DELETE"
      :> API.Types.ProviderPlatform.Management.Merchant.DeleteMerchantTollDelete
  )

type PostMerchantConfigClearCacheSubscription =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_CLEAR_CACHE_SUBSCRIPTION"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigClearCacheSubscription
  )

type PostMerchantConfigUpsertPlanAndConfigSubscription =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_UPSERT_PLAN_AND_CONFIG_SUBSCRIPTION"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigUpsertPlanAndConfigSubscription
  )

type GetMerchantConfigVendorSplitDetailsList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_VENDOR_SPLIT_DETAILS_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigVendorSplitDetailsList
  )

type GetMerchantConfigSubscriptionConfigList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_SUBSCRIPTION_CONFIG_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigSubscriptionConfigList
  )

type PostMerchantConfigFailover =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigFailover
  )

type PostMerchantPayoutConfigUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_PAYOUT_CONFIG_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantPayoutConfigUpdate
  )

type PostMerchantConfigOperatingCityWhiteList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigOperatingCityWhiteList
  )

type PostMerchantConfigMerchantCreateHelper =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_MERCHANT_CREATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigMerchantCreateHelper
  )

type GetMerchantConfigVehicleServiceTier =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigVehicleServiceTier
  )

type GetMerchantConfigVehicleServiceTierList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantConfigVehicleServiceTierList
  )

type PostMerchantConfigVehicleServiceTierUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigVehicleServiceTierUpdate
  )

type PostMerchantConfigVehicleServiceTierCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER_CREATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigVehicleServiceTierCreate
  )

type PostMerchantConfigDebugLogUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DEBUG_LOG_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantConfigDebugLogUpdate
  )

type GetMerchantMerchantDocumentList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_DOCUMENT_LIST"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantMerchantDocumentList
  )

type GetMerchantMerchantDocument = API.Types.ProviderPlatform.Management.Merchant.GetMerchantMerchantDocument

type PostMerchantMerchantDocumentCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_CREATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantMerchantDocumentCreate
  )

type PostMerchantMerchantDocumentUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_UPDATE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantMerchantDocumentUpdate
  )

type PostMerchantMerchantDocumentDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_DELETE"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantMerchantDocumentDelete
  )

type GetMerchantMerchantMessageCatalog =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_MESSAGE_CATALOG"
      :> API.Types.ProviderPlatform.Management.Merchant.GetMerchantMerchantMessageCatalog
  )

type PostMerchantMerchantMessageUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_MESSAGE_UPSERT"
      :> API.Types.ProviderPlatform.Management.Merchant.PostMerchantMerchantMessageUpsert
  )

type DeleteMerchantMerchantMessage =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_MERCHANT_MESSAGE"
      :> API.Types.ProviderPlatform.Management.Merchant.DeleteMerchantMerchantMessage
  )

type GetMerchantCityList = API.Types.ProviderPlatform.Management.Merchant.GetMerchantCityList

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMerchantUpdate merchantId city :<|> getMerchantConfigCommon merchantId city :<|> postMerchantConfigCommonUpdate merchantId city :<|> getMerchantConfigDriverPool merchantId city :<|> postMerchantConfigDriverPoolUpdate merchantId city :<|> postMerchantConfigDriverPoolCreate merchantId city :<|> postMerchantConfigDriverPoolUpsert merchantId city :<|> getMerchantConfigDriverPoolList merchantId city :<|> getMerchantConfigDriverIntelligentPool merchantId city :<|> postMerchantConfigDriverIntelligentPoolUpdate merchantId city :<|> getMerchantConfigOnboardingDocument merchantId city :<|> postMerchantConfigOnboardingDocumentUpdate merchantId city :<|> postMerchantConfigOnboardingDocumentCreate merchantId city :<|> getMerchantServiceUsageConfig merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city :<|> postMerchantServiceUsageConfigMapsUpdate merchantId city :<|> postMerchantServiceConfigSmsUpdate merchantId city :<|> postMerchantServiceUsageConfigSmsUpdate merchantId city :<|> postMerchantServiceConfigVerificationUpdate merchantId city :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate merchantId city :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate merchantId city :<|> postMerchantConfigFarePolicyPerExtraKmRateUpdate merchantId city :<|> postMerchantConfigFarePolicyUpdate merchantId city :<|> postMerchantConfigFarePolicyUpsert merchantId city :<|> getMerchantConfigFarePolicyExport merchantId city :<|> getMerchantConfigFarePolicyDetails merchantId city :<|> getMerchantConfigFareProductList merchantId city :<|> postMerchantConfigFareProductSetEnabled merchantId city :<|> postMerchantConfigOperatingCityCreate merchantId city :<|> postMerchantSchedulerTrigger merchantId city :<|> postMerchantUpdateOnboardingVehicleVariantMapping merchantId city :<|> postMerchantConfigSpecialLocationUpsert merchantId city :<|> getMerchantConfigSpecialLocationList merchantId city :<|> getMerchantConfigGeometryList merchantId city :<|> putMerchantConfigGeometryUpdate merchantId city :<|> postMerchantSpecialLocationUpsert merchantId city :<|> deleteMerchantSpecialLocationDelete merchantId city :<|> postMerchantSpecialLocationGatesUpsert merchantId city :<|> deleteMerchantSpecialLocationGatesDelete merchantId city :<|> postMerchantConfigTollUpsert merchantId city :<|> getMerchantConfigTollList merchantId city :<|> postMerchantTollUpsert merchantId city :<|> deleteMerchantTollDelete merchantId city :<|> postMerchantConfigClearCacheSubscription merchantId city :<|> postMerchantConfigUpsertPlanAndConfigSubscription merchantId city :<|> getMerchantConfigVendorSplitDetailsList merchantId city :<|> getMerchantConfigSubscriptionConfigList merchantId city :<|> postMerchantConfigFailover merchantId city :<|> postMerchantPayoutConfigUpdate merchantId city :<|> postMerchantConfigOperatingCityWhiteList merchantId city :<|> postMerchantConfigMerchantCreate merchantId city :<|> getMerchantConfigVehicleServiceTier merchantId city :<|> getMerchantConfigVehicleServiceTierList merchantId city :<|> postMerchantConfigVehicleServiceTierUpdate merchantId city :<|> postMerchantConfigVehicleServiceTierCreate merchantId city :<|> postMerchantConfigDebugLogUpdate merchantId city :<|> getMerchantMerchantDocumentList merchantId city :<|> getMerchantMerchantDocument merchantId city :<|> postMerchantMerchantDocumentCreate merchantId city :<|> postMerchantMerchantDocumentUpdate merchantId city :<|> postMerchantMerchantDocumentDelete merchantId city :<|> getMerchantMerchantMessageCatalog merchantId city :<|> postMerchantMerchantMessageUpsert merchantId city :<|> deleteMerchantMerchantMessage merchantId city :<|> getMerchantCityList merchantId city

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes)
postMerchantUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantUpdate a4 a3 a1

getMerchantConfigCommon :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes)
getMerchantConfigCommon a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigCommon a3 a2

postMerchantConfigCommonUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigCommonUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigCommonUpdate a4 a3 a1

getMerchantConfigDriverPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Common.Meters) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes)
getMerchantConfigDriverPool a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigDriverPool a6 a5 a3 a2 a1

postMerchantConfigDriverPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Dashboard.Common.VehicleVariant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolUpdate a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDriverPoolUpdate a10 a9 a7 a6 a5 a4 a3 a2 a1

postMerchantConfigDriverPoolCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Dashboard.Common.VehicleVariant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolCreate a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDriverPoolCreate a10 a9 a7 a6 a5 a4 a3 a2 a1

postMerchantConfigDriverPoolUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.UpsertDriverPoolConfigCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigDriverPoolUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDriverPoolUpsert a4 a3 a1

getMerchantConfigDriverPoolList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigListRes)
getMerchantConfigDriverPoolList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigDriverPoolList a3 a2

getMerchantConfigDriverIntelligentPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes)
getMerchantConfigDriverIntelligentPool a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigDriverIntelligentPool a3 a2

postMerchantConfigDriverIntelligentPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverIntelligentPoolUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDriverIntelligentPoolUpdate a4 a3 a1

getMerchantConfigOnboardingDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Merchant.DocumentType) -> Kernel.Prelude.Maybe (Dashboard.Common.VehicleCategory) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes)
getMerchantConfigOnboardingDocument a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigOnboardingDocument a5 a4 a2 a1

postMerchantConfigOnboardingDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Dashboard.Common.VehicleCategory -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentUpdate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOnboardingDocumentUpdate a6 a5 a3 a2 a1

postMerchantConfigOnboardingDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Dashboard.Common.VehicleCategory -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentCreate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOnboardingDocumentCreate a6 a5 a3 a2 a1

getMerchantServiceUsageConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler Dashboard.Common.Merchant.ServiceUsageConfigRes)
getMerchantServiceUsageConfig a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantServiceUsageConfig a3 a2

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceConfigMapsUpdate a4 a3 a1

postMerchantServiceUsageConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigMapsUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceUsageConfigMapsUpdate a4 a3 a1

postMerchantServiceConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigSmsUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceConfigSmsUpdate a4 a3 a1

postMerchantServiceUsageConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigSmsUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceUsageConfigSmsUpdate a4 a3 a1

postMerchantServiceConfigVerificationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.VerificationServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigVerificationUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantServiceConfigVerificationUpdate a4 a3 a1

postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate a8 a7 a5 a4 a3 a2 a1

postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate a8 a7 a5 a4 a3 a2 a1

postMerchantConfigFarePolicyPerExtraKmRateUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyPerExtraKmRateUpdate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyPerExtraKmRateUpdate a6 a5 a3 a2 a1

postMerchantConfigFarePolicyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyUpdate a5 a4 a2 a1

postMerchantConfigFarePolicyUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyResp)
postMerchantConfigFarePolicyUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFarePolicyUpsert a4 a3 a1

getMerchantConfigFarePolicyExport :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler Kernel.Prelude.Text)
getMerchantConfigFarePolicyExport a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigFarePolicyExport a3 a2

getMerchantConfigFarePolicyDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.FarePolicyDetailsResp)
getMerchantConfigFarePolicyDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigFarePolicyDetails a4 a3 a1

getMerchantConfigFareProductList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Types.SpecialLocation.Area -> Kernel.Prelude.Bool -> Dashboard.Common.TripCategory -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.FareProductListRes)
getMerchantConfigFareProductList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigFareProductList a6 a5 a3 a2 a1

postMerchantConfigFareProductSetEnabled :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.SetFareProductEnabledReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFareProductSetEnabled a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFareProductSetEnabled a4 a3 a1

postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOperatingCityCreate a4 a3 a1

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantSchedulerTrigger a4 a3 a1

postMerchantUpdateOnboardingVehicleVariantMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.UpdateOnboardingVehicleVariantMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdateOnboardingVehicleVariantMapping a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantUpdateOnboardingVehicleVariantMapping a4 a3 a1

postMerchantConfigSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigSpecialLocationUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigSpecialLocationUpsert a4 a3 a1

getMerchantConfigSpecialLocationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Types.SpecialLocation.SpecialLocationType) -> Kernel.Prelude.Maybe ([Lib.Types.SpecialLocation.SpecialLocationType]) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.SpecialLocationResp)
getMerchantConfigSpecialLocationList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigSpecialLocationList a7 a6 a4 a3 a2 a1

getMerchantConfigGeometryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.GeometryResp)
getMerchantConfigGeometryList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigGeometryList a5 a4 a2 a1

putMerchantConfigGeometryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.UpdateGeometryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putMerchantConfigGeometryUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.putMerchantConfigGeometryUpdate a4 a3 a1

postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantSpecialLocationUpsert a5 a4 a2 a1

deleteMerchantSpecialLocationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.deleteMerchantSpecialLocationDelete a4 a3 a1

postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantSpecialLocationGatesUpsert a5 a4 a2 a1

deleteMerchantSpecialLocationGatesDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationGatesDelete a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.deleteMerchantSpecialLocationGatesDelete a5 a4 a2 a1

postMerchantConfigTollUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.UpsertTollCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigTollUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigTollUpsert a4 a3 a1

getMerchantConfigTollList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.TollListResp)
getMerchantConfigTollList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigTollList a5 a4 a2 a1

postMerchantTollUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll) -> Dashboard.Common.Merchant.UpsertTollReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantTollUpsert a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantTollUpsert a5 a4 a2 a1

deleteMerchantTollDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantTollDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.deleteMerchantTollDelete a4 a3 a1

postMerchantConfigClearCacheSubscription :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.ClearCacheSubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigClearCacheSubscription a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigClearCacheSubscription a4 a3 a1

postMerchantConfigUpsertPlanAndConfigSubscription :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.UpsertPlanAndConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.UpsertPlanAndConfigResp)
postMerchantConfigUpsertPlanAndConfigSubscription a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigUpsertPlanAndConfigSubscription a4 a3 a1

getMerchantConfigVendorSplitDetailsList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Merchant.VendorSplitDetailsAPIEntity])
getMerchantConfigVendorSplitDetailsList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigVendorSplitDetailsList a3 a2

getMerchantConfigSubscriptionConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Merchant.SubscriptionConfigAPIEntity])
getMerchantConfigSubscriptionConfigList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigSubscriptionConfigList a3 a2

postMerchantConfigFailover :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFailover a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigFailover a5 a4 a2 a1

postMerchantPayoutConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.PayoutConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantPayoutConfigUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantPayoutConfigUpdate a4 a3 a1

postMerchantConfigOperatingCityWhiteList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.WhiteListOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.WhiteListOperatingCityRes)
postMerchantConfigOperatingCityWhiteList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigOperatingCityWhiteList a4 a3 a1

postMerchantConfigMerchantCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigMerchantCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigMerchantCreate a4 a3 a1

getMerchantConfigVehicleServiceTier :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Dashboard.Common.ServiceTierType) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.VehicleServiceTierRes)
getMerchantConfigVehicleServiceTier a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigVehicleServiceTier a4 a3 a1

getMerchantConfigVehicleServiceTierList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.VehicleServiceTierListRes)
getMerchantConfigVehicleServiceTierList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantConfigVehicleServiceTierList a3 a2

postMerchantConfigVehicleServiceTierUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.ServiceTierType -> API.Types.ProviderPlatform.Management.Merchant.VehicleServiceTierConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigVehicleServiceTierUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigVehicleServiceTierUpdate a5 a4 a2 a1

postMerchantConfigVehicleServiceTierCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.VehicleServiceTierConfigCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigVehicleServiceTierCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigVehicleServiceTierCreate a4 a3 a1

postMerchantConfigDebugLogUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Tools.DebugLog.SetJsonLogicDebugReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDebugLogUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantConfigDebugLogUpdate a4 a3 a1

getMerchantMerchantDocumentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> API.Types.ProviderPlatform.Management.Merchant.MerchantDocumentRoleT -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantDocumentListResp)
getMerchantMerchantDocumentList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantMerchantDocumentList a5 a4 a2 a1

getMerchantMerchantDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> API.Types.ProviderPlatform.Management.Merchant.MerchantDocumentRoleT -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantDocumentItem)
getMerchantMerchantDocument a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantMerchantDocument a5 a4 a3 a2 a1

postMerchantMerchantDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.CreateMerchantDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantDocumentItem)
postMerchantMerchantDocumentCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantMerchantDocumentCreate a4 a3 a1

postMerchantMerchantDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.UpdateMerchantDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantDocumentItem)
postMerchantMerchantDocumentUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantMerchantDocumentUpdate a4 a3 a1

postMerchantMerchantDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.DeleteMerchantDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantMerchantDocumentDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantMerchantDocumentDelete a4 a3 a1

getMerchantMerchantMessageCatalog :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.MerchantMessageCatalogType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantMessageCatalogResp)
getMerchantMerchantMessageCatalog a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantMerchantMessageCatalog a4 a3 a1

postMerchantMerchantMessageUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Merchant.UpsertMerchantMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantMerchantMessageUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.postMerchantMerchantMessageUpsert a4 a3 a1

deleteMerchantMerchantMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Dashboard.Common.VehicleCategory) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantMerchantMessage a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.deleteMerchantMerchantMessage a5 a4 a2 a1

getMerchantCityList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.CityListResp)
getMerchantCityList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Merchant.getMerchantCityList a2 a1
