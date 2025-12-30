{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Merchant
  ( API.Types.RiderPlatform.Management.Merchant.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Dashboard.Common.Merchant
import qualified Domain.Action.Dashboard.Merchant
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Merchant.API)
handler merchantId city = postMerchantUpdate merchantId city :<|> getMerchantServiceUsageConfig merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city :<|> postMerchantServiceUsageConfigMapsUpdate merchantId city :<|> postMerchantServiceConfigSmsUpdate merchantId city :<|> postMerchantServiceUsageConfigSmsUpdate merchantId city :<|> postMerchantConfigOperatingCityCreate merchantId city :<|> postMerchantConfigSpecialLocationUpsert merchantId city :<|> postMerchantSpecialLocationUpsert merchantId city :<|> deleteMerchantSpecialLocationDelete merchantId city :<|> postMerchantSpecialLocationGatesUpsert merchantId city :<|> deleteMerchantSpecialLocationGatesDelete merchantId city :<|> postMerchantSpecialLocationList merchantId city :<|> postMerchantConfigFailover merchantId city :<|> postMerchantTicketConfigUpsert merchantId city :<|> postMerchantSchedulerTrigger merchantId city :<|> postMerchantConfigOperatingCityWhiteList merchantId city :<|> postMerchantConfigMerchantCreate merchantId city

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantUpdate a3 a2 a1

getMerchantServiceUsageConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Dashboard.Common.Merchant.ServiceUsageConfigRes)
getMerchantServiceUsageConfig a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantServiceUsageConfig a2 a1

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantServiceConfigMapsUpdate a3 a2 a1

postMerchantServiceUsageConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigMapsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantServiceUsageConfigMapsUpdate a3 a2 a1

postMerchantServiceConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigSmsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantServiceConfigSmsUpdate a3 a2 a1

postMerchantServiceUsageConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigSmsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantServiceUsageConfigSmsUpdate a3 a2 a1

postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigOperatingCityCreate a3 a2 a1

postMerchantConfigSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigSpecialLocationUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigSpecialLocationUpsert a3 a2 a1

postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantSpecialLocationUpsert a4 a3 a2 a1

deleteMerchantSpecialLocationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.deleteMerchantSpecialLocationDelete a3 a2 a1

postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantSpecialLocationGatesUpsert a4 a3 a2 a1

deleteMerchantSpecialLocationGatesDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationGatesDelete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.deleteMerchantSpecialLocationGatesDelete a4 a3 a2 a1

postMerchantSpecialLocationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Merchant.GetSpecialLocationListReq -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Merchant.SpecialLocationListRes])
postMerchantSpecialLocationList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantSpecialLocationList a3 a2 a1

postMerchantConfigFailover :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFailover a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigFailover a4 a3 a2 a1

postMerchantTicketConfigUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Merchant.UpsertTicketConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.UpsertTicketConfigResp)
postMerchantTicketConfigUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantTicketConfigUpsert a3 a2 a1

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Merchant.SchedulerTriggerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantSchedulerTrigger a3 a2 a1

postMerchantConfigOperatingCityWhiteList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.WhiteListOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.WhiteListOperatingCityRes)
postMerchantConfigOperatingCityWhiteList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigOperatingCityWhiteList a3 a2 a1

postMerchantConfigMerchantCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigMerchantCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigOperatingCityCreate a3 a2 a1
