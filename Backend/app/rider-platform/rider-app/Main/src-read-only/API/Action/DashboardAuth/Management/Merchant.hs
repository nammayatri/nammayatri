{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Merchant
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Dashboard.Common.Merchant
import qualified Domain.Action.Dashboard.Merchant
import qualified Domain.Action.DashboardAuth.Management.Merchant
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import qualified Lib.Yudhishthira.Tools.DebugLog
import Servant
import qualified Toll.Domain.Types.Toll
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantConfigOperatingCityCreate :<|> PostMerchantConfigSpecialLocationUpsert :<|> GetMerchantConfigSpecialLocationList :<|> GetMerchantConfigGeometryList :<|> PutMerchantConfigGeometryUpdate :<|> PostMerchantSpecialLocationUpsert :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsert :<|> DeleteMerchantSpecialLocationGatesDelete :<|> PostMerchantConfigTollUpsert :<|> GetMerchantConfigTollList :<|> PostMerchantTollUpsert :<|> DeleteMerchantTollDelete :<|> PostMerchantConfigFailover :<|> PostMerchantTicketConfigUpsert :<|> PostMerchantSchedulerTrigger :<|> PostMerchantConfigOperatingCityWhiteList :<|> PostMerchantConfigMerchantCreate :<|> GetMerchantRiderConfigEstimatesOrder :<|> PostMerchantRiderConfigEstimatesOrderUpdate :<|> PostMerchantConfigDebugLogUpdate :<|> GetMerchantMerchantMessageCatalog :<|> PostMerchantMerchantMessageUpsert :<|> DeleteMerchantMerchantMessage))

type PostMerchantUpdate = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE" :> API.Types.RiderPlatform.Management.Merchant.PostMerchantUpdate)

type GetMerchantServiceUsageConfig =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_SERVICE_USAGE_CONFIG"
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantServiceUsageConfig
  )

type PostMerchantServiceConfigMapsUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceConfigMapsUpdate
  )

type PostMerchantServiceUsageConfigMapsUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceUsageConfigMapsUpdate
  )

type PostMerchantServiceConfigSmsUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceConfigSmsUpdate
  )

type PostMerchantServiceUsageConfigSmsUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceUsageConfigSmsUpdate
  )

type PostMerchantConfigOperatingCityCreate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigOperatingCityCreate
  )

type PostMerchantConfigSpecialLocationUpsert =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigSpecialLocationUpsert
  )

type GetMerchantConfigSpecialLocationList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_SPECIAL_LOCATION_LIST"
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantConfigSpecialLocationList
  )

type GetMerchantConfigGeometryList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_GEOMETRY_LIST"
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantConfigGeometryList
  )

type PutMerchantConfigGeometryUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE"
      :> API.Types.RiderPlatform.Management.Merchant.PutMerchantConfigGeometryUpdate
  )

type PostMerchantSpecialLocationUpsert =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSpecialLocationUpsert
  )

type DeleteMerchantSpecialLocationDelete =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE"
      :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantSpecialLocationDelete
  )

type PostMerchantSpecialLocationGatesUpsert =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSpecialLocationGatesUpsert
  )

type DeleteMerchantSpecialLocationGatesDelete =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE"
      :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantSpecialLocationGatesDelete
  )

type PostMerchantConfigTollUpsert =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_TOLL_UPSERT"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigTollUpsert
  )

type GetMerchantConfigTollList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_TOLL_LIST"
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantConfigTollList
  )

type PostMerchantTollUpsert = (DashboardUserAuth 'APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TOLL_UPSERT" :> API.Types.RiderPlatform.Management.Merchant.PostMerchantTollUpsert)

type DeleteMerchantTollDelete =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_TOLL_DELETE"
      :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantTollDelete
  )

type PostMerchantConfigFailover =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigFailover
  )

type PostMerchantTicketConfigUpsert =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TICKET_CONFIG_UPSERT"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantTicketConfigUpsert
  )

type PostMerchantSchedulerTrigger =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SCHEDULER_TRIGGER"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSchedulerTrigger
  )

type PostMerchantConfigOperatingCityWhiteList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigOperatingCityWhiteList
  )

type PostMerchantConfigMerchantCreate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_MERCHANT_CREATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigMerchantCreate
  )

type GetMerchantRiderConfigEstimatesOrder =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_RIDER_CONFIG_ESTIMATES_ORDER"
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantRiderConfigEstimatesOrder
  )

type PostMerchantRiderConfigEstimatesOrderUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_RIDER_CONFIG_ESTIMATES_ORDER_UPDATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantRiderConfigEstimatesOrderUpdate
  )

type PostMerchantConfigDebugLogUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DEBUG_LOG_UPDATE"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigDebugLogUpdate
  )

type GetMerchantMerchantMessageCatalog =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_MESSAGE_CATALOG"
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantMerchantMessageCatalog
  )

type PostMerchantMerchantMessageUpsert =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_MESSAGE_UPSERT"
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantMerchantMessageUpsert
  )

type DeleteMerchantMerchantMessage =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_MERCHANT_MESSAGE"
      :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantMerchantMessage
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMerchantUpdate merchantId city :<|> getMerchantServiceUsageConfig merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city :<|> postMerchantServiceUsageConfigMapsUpdate merchantId city :<|> postMerchantServiceConfigSmsUpdate merchantId city :<|> postMerchantServiceUsageConfigSmsUpdate merchantId city :<|> postMerchantConfigOperatingCityCreate merchantId city :<|> postMerchantConfigSpecialLocationUpsert merchantId city :<|> getMerchantConfigSpecialLocationList merchantId city :<|> getMerchantConfigGeometryList merchantId city :<|> putMerchantConfigGeometryUpdate merchantId city :<|> postMerchantSpecialLocationUpsert merchantId city :<|> deleteMerchantSpecialLocationDelete merchantId city :<|> postMerchantSpecialLocationGatesUpsert merchantId city :<|> deleteMerchantSpecialLocationGatesDelete merchantId city :<|> postMerchantConfigTollUpsert merchantId city :<|> getMerchantConfigTollList merchantId city :<|> postMerchantTollUpsert merchantId city :<|> deleteMerchantTollDelete merchantId city :<|> postMerchantConfigFailover merchantId city :<|> postMerchantTicketConfigUpsert merchantId city :<|> postMerchantSchedulerTrigger merchantId city :<|> postMerchantConfigOperatingCityWhiteList merchantId city :<|> postMerchantConfigMerchantCreate merchantId city :<|> getMerchantRiderConfigEstimatesOrder merchantId city :<|> postMerchantRiderConfigEstimatesOrderUpdate merchantId city :<|> postMerchantConfigDebugLogUpdate merchantId city :<|> getMerchantMerchantMessageCatalog merchantId city :<|> postMerchantMerchantMessageUpsert merchantId city :<|> deleteMerchantMerchantMessage merchantId city

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantUpdate a4 a3 a1
    )

getMerchantServiceUsageConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler Dashboard.Common.Merchant.ServiceUsageConfigRes)
getMerchantServiceUsageConfig a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantServiceUsageConfig a3 a2

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantServiceConfigMapsUpdate a4 a3 a1
    )

postMerchantServiceUsageConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigMapsUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantServiceUsageConfigMapsUpdate a4 a3 a1
    )

postMerchantServiceConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigSmsUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantServiceConfigSmsUpdate a4 a3 a1
    )

postMerchantServiceUsageConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigSmsUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantServiceUsageConfigSmsUpdate a4 a3 a1
    )

postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.DashboardAuth.Management.Merchant.postMerchantConfigOperatingCityCreate a4 a3 a2 a1
    )

postMerchantConfigSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigSpecialLocationUpsert a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantConfigSpecialLocationUpsert a4 a3 a1
    )

getMerchantConfigSpecialLocationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Types.SpecialLocation.SpecialLocationType -> Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.SpecialLocationType] -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.SpecialLocationResp)
getMerchantConfigSpecialLocationList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantConfigSpecialLocationList a7 a6 a4 a3 a2 a1

getMerchantConfigGeometryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.GeometryResp)
getMerchantConfigGeometryList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantConfigGeometryList a6 a5 a3 a2 a1

putMerchantConfigGeometryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.UpdateGeometryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putMerchantConfigGeometryUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.putMerchantConfigGeometryUpdate a4 a3 a1
    )

postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.DashboardAuth.Management.Merchant.postMerchantSpecialLocationUpsert a5 a4 a3 a2 a1
    )

deleteMerchantSpecialLocationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationDelete a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.Dashboard.Merchant.deleteMerchantSpecialLocationDelete a4 a3 a1
    )

postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.DashboardAuth.Management.Merchant.postMerchantSpecialLocationGatesUpsert a5 a4 a3 a2 a1
    )

deleteMerchantSpecialLocationGatesDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationGatesDelete a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.Dashboard.Merchant.deleteMerchantSpecialLocationGatesDelete a5 a4 a2 a1
    )

postMerchantConfigTollUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.UpsertTollCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigTollUpsert a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_TOLL_UPSERT" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantConfigTollUpsert a4 a3 a1
    )

getMerchantConfigTollList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.TollListResp)
getMerchantConfigTollList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantConfigTollList a5 a4 a2 a1

postMerchantTollUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll) -> Dashboard.Common.Merchant.UpsertTollReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantTollUpsert a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TOLL_UPSERT" a3 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantTollUpsert a5 a4 a2 a1
    )

deleteMerchantTollDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantTollDelete a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_TOLL_DELETE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.Dashboard.Merchant.deleteMerchantTollDelete a4 a3 a1
    )

postMerchantConfigFailover :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFailover a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER" a3 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantConfigFailover a5 a4 a2 a1
    )

postMerchantTicketConfigUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Merchant.UpsertTicketConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.UpsertTicketConfigResp)
postMerchantTicketConfigUpsert a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TICKET_CONFIG_UPSERT" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantTicketConfigUpsert a4 a3 a1
    )

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Merchant.SchedulerTriggerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SCHEDULER_TRIGGER" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantSchedulerTrigger a4 a3 a1
    )

postMerchantConfigOperatingCityWhiteList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.WhiteListOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.WhiteListOperatingCityRes)
postMerchantConfigOperatingCityWhiteList a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantConfigOperatingCityWhiteList a4 a3 a1
    )

postMerchantConfigMerchantCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigMerchantCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_MERCHANT_CREATE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.DashboardAuth.Management.Merchant.postMerchantConfigMerchantCreate a4 a3 a2 a1
    )

getMerchantRiderConfigEstimatesOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.RiderConfigEstimatesOrderRes)
getMerchantRiderConfigEstimatesOrder a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantRiderConfigEstimatesOrder a3 a2

postMerchantRiderConfigEstimatesOrderUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Merchant.UpdateRiderConfigEstimatesOrderReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantRiderConfigEstimatesOrderUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_RIDER_CONFIG_ESTIMATES_ORDER_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantRiderConfigEstimatesOrderUpdate a4 a3 a1
    )

postMerchantConfigDebugLogUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Tools.DebugLog.SetJsonLogicDebugReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDebugLogUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DEBUG_LOG_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantConfigDebugLogUpdate a4 a3 a1
    )

getMerchantMerchantMessageCatalog :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.RiderMerchantMessageCatalogResp)
getMerchantMerchantMessageCatalog a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantMerchantMessageCatalog a3 a2

postMerchantMerchantMessageUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Merchant.UpsertRiderMerchantMessageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantMerchantMessageUpsert a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_MESSAGE_UPSERT" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.Merchant.postMerchantMerchantMessageUpsert a4 a3 a1
    )

deleteMerchantMerchantMessage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantMerchantMessage a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_MERCHANT_MESSAGE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.Dashboard.Merchant.deleteMerchantMerchantMessage a4 a3 a1
    )
