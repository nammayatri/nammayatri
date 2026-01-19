{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Merchant
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Dashboard.Common.Merchant
import qualified Domain.Action.RiderPlatform.Management.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantConfigOperatingCityCreate :<|> PostMerchantConfigSpecialLocationUpsert :<|> GetMerchantConfigSpecialLocationList :<|> GetMerchantConfigGeometryList :<|> PutMerchantConfigGeometryUpdate :<|> PostMerchantSpecialLocationUpsert :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsert :<|> DeleteMerchantSpecialLocationGatesDelete :<|> PostMerchantConfigFailover :<|> PostMerchantTicketConfigUpsert :<|> PostMerchantSchedulerTrigger :<|> PostMerchantConfigOperatingCityWhiteList :<|> PostMerchantConfigMerchantCreate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMerchantUpdate merchantId city :<|> getMerchantServiceUsageConfig merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city :<|> postMerchantServiceUsageConfigMapsUpdate merchantId city :<|> postMerchantServiceConfigSmsUpdate merchantId city :<|> postMerchantServiceUsageConfigSmsUpdate merchantId city :<|> postMerchantConfigOperatingCityCreate merchantId city :<|> postMerchantConfigSpecialLocationUpsert merchantId city :<|> getMerchantConfigSpecialLocationList merchantId city :<|> getMerchantConfigGeometryList merchantId city :<|> putMerchantConfigGeometryUpdate merchantId city :<|> postMerchantSpecialLocationUpsert merchantId city :<|> deleteMerchantSpecialLocationDelete merchantId city :<|> postMerchantSpecialLocationGatesUpsert merchantId city :<|> deleteMerchantSpecialLocationGatesDelete merchantId city :<|> postMerchantConfigFailover merchantId city :<|> postMerchantTicketConfigUpsert merchantId city :<|> postMerchantSchedulerTrigger merchantId city :<|> postMerchantConfigOperatingCityWhiteList merchantId city :<|> postMerchantConfigMerchantCreate merchantId city

type PostMerchantUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_UPDATE)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantUpdate
  )

type GetMerchantServiceUsageConfig =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.GET_MERCHANT_SERVICE_USAGE_CONFIG)
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantServiceUsageConfig
  )

type PostMerchantServiceConfigMapsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceConfigMapsUpdate
  )

type PostMerchantServiceUsageConfigMapsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceUsageConfigMapsUpdate
  )

type PostMerchantServiceConfigSmsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceConfigSmsUpdate
  )

type PostMerchantServiceUsageConfigSmsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceUsageConfigSmsUpdate
  )

type PostMerchantConfigOperatingCityCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigOperatingCityCreate
  )

type PostMerchantConfigSpecialLocationUpsert =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigSpecialLocationUpsert
  )

type GetMerchantConfigSpecialLocationList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.GET_MERCHANT_CONFIG_SPECIAL_LOCATION_LIST)
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantConfigSpecialLocationList
  )

type GetMerchantConfigGeometryList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.GET_MERCHANT_CONFIG_GEOMETRY_LIST)
      :> API.Types.RiderPlatform.Management.Merchant.GetMerchantConfigGeometryList
  )

type PutMerchantConfigGeometryUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE)
      :> API.Types.RiderPlatform.Management.Merchant.PutMerchantConfigGeometryUpdate
  )

type PostMerchantSpecialLocationUpsert =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_SPECIAL_LOCATION_UPSERT)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSpecialLocationUpsert
  )

type DeleteMerchantSpecialLocationDelete =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.DELETE_MERCHANT_SPECIAL_LOCATION_DELETE)
      :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantSpecialLocationDelete
  )

type PostMerchantSpecialLocationGatesUpsert =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSpecialLocationGatesUpsert
  )

type DeleteMerchantSpecialLocationGatesDelete =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE)
      :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantSpecialLocationGatesDelete
  )

type PostMerchantConfigFailover =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_CONFIG_FAILOVER)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigFailover
  )

type PostMerchantTicketConfigUpsert =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_TICKET_CONFIG_UPSERT)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantTicketConfigUpsert
  )

type PostMerchantSchedulerTrigger =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_SCHEDULER_TRIGGER)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSchedulerTrigger
  )

type PostMerchantConfigOperatingCityWhiteList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigOperatingCityWhiteList
  )

type PostMerchantConfigMerchantCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MERCHANT / 'API.Types.RiderPlatform.Management.Merchant.POST_MERCHANT_CONFIG_MERCHANT_CREATE)
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigMerchantCreate
  )

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantUpdate merchantShortId opCity apiTokenInfo req

getMerchantServiceUsageConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Dashboard.Common.Merchant.ServiceUsageConfigRes)
getMerchantServiceUsageConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.getMerchantServiceUsageConfig merchantShortId opCity apiTokenInfo

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req

postMerchantServiceUsageConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigMapsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceUsageConfigMapsUpdate merchantShortId opCity apiTokenInfo req

postMerchantServiceConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigSmsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceConfigSmsUpdate merchantShortId opCity apiTokenInfo req

postMerchantServiceUsageConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigSmsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceUsageConfigSmsUpdate merchantShortId opCity apiTokenInfo req

postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantConfigOperatingCityCreate merchantShortId opCity apiTokenInfo req

postMerchantConfigSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq -> Environment.FlowHandler Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities)
postMerchantConfigSpecialLocationUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantConfigSpecialLocationUpsert merchantShortId opCity apiTokenInfo req

getMerchantConfigSpecialLocationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Types.SpecialLocation.SpecialLocationType -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.SpecialLocationResp)
getMerchantConfigSpecialLocationList merchantShortId opCity apiTokenInfo limit offset locationType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.getMerchantConfigSpecialLocationList merchantShortId opCity apiTokenInfo limit offset locationType

getMerchantConfigGeometryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.GeometryResp)
getMerchantConfigGeometryList merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.getMerchantConfigGeometryList merchantShortId opCity apiTokenInfo limit offset

putMerchantConfigGeometryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.UpdateGeometryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putMerchantConfigGeometryUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.putMerchantConfigGeometryUpdate merchantShortId opCity apiTokenInfo req

postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert merchantShortId opCity apiTokenInfo specialLocationId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantSpecialLocationUpsert merchantShortId opCity apiTokenInfo specialLocationId req

deleteMerchantSpecialLocationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationDelete merchantShortId opCity apiTokenInfo specialLocationId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.deleteMerchantSpecialLocationDelete merchantShortId opCity apiTokenInfo specialLocationId

postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert merchantShortId opCity apiTokenInfo specialLocationId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantSpecialLocationGatesUpsert merchantShortId opCity apiTokenInfo specialLocationId req

deleteMerchantSpecialLocationGatesDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationGatesDelete merchantShortId opCity apiTokenInfo specialLocationId gateName = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.deleteMerchantSpecialLocationGatesDelete merchantShortId opCity apiTokenInfo specialLocationId gateName

postMerchantConfigFailover :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFailover merchantShortId opCity apiTokenInfo configName req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantConfigFailover merchantShortId opCity apiTokenInfo configName req

postMerchantTicketConfigUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Merchant.UpsertTicketConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Merchant.UpsertTicketConfigResp)
postMerchantTicketConfigUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantTicketConfigUpsert merchantShortId opCity apiTokenInfo req

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Merchant.SchedulerTriggerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantSchedulerTrigger merchantShortId opCity apiTokenInfo req

postMerchantConfigOperatingCityWhiteList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.WhiteListOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.WhiteListOperatingCityRes)
postMerchantConfigOperatingCityWhiteList merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantConfigOperatingCityWhiteList merchantShortId opCity apiTokenInfo req

postMerchantConfigMerchantCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigMerchantCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantConfigMerchantCreate merchantShortId opCity apiTokenInfo req
