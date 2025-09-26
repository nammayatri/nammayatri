{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Merchant where

import qualified Dashboard.Common.Merchant
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data JobName
  = NyRegularMasterTrigger
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantUpdateReq = MerchantUpdateReq
  { name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exoPhones :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty Dashboard.Common.Merchant.ExophoneReq),
    fcmConfig :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.FCMConfigUpdateReq,
    gatewayUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl,
    registryUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SchedulerTriggerReq = SchedulerTriggerReq {scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, jobName :: Kernel.Prelude.Maybe JobName, jobData :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SchedulerTriggerReq where
  hideSecrets = Kernel.Prelude.identity

newtype UpsertTicketConfigReq = UpsertTicketConfigReq {file :: EulerHS.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertTicketConfigReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertTicketConfigResp = UpsertTicketConfigResp {unprocessedTicketConfigs :: [Kernel.Prelude.Text], success :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantConfigOperatingCityCreateHelper :<|> PostMerchantConfigSpecialLocationUpsert :<|> PostMerchantSpecialLocationUpsertHelper :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsertHelper :<|> DeleteMerchantSpecialLocationGatesDelete :<|> PostMerchantConfigFailover :<|> PostMerchantTicketConfigUpsert :<|> PostMerchantSchedulerTrigger :<|> PostMerchantConfigOperatingCityWhiteList :<|> PostMerchantConfigMerchantCreateHelper))

type PostMerchantUpdate = ("update" :> ReqBody '[JSON] MerchantUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetMerchantServiceUsageConfig = ("serviceUsageConfig" :> Get '[JSON] Dashboard.Common.Merchant.ServiceUsageConfigRes)

type PostMerchantServiceConfigMapsUpdate =
  ( "serviceConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceUsageConfigMapsUpdate =
  ( "serviceUsageConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceConfigSmsUpdate =
  ( "serviceConfig" :> "sms" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.SmsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceUsageConfigSmsUpdate =
  ( "serviceUsageConfig" :> "sms" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigOperatingCityCreate =
  ( "config" :> "operatingCity" :> "create"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.CreateMerchantOperatingCityReq
      :> Post '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantConfigOperatingCityCreateHelper =
  ( "config" :> "operatingCity" :> "create" :> ReqBody '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantConfigSpecialLocationUpsert =
  ( "config" :> "specialLocation" :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq
      :> Post '[JSON] Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities
  )

type PostMerchantSpecialLocationUpsert =
  ( "specialLocation" :> "upsert"
      :> QueryParam
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.Merchant.UpsertSpecialLocationReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationUpsertHelper =
  ( "specialLocation" :> "upsert" :> QueryParam "specialLocationId" (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> ReqBody
           '[JSON]
           Dashboard.Common.Merchant.UpsertSpecialLocationReqT
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantSpecialLocationDelete =
  ( "specialLocation" :> Capture "specialLocationId" (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationGatesUpsert =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.UpsertSpecialLocationGateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationGatesUpsertHelper =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "upsert"
      :> ReqBody '[JSON] Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantSpecialLocationGatesDelete =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "delete"
      :> Capture "gateName" Kernel.Prelude.Text
      :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFailover =
  ( "config" :> Capture "configName" Dashboard.Common.Merchant.ConfigNames :> "failover"
      :> ReqBody
           '[JSON]
           Dashboard.Common.Merchant.ConfigFailoverReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantTicketConfigUpsert =
  ( "ticket" :> "config" :> "upsert" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp UpsertTicketConfigReq
      :> Post
           '[JSON]
           UpsertTicketConfigResp
  )

type PostMerchantSchedulerTrigger = ("scheduler" :> "trigger" :> ReqBody '[JSON] SchedulerTriggerReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMerchantConfigOperatingCityWhiteList =
  ( "config" :> "operatingCity" :> "whiteList" :> ReqBody '[JSON] Dashboard.Common.Merchant.WhiteListOperatingCityReq
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.WhiteListOperatingCityRes
  )

type PostMerchantConfigMerchantCreate =
  ( "config" :> "merchant" :> "create"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.CreateMerchantOperatingCityReq
      :> Post '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantConfigMerchantCreateHelper =
  ( "config" :> "merchant" :> "create" :> ReqBody '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

data MerchantAPIs = MerchantAPIs
  { postMerchantUpdate :: MerchantUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantServiceUsageConfig :: EulerHS.Types.EulerClient Dashboard.Common.Merchant.ServiceUsageConfigRes,
    postMerchantServiceConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigOperatingCityCreate :: Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.CreateMerchantOperatingCityRes,
    postMerchantConfigSpecialLocationUpsert ::
      ( Data.ByteString.Lazy.ByteString,
        Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq
      ) ->
      EulerHS.Types.EulerClient Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities,
    postMerchantSpecialLocationUpsert :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationGatesUpsert :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationGatesDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFailover :: Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantTicketConfigUpsert :: (Data.ByteString.Lazy.ByteString, UpsertTicketConfigReq) -> EulerHS.Types.EulerClient UpsertTicketConfigResp,
    postMerchantSchedulerTrigger :: SchedulerTriggerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigOperatingCityWhiteList :: Dashboard.Common.Merchant.WhiteListOperatingCityReq -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.WhiteListOperatingCityRes,
    postMerchantConfigMerchantCreate :: Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> getMerchantServiceUsageConfig :<|> postMerchantServiceConfigMapsUpdate :<|> postMerchantServiceUsageConfigMapsUpdate :<|> postMerchantServiceConfigSmsUpdate :<|> postMerchantServiceUsageConfigSmsUpdate :<|> postMerchantConfigOperatingCityCreate :<|> postMerchantConfigSpecialLocationUpsert :<|> postMerchantSpecialLocationUpsert :<|> deleteMerchantSpecialLocationDelete :<|> postMerchantSpecialLocationGatesUpsert :<|> deleteMerchantSpecialLocationGatesDelete :<|> postMerchantConfigFailover :<|> postMerchantTicketConfigUpsert :<|> postMerchantSchedulerTrigger :<|> postMerchantConfigOperatingCityWhiteList :<|> postMerchantConfigMerchantCreate = merchantClient

data MerchantUserActionType
  = POST_MERCHANT_UPDATE
  | GET_MERCHANT_SERVICE_USAGE_CONFIG
  | POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE
  | POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE
  | POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE
  | POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE
  | POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE
  | POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT
  | POST_MERCHANT_SPECIAL_LOCATION_UPSERT
  | DELETE_MERCHANT_SPECIAL_LOCATION_DELETE
  | POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT
  | DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE
  | POST_MERCHANT_CONFIG_FAILOVER
  | POST_MERCHANT_TICKET_CONFIG_UPSERT
  | POST_MERCHANT_SCHEDULER_TRIGGER
  | POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST
  | POST_MERCHANT_CONFIG_MERCHANT_CREATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MerchantUserActionType])
