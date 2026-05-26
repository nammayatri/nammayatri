{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Merchant where

import qualified Dashboard.Common
import qualified Dashboard.Common.Merchant
import Data.Aeson
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Queries.SpecialLocation
import qualified Lib.Types.SpecialLocation
import qualified Lib.Yudhishthira.Tools.DebugLog
import Servant
import Servant.Client
import qualified Toll.Domain.Types.Toll

data DeleteRiderMerchantMessageReq = DeleteRiderMerchantMessageReq {messageKey :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeleteRiderMerchantMessageReq where
  hideSecrets = Kernel.Prelude.identity

data GeometryAPIEntity = GeometryAPIEntity
  { region :: Kernel.Prelude.Text,
    state :: Kernel.Types.Beckn.Context.IndianState,
    city :: Kernel.Types.Beckn.Context.City,
    geom :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platform :: Platform
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type GeometryResp = [GeometryAPIEntity]

data JobName
  = NyRegularMasterTrigger
  | PartnerInvoiceDataExportTrigger
  | DailyPassStatusUpdateTrigger
  | PassExpiryReminderMasterTrigger
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

data Platform
  = Rider
  | Driver
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderConfigEstimatesOrderRes = RiderConfigEstimatesOrderRes
  { userServiceTierOrderConfig :: [Dashboard.Common.VehicleServiceTierOrderConfig],
    defaultServiceTierOrderConfig :: [Dashboard.Common.ServiceTierType],
    noOfRideRequestsConfig :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderMerchantMessageCatalogResp = RiderMerchantMessageCatalogResp {values :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderMerchantMessageCatalogType
  = KEY
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data SchedulerTriggerReq = SchedulerTriggerReq {scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, jobName :: Kernel.Prelude.Maybe JobName, jobData :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SchedulerTriggerReq where
  hideSecrets = Kernel.Prelude.identity

type SpecialLocationResp = [SpecialLocationWithPlatform]

data SpecialLocationWithPlatform = SpecialLocationWithPlatform {location :: Lib.Queries.SpecialLocation.SpecialLocationFull, platform :: Platform}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type TollListResp = [Dashboard.Common.Merchant.TollAPIEntity]

data UpdateRiderConfigEstimatesOrderReq = UpdateRiderConfigEstimatesOrderReq
  { userServiceTierOrderConfig :: Kernel.Prelude.Maybe [Dashboard.Common.VehicleServiceTierOrderConfig],
    defaultServiceTierOrderConfig :: Kernel.Prelude.Maybe [Dashboard.Common.ServiceTierType],
    noOfRideRequestsConfig :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateRiderConfigEstimatesOrderReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertRiderMerchantMessageReq = UpsertRiderMerchantMessageReq
  { messageKey :: Kernel.Prelude.Text,
    message :: Kernel.Prelude.Text,
    templateId :: Kernel.Prelude.Text,
    containsUrlButton :: Kernel.Prelude.Bool,
    jsonData :: Kernel.Prelude.Maybe Data.Aeson.Value,
    messageType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    senderHeader :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertRiderMerchantMessageReq where
  hideSecrets = Kernel.Prelude.identity

newtype UpsertTicketConfigReq = UpsertTicketConfigReq {file :: EulerHS.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertTicketConfigReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertTicketConfigResp = UpsertTicketConfigResp {unprocessedTicketConfigs :: [Kernel.Prelude.Text], success :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantConfigOperatingCityCreateHelper :<|> PostMerchantConfigSpecialLocationUpsert :<|> GetMerchantConfigSpecialLocationList :<|> GetMerchantConfigGeometryList :<|> PutMerchantConfigGeometryUpdate :<|> PostMerchantSpecialLocationUpsertHelper :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsertHelper :<|> DeleteMerchantSpecialLocationGatesDelete :<|> PostMerchantConfigTollUpsert :<|> GetMerchantConfigTollList :<|> PostMerchantTollUpsert :<|> DeleteMerchantTollDelete :<|> PostMerchantConfigFailover :<|> PostMerchantTicketConfigUpsert :<|> PostMerchantSchedulerTrigger :<|> PostMerchantConfigOperatingCityWhiteList :<|> PostMerchantConfigMerchantCreateHelper :<|> GetMerchantRiderConfigEstimatesOrder :<|> PostMerchantRiderConfigEstimatesOrderUpdate :<|> PostMerchantConfigDebugLogUpdate :<|> GetMerchantMerchantMessageCatalog :<|> PostMerchantMerchantMessageUpsert :<|> PostMerchantMerchantMessageDelete :<|> GetMerchantConfigPushNotification :<|> GetMerchantConfigPushNotificationKeys :<|> PostMerchantConfigPushNotificationUpsert :<|> DeleteMerchantConfigPushNotificationDelete))

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

type GetMerchantConfigSpecialLocationList =
  ( "config" :> "specialLocation" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "locationType"
           Lib.Types.SpecialLocation.SpecialLocationType
      :> QueryParam
           "locationTypes"
           [Lib.Types.SpecialLocation.SpecialLocationType]
      :> Get
           '[JSON]
           SpecialLocationResp
  )

type GetMerchantConfigGeometryList =
  ( "config" :> "geometry" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "allCities"
           Kernel.Prelude.Bool
      :> Get '[JSON] GeometryResp
  )

type PutMerchantConfigGeometryUpdate =
  ( "config" :> "geometry" :> "update"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.UpdateGeometryReq
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
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

type PostMerchantConfigTollUpsert =
  ( "config" :> "toll" :> "upsert" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.Merchant.UpsertTollCsvReq
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities
  )

type GetMerchantConfigTollList = ("config" :> "toll" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] TollListResp)

type PostMerchantTollUpsert =
  ( "toll" :> "upsert" :> QueryParam "tollId" (Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll) :> ReqBody '[JSON] Dashboard.Common.Merchant.UpsertTollReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantTollDelete = ("toll" :> Capture "tollId" (Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll) :> "delete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

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

type GetMerchantRiderConfigEstimatesOrder = ("riderConfig" :> "estimatesOrder" :> Get '[JSON] RiderConfigEstimatesOrderRes)

type PostMerchantRiderConfigEstimatesOrderUpdate =
  ( "riderConfig" :> "estimatesOrder" :> "update" :> ReqBody '[JSON] UpdateRiderConfigEstimatesOrderReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigDebugLogUpdate =
  ( "config" :> "debugLog" :> "update" :> ReqBody '[JSON] Lib.Yudhishthira.Tools.DebugLog.SetJsonLogicDebugReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantMerchantMessageCatalog = ("merchantMessage" :> "catalog" :> MandatoryQueryParam "catalogType" RiderMerchantMessageCatalogType :> Get '[JSON] RiderMerchantMessageCatalogResp)

type PostMerchantMerchantMessageUpsert = ("merchantMessage" :> "upsert" :> ReqBody '[JSON] UpsertRiderMerchantMessageReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMerchantMerchantMessageDelete = ("merchantMessage" :> "delete" :> ReqBody '[JSON] DeleteRiderMerchantMessageReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetMerchantConfigPushNotification =
  ( "config" :> "pushNotification" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "key"
           Kernel.Prelude.Text
      :> QueryParam "language" Kernel.External.Types.Language
      :> QueryParam
           "isCritical"
           Kernel.Prelude.Bool
      :> QueryParam
           "shouldTrigger"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           [Dashboard.Common.Merchant.MerchantPushNotificationRes]
  )

type GetMerchantConfigPushNotificationKeys = ("config" :> "pushNotification" :> "keys" :> Get '[JSON] [Kernel.Prelude.Text])

type PostMerchantConfigPushNotificationUpsert =
  ( "config" :> "pushNotification" :> "upsert" :> QueryParam "toggle" Kernel.Prelude.Bool
      :> QueryParam
           "allLanguages"
           Kernel.Prelude.Bool
      :> ReqBody '[JSON] Dashboard.Common.Merchant.MerchantPushNotificationUpsertReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantConfigPushNotificationDelete =
  ( "config" :> "pushNotification"
      :> Capture
           "notificationId"
           (Kernel.Types.Id.Id Dashboard.Common.Merchant.MerchantPushNotification)
      :> "delete"
      :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess
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
    getMerchantConfigSpecialLocationList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Types.SpecialLocation.SpecialLocationType -> Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.SpecialLocationType] -> EulerHS.Types.EulerClient SpecialLocationResp,
    getMerchantConfigGeometryList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient GeometryResp,
    putMerchantConfigGeometryUpdate :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.Merchant.UpdateGeometryReq) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationUpsert :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationGatesUpsert :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationGatesDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigTollUpsert ::
      ( Data.ByteString.Lazy.ByteString,
        Dashboard.Common.Merchant.UpsertTollCsvReq
      ) ->
      EulerHS.Types.EulerClient Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities,
    getMerchantConfigTollList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient TollListResp,
    postMerchantTollUpsert :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll) -> Dashboard.Common.Merchant.UpsertTollReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantTollDelete :: Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFailover :: Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantTicketConfigUpsert :: (Data.ByteString.Lazy.ByteString, UpsertTicketConfigReq) -> EulerHS.Types.EulerClient UpsertTicketConfigResp,
    postMerchantSchedulerTrigger :: SchedulerTriggerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigOperatingCityWhiteList :: Dashboard.Common.Merchant.WhiteListOperatingCityReq -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.WhiteListOperatingCityRes,
    postMerchantConfigMerchantCreate :: Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.CreateMerchantOperatingCityRes,
    getMerchantRiderConfigEstimatesOrder :: EulerHS.Types.EulerClient RiderConfigEstimatesOrderRes,
    postMerchantRiderConfigEstimatesOrderUpdate :: UpdateRiderConfigEstimatesOrderReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigDebugLogUpdate :: Lib.Yudhishthira.Tools.DebugLog.SetJsonLogicDebugReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantMerchantMessageCatalog :: RiderMerchantMessageCatalogType -> EulerHS.Types.EulerClient RiderMerchantMessageCatalogResp,
    postMerchantMerchantMessageUpsert :: UpsertRiderMerchantMessageReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantMerchantMessageDelete :: DeleteRiderMerchantMessageReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantConfigPushNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient [Dashboard.Common.Merchant.MerchantPushNotificationRes],
    getMerchantConfigPushNotificationKeys :: EulerHS.Types.EulerClient [Kernel.Prelude.Text],
    postMerchantConfigPushNotificationUpsert :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Dashboard.Common.Merchant.MerchantPushNotificationUpsertReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantConfigPushNotificationDelete :: Kernel.Types.Id.Id Dashboard.Common.Merchant.MerchantPushNotification -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> getMerchantServiceUsageConfig :<|> postMerchantServiceConfigMapsUpdate :<|> postMerchantServiceUsageConfigMapsUpdate :<|> postMerchantServiceConfigSmsUpdate :<|> postMerchantServiceUsageConfigSmsUpdate :<|> postMerchantConfigOperatingCityCreate :<|> postMerchantConfigSpecialLocationUpsert :<|> getMerchantConfigSpecialLocationList :<|> getMerchantConfigGeometryList :<|> putMerchantConfigGeometryUpdate :<|> postMerchantSpecialLocationUpsert :<|> deleteMerchantSpecialLocationDelete :<|> postMerchantSpecialLocationGatesUpsert :<|> deleteMerchantSpecialLocationGatesDelete :<|> postMerchantConfigTollUpsert :<|> getMerchantConfigTollList :<|> postMerchantTollUpsert :<|> deleteMerchantTollDelete :<|> postMerchantConfigFailover :<|> postMerchantTicketConfigUpsert :<|> postMerchantSchedulerTrigger :<|> postMerchantConfigOperatingCityWhiteList :<|> postMerchantConfigMerchantCreate :<|> getMerchantRiderConfigEstimatesOrder :<|> postMerchantRiderConfigEstimatesOrderUpdate :<|> postMerchantConfigDebugLogUpdate :<|> getMerchantMerchantMessageCatalog :<|> postMerchantMerchantMessageUpsert :<|> postMerchantMerchantMessageDelete :<|> getMerchantConfigPushNotification :<|> getMerchantConfigPushNotificationKeys :<|> postMerchantConfigPushNotificationUpsert :<|> deleteMerchantConfigPushNotificationDelete = merchantClient

data MerchantUserActionType
  = POST_MERCHANT_UPDATE
  | GET_MERCHANT_SERVICE_USAGE_CONFIG
  | POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE
  | POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE
  | POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE
  | POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE
  | POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE
  | POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT
  | GET_MERCHANT_CONFIG_SPECIAL_LOCATION_LIST
  | GET_MERCHANT_CONFIG_GEOMETRY_LIST
  | PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE
  | POST_MERCHANT_SPECIAL_LOCATION_UPSERT
  | DELETE_MERCHANT_SPECIAL_LOCATION_DELETE
  | POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT
  | DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE
  | POST_MERCHANT_CONFIG_TOLL_UPSERT
  | GET_MERCHANT_CONFIG_TOLL_LIST
  | POST_MERCHANT_TOLL_UPSERT
  | DELETE_MERCHANT_TOLL_DELETE
  | POST_MERCHANT_CONFIG_FAILOVER
  | POST_MERCHANT_TICKET_CONFIG_UPSERT
  | POST_MERCHANT_SCHEDULER_TRIGGER
  | POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST
  | POST_MERCHANT_CONFIG_MERCHANT_CREATE
  | GET_MERCHANT_RIDER_CONFIG_ESTIMATES_ORDER
  | POST_MERCHANT_RIDER_CONFIG_ESTIMATES_ORDER_UPDATE
  | POST_MERCHANT_CONFIG_DEBUG_LOG_UPDATE
  | GET_MERCHANT_MERCHANT_MESSAGE_CATALOG
  | POST_MERCHANT_MERCHANT_MESSAGE_UPSERT
  | POST_MERCHANT_MERCHANT_MESSAGE_DELETE
  | GET_MERCHANT_CONFIG_PUSH_NOTIFICATION
  | GET_MERCHANT_CONFIG_PUSH_NOTIFICATION_KEYS
  | POST_MERCHANT_CONFIG_PUSH_NOTIFICATION_UPSERT
  | DELETE_MERCHANT_CONFIG_PUSH_NOTIFICATION_DELETE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''RiderMerchantMessageCatalogType)

$(Data.Singletons.TH.genSingletons [''MerchantUserActionType])
