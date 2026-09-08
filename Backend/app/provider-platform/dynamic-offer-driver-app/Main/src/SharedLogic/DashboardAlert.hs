{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DashboardAlert
  ( AlertActor (..),
    audiencesFor,
    withoutAdminAudience,
    onboardingAlertKey,
    DashboardAlertFlow,
    driverAlertHandle,
    notifyOnboardingChange,
    notifyAdminsRealtime,
    notifyFleetOwnerRealtime,
    dashboardAudiences,
    notifyDashboardConfigChange,
  )
where

import qualified DashboardAlert.Domain.Types.Audience as DAA
import qualified DashboardAlert.Domain.Types.Common as DAC
import qualified DashboardAlert.Domain.Types.DashboardAlert as DADT
import qualified DashboardAlert.ServiceHandle as DAS
import qualified DashboardAlert.Trigger as DAT
import Data.List (nub)
import qualified Domain.Types.Alert as DAlert
import qualified Domain.Types.Alert.AlertCategory as DAlertCategory
import qualified Domain.Types.Alert.AlertEntityType as DAlertEntity
import qualified Domain.Types.Alert.AlertRequestData as DAlertData
import qualified Domain.Types.Alert.AlertRequestType as DAlertType
import qualified Domain.Types.Alert.OnboardingAlertAction as DOnboardingAlertAction
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Notification.GRPC.Types as GRPCTypes
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.Beam.DashboardAlert ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as TN

type DashboardAlertFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  )

data AlertActor
  = AdminActor
  | FleetOwnerActor (Id DP.Person)
  | SystemActor
  deriving (Show, Eq)

audiencesFor :: AlertActor -> [Id DP.Person] -> [DAA.Audience]
audiencesFor actor fleetOwnerIds =
  [DAA.AccessTypeAudience DAA.DASHBOARD_ADMIN | actor /= AdminActor]
    <> [DAA.FleetOwnerAudience (cast fleetOwnerId) | fleetOwnerId <- nub fleetOwnerIds, actor /= FleetOwnerActor fleetOwnerId]

withoutAdminAudience :: [DAA.Audience] -> [DAA.Audience]
withoutAdminAudience = filter (/= DAA.AccessTypeAudience DAA.DASHBOARD_ADMIN)

driverAlertHandle :: DashboardAlertFlow m r => DAS.ServiceHandle m
driverAlertHandle =
  DAS.ServiceHandle
    { getGRPCConfig = resolveGRPCConfig . cast,
      getAlertContext = resolveAlertContext,
      platform = DAA.DriverPlatform
    }

resolveAlertContext :: DashboardAlertFlow m r => Id DAC.Merchant -> Id DAC.MerchantOperatingCity -> m DAS.AlertContext
resolveAlertContext merchantId merchantOpCityId = do
  mbMerchant <- CQM.findById (cast merchantId)
  mbMerchantOpCity <- CQMOC.findById (cast merchantOpCityId)
  pure $
    DAS.AlertContext
      { merchantName = (.name) <$> mbMerchant,
        cityName = show . (.city) <$> mbMerchantOpCity
      }

resolveGRPCConfig :: DashboardAlertFlow m r => Id DMOC.MerchantOperatingCity -> m GRPCTypes.GRPCConfig
resolveGRPCConfig merchantOpCityId = do
  serviceConfig <-
    getOneConfig
      (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.NotificationService Notification.GRPC)})
      Nothing
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Notification" "GRPC")
  case serviceConfig.serviceConfig of
    DMSC.NotificationServiceConfig (Notification.GRPCConfig cfg) -> pure cfg
    _ -> throwError $ InternalError "Expected a GRPC notification service config"

buildOnboardingContent ::
  DAlertEntity.AlertEntityType ->
  Text ->
  DOnboardingAlertAction.OnboardingAlertAction ->
  Text ->
  Text ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DAT.AlertContent
buildOnboardingContent entityType entityId action title body requestorId merchantId merchantOperatingCityId =
  let onboardingData =
        DAlertData.OnboardingAlertData
          { entityType = entityType,
            entityId = entityId,
            action = action,
            title = title,
            body = body
          }
   in DAT.AlertContent
        { category = DAlertCategory.ONBOARDING_UPDATE,
          title = title,
          body = body,
          entityId = entityId,
          entityType = entityType,
          entityData = toJSON onboardingData,
          requestType = DAlertType.OnboardingAlert,
          requestData = DAlertData.Onboarding onboardingData,
          requestorId = cast requestorId,
          requestorType = DADT.SystemGenerated,
          requesteeType = DADT.FleetOwner,
          requiresAction = False,
          visibility = Notification.SHOW,
          merchantId = cast merchantId,
          merchantOperatingCityId = cast merchantOperatingCityId,
          ttlSeconds = Seconds 300
        }

actionKey :: DOnboardingAlertAction.OnboardingAlertAction -> Text
actionKey = \case
  DOnboardingAlertAction.LinkVehicleAction -> "LINK_VEHICLE"
  DOnboardingAlertAction.UnlinkVehicleAction -> "UNLINK_VEHICLE"
  DOnboardingAlertAction.ActivateVehicleAction -> "ACTIVATE_VEHICLE"
  DOnboardingAlertAction.DeactivateVehicleAction -> "DEACTIVATE_VEHICLE"
  DOnboardingAlertAction.LinkToOperatorAction -> "LINK_TO_OPERATOR"
  DOnboardingAlertAction.UnlinkFromOperatorAction -> "UNLINK_FROM_OPERATOR"
  DOnboardingAlertAction.UnlinkFromFleetAction -> "UNLINK_FROM_FLEET"
  DOnboardingAlertAction.AddAction -> "ADD"
  DOnboardingAlertAction.DeleteAction -> "DELETE"
  DOnboardingAlertAction.EnableAction -> "ENABLE"
  DOnboardingAlertAction.DisableAction -> "DISABLE"
  DOnboardingAlertAction.BlockAction -> "BLOCK"
  DOnboardingAlertAction.UnblockAction -> "UNBLOCK"
  DOnboardingAlertAction.ApproveAction -> "APPROVE"
  DOnboardingAlertAction.RejectAction -> "REJECT"
  DOnboardingAlertAction.SetOnboardingAsAction -> "SET_ONBOARDING_AS"
  DOnboardingAlertAction.LinkToFleetAction -> "LINK_TO_FLEET"
  DOnboardingAlertAction.ActivateToFleetAction -> "ACTIVATE_TO_FLEET"
  DOnboardingAlertAction.DeactivateFromFleetAction -> "DEACTIVATE_FROM_FLEET"
  DOnboardingAlertAction.ViewAction -> "VIEW"
  DOnboardingAlertAction.ChangeFleetOwnerAction -> "CHANGE_FLEET_OWNER"
  DOnboardingAlertAction.ExpireAction -> "EXPIRE"
  DOnboardingAlertAction.UnlinkDocumentAction -> "UNLINK_DOCUMENT"
  DOnboardingAlertAction.OnboardingFlagMutationAction -> "ONBOARDING_FLAG_MUTATION"
  DOnboardingAlertAction.DocumentApprovalPendingAction -> "DOCUMENT_APPROVAL_PENDING"

audienceKeySuffix :: DAA.Audience -> Text
audienceKeySuffix = \case
  DAA.AccessTypeAudience accessType -> show accessType
  DAA.FleetOwnerAudience _ -> "FLEET_OWNER"

onboardingAlertKey :: DOnboardingAlertAction.OnboardingAlertAction -> DAA.Audience -> Text
onboardingAlertKey action audience = "ONBOARDING_ALERT_" <> actionKey action <> "_" <> audienceKeySuffix audience

audienceLanguage :: DashboardAlertFlow m r => DAA.Audience -> m (Maybe Language)
audienceLanguage = \case
  DAA.AccessTypeAudience _ -> pure Nothing
  DAA.FleetOwnerAudience personId -> ((.language) =<<) <$> QPerson.findById (cast personId)

notifyOnboardingChange ::
  DashboardAlertFlow m r =>
  [DAA.Audience] ->
  DAlertEntity.AlertEntityType ->
  Text ->
  DOnboardingAlertAction.OnboardingAlertAction ->
  [(Text, Text)] ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
notifyOnboardingChange audiences entityType entityId action dynamicParams requestorId merchantId merchantOpCityId =
  forM_ audiences $ \audience -> do
    mbLanguage <- audienceLanguage audience
    mbContent <- TN.buildMerchantPNContent merchantOpCityId mbLanguage (onboardingAlertKey action audience) dynamicParams
    whenJust mbContent $ \(title, body) ->
      DAT.triggerRealtime driverAlertHandle [audience] $
        buildOnboardingContent entityType entityId action title body requestorId merchantId merchantOpCityId

notifyAdminsRealtime ::
  DashboardAlertFlow m r =>
  DAlertEntity.AlertEntityType ->
  Text ->
  DOnboardingAlertAction.OnboardingAlertAction ->
  [(Text, Text)] ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
notifyAdminsRealtime = notifyOnboardingChange [DAA.AccessTypeAudience DAA.DASHBOARD_ADMIN]

notifyFleetOwnerRealtime ::
  DashboardAlertFlow m r =>
  Id DP.Person ->
  DAlertEntity.AlertEntityType ->
  Text ->
  DOnboardingAlertAction.OnboardingAlertAction ->
  [(Text, Text)] ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
notifyFleetOwnerRealtime fleetOwnerId = notifyOnboardingChange [DAA.FleetOwnerAudience (cast fleetOwnerId)]

dashboardAudiences :: [DAA.Audience]
dashboardAudiences = [DAA.AccessTypeAudience DAA.DASHBOARD_ADMIN, DAA.AccessTypeAudience DAA.DASHBOARD_USER]

configRequestTypeKey :: DAlertType.AlertRequestType -> Text
configRequestTypeKey = \case
  DAlertType.FareConfigUpdate -> "FARE_CONFIG_UPDATE"
  DAlertType.OperatingCityCreate -> "OPERATING_CITY_CREATE"
  other -> show other

mkConfigAlertData :: Text -> Text -> Text -> DAlertData.ConfigAlertData
mkConfigAlertData entityId title body =
  DAlertData.ConfigAlertData
    { entityType = DAlertEntity.ConfigChangeEntity,
      entityId = entityId,
      title = title,
      body = body
    }

configAlertKey :: DAlertType.AlertRequestType -> DAA.Audience -> Text
configAlertKey requestType audience = "CONFIG_ALERT_" <> configRequestTypeKey requestType <> "_" <> audienceKeySuffix audience

buildConfigContent ::
  Text ->
  (DAlertData.ConfigAlertData -> DAlertData.AlertRequestData) ->
  Text ->
  Text ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DAT.AlertContent
buildConfigContent entityId mkRequestData title body requestorId merchantId merchantOperatingCityId =
  let configData = mkConfigAlertData entityId title body
      requestData = mkRequestData configData
   in DAT.AlertContent
        { category = DAlertCategory.CONFIG_CHANGE,
          title = title,
          body = body,
          entityId = entityId,
          entityType = DAlertEntity.ConfigChangeEntity,
          entityData = toJSON configData,
          requestType = DAlert.castAlertRequestDataToRequestType requestData,
          requestData = requestData,
          requestorId = cast requestorId,
          requestorType = DADT.SystemGenerated,
          requesteeType = DADT.FleetOwner,
          requiresAction = False,
          visibility = Notification.SHOW,
          merchantId = cast merchantId,
          merchantOperatingCityId = cast merchantOperatingCityId,
          ttlSeconds = Seconds 300
        }

notifyDashboardConfigChange ::
  DashboardAlertFlow m r =>
  [DAA.Audience] ->
  Text ->
  (DAlertData.ConfigAlertData -> DAlertData.AlertRequestData) ->
  [(Text, Text)] ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
notifyDashboardConfigChange audiences entityId mkRequestData dynamicParams requestorId merchantId merchantOpCityId = do
  let requestType = DAlert.castAlertRequestDataToRequestType (mkRequestData (mkConfigAlertData entityId "" ""))
  forM_ audiences $ \audience -> do
    mbLanguage <- audienceLanguage audience
    mbContent <- TN.buildMerchantPNContent merchantOpCityId mbLanguage (configAlertKey requestType audience) dynamicParams
    whenJust mbContent $ \(title, body) ->
      void $
        DAT.triggerPersist driverAlertHandle [audience] $
          buildConfigContent entityId mkRequestData title body requestorId merchantId merchantOpCityId
