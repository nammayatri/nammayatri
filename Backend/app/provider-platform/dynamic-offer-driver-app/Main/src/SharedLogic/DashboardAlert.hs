{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DashboardAlert
  ( DashboardAlertFlow,
    driverAlertHandle,
    notifyOnboardingChange,
    notifyAdminsRealtime,
    notifyFleetOwnerRealtime,
  )
where

import qualified DashboardAlert.Domain.Types.Audience as DAA
import qualified DashboardAlert.Domain.Types.DashboardAlert as DADT
import qualified DashboardAlert.ServiceHandle as DAS
import qualified DashboardAlert.Trigger as DAT
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
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.Beam.DashboardAlert ()
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

type DashboardAlertFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  )

driverAlertHandle :: DashboardAlertFlow m r => DAS.ServiceHandle m
driverAlertHandle =
  DAS.ServiceHandle
    { getGRPCConfig = resolveGRPCConfig . cast,
      platform = DAA.DriverPlatform
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
          ttlSeconds = Seconds 3600
        }

notifyOnboardingChange ::
  DashboardAlertFlow m r =>
  [DAA.Audience] ->
  DAlertEntity.AlertEntityType ->
  Text ->
  DOnboardingAlertAction.OnboardingAlertAction ->
  Text ->
  Text ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
notifyOnboardingChange audiences entityType entityId action title body requestorId merchantId merchantOpCityId =
  DAT.triggerRealtime driverAlertHandle audiences $
    buildOnboardingContent entityType entityId action title body requestorId merchantId merchantOpCityId

notifyAdminsRealtime ::
  DashboardAlertFlow m r =>
  DAlertEntity.AlertEntityType ->
  Text ->
  DOnboardingAlertAction.OnboardingAlertAction ->
  Text ->
  Text ->
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
  Text ->
  Text ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
notifyFleetOwnerRealtime fleetOwnerId = notifyOnboardingChange [DAA.FleetOwnerAudience (cast fleetOwnerId)]
