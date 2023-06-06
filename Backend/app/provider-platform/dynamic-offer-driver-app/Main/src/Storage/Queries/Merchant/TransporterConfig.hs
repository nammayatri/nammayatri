{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.TransporterConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.Merchant.TransporterConfig
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.TransporterConfig as BeamTC

-- findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe TransporterConfig)
-- findByMerchantId merchantId =
--   Esq.findOne $ do
--     config <- from $ table @TransporterConfigT
--     where_ $
--       config ^. TransporterConfigMerchantId ==. val (toKey merchantId)
--     return config

findByMerchantId :: L.MonadFlow m => Id Merchant -> m (Maybe TransporterConfig)
findByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamTC.merchantId $ Se.Eq merchantId]
      case result of
        Left _ -> pure Nothing
        Right tc -> sequence $ transformBeamTransporterConfigToDomain <$> tc
    Nothing -> pure Nothing

-- updateFCMConfig :: Id Merchant -> BaseUrl -> Text -> SqlDB ()
-- updateFCMConfig merchantId fcmUrl fcmServiceAccount = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ TransporterConfigFcmUrl =. val (showBaseUrl fcmUrl),
--         TransporterConfigFcmServiceAccount =. val fcmServiceAccount,
--         TransporterConfigUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. TransporterConfigMerchantId ==. val (toKey merchantId)

updateFCMConfig :: (L.MonadFlow m, MonadTime m) => Id Merchant -> BaseUrl -> Text -> m ()
updateFCMConfig (Id merchantId) fcmUrl fcmServiceAccount = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.Set BeamTC.fcmUrl $ showBaseUrl fcmUrl,
            Se.Set BeamTC.fcmServiceAccount fcmServiceAccount,
            Se.Set BeamTC.updatedAt now
          ]
          [Se.Is BeamTC.merchantId (Se.Eq merchantId)]
    Nothing -> pure ()

-- updateReferralLinkPassword :: Id Merchant -> Text -> SqlDB ()
-- updateReferralLinkPassword merchantId newPassword = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ TransporterConfigReferralLinkPassword =. val newPassword,
--         TransporterConfigUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. TransporterConfigMerchantId ==. val (toKey merchantId)

updateReferralLinkPassword :: (L.MonadFlow m, MonadTime m) => Id Merchant -> Text -> m (MeshResult ())
updateReferralLinkPassword (Id merchantId) newPassword = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamTC.referralLinkPassword newPassword,
          Se.Set BeamTC.updatedAt now
        ]
        [Se.Is BeamTC.merchantId (Se.Eq merchantId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- update :: TransporterConfig -> SqlDB ()
-- update config = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ TransporterConfigPickupLocThreshold =. val config.pickupLocThreshold,
--         TransporterConfigDropLocThreshold =. val config.dropLocThreshold,
--         TransporterConfigRideTimeEstimatedThreshold =. val config.rideTimeEstimatedThreshold,
--         TransporterConfigDefaultPopupDelay =. val config.defaultPopupDelay,
--         TransporterConfigPopupDelayToAddAsPenalty =. val config.popupDelayToAddAsPenalty,
--         TransporterConfigThresholdCancellationScore =. val config.thresholdCancellationScore,
--         TransporterConfigMinRidesForCancellationScore =. val config.minRidesForCancellationScore,
--         TransporterConfigMediaFileUrlPattern =. val config.mediaFileUrlPattern,
--         TransporterConfigMediaFileSizeUpperLimit =. val config.mediaFileSizeUpperLimit,
--         TransporterConfigWaitingTimeEstimatedThreshold =. val config.waitingTimeEstimatedThreshold,
--         TransporterConfigOnboardingTryLimit =. val config.onboardingTryLimit,
--         TransporterConfigOnboardingRetryTimeInHours =. val config.onboardingRetryTimeInHours,
--         TransporterConfigCheckImageExtractionForDashboard =. val config.checkImageExtractionForDashboard,
--         TransporterConfigSearchRepeatLimit =. val config.searchRepeatLimit,
--         TransporterConfigUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. TransporterConfigMerchantId ==. val (toKey config.merchantId)

update :: (L.MonadFlow m, MonadTime m) => TransporterConfig -> m (MeshResult ())
update config = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamTC.pickupLocThreshold config.pickupLocThreshold,
          Se.Set BeamTC.dropLocThreshold config.dropLocThreshold,
          Se.Set BeamTC.rideTimeEstimatedThreshold config.rideTimeEstimatedThreshold,
          Se.Set BeamTC.defaultPopupDelay config.defaultPopupDelay,
          Se.Set BeamTC.popupDelayToAddAsPenalty config.popupDelayToAddAsPenalty,
          Se.Set BeamTC.thresholdCancellationScore config.thresholdCancellationScore,
          Se.Set BeamTC.minRidesForCancellationScore config.minRidesForCancellationScore,
          Se.Set BeamTC.thresholdCancellationPercentageToUnlist config.thresholdCancellationPercentageToUnlist,
          Se.Set BeamTC.minRidesToUnlist config.minRidesToUnlist,
          Se.Set BeamTC.mediaFileUrlPattern config.mediaFileUrlPattern,
          Se.Set BeamTC.mediaFileSizeUpperLimit config.mediaFileSizeUpperLimit,
          Se.Set BeamTC.onboardingTryLimit config.onboardingTryLimit,
          Se.Set BeamTC.onboardingRetryTimeInHours config.onboardingRetryTimeInHours,
          Se.Set BeamTC.checkImageExtractionForDashboard config.checkImageExtractionForDashboard,
          Se.Set BeamTC.searchRepeatLimit config.searchRepeatLimit,
          Se.Set BeamTC.updatedAt now
        ]
        [Se.Is BeamTC.merchantId (Se.Eq $ getId config.merchantId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamTransporterConfigToDomain :: L.MonadFlow m => BeamTC.TransporterConfig -> m TransporterConfig
transformBeamTransporterConfigToDomain BeamTC.TransporterConfigT {..} = do
  fcmUrl' <- parseBaseUrl fcmUrl
  pure
    TransporterConfig
      { merchantId = Id merchantId,
        pickupLocThreshold = pickupLocThreshold,
        dropLocThreshold = dropLocThreshold,
        rideTimeEstimatedThreshold = rideTimeEstimatedThreshold,
        includeDriverCurrentlyOnRide = includeDriverCurrentlyOnRide,
        defaultPopupDelay = defaultPopupDelay,
        popupDelayToAddAsPenalty = popupDelayToAddAsPenalty,
        thresholdCancellationScore = thresholdCancellationScore,
        minRidesForCancellationScore = minRidesForCancellationScore,
        thresholdCancellationPercentageToUnlist = thresholdCancellationPercentageToUnlist,
        minRidesToUnlist = minRidesToUnlist,
        mediaFileUrlPattern = mediaFileUrlPattern,
        mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
        referralLinkPassword = referralLinkPassword,
        fcmConfig =
          FCM.FCMConfig
            { fcmUrl = fcmUrl',
              fcmServiceAccount = fcmServiceAccount,
              fcmTokenKeyPrefix = fcmTokenKeyPrefix
            },
        onboardingTryLimit = onboardingTryLimit,
        onboardingRetryTimeInHours = onboardingRetryTimeInHours,
        checkImageExtractionForDashboard = checkImageExtractionForDashboard,
        searchRepeatLimit = searchRepeatLimit,
        actualRideDistanceDiffThreshold = actualRideDistanceDiffThreshold,
        upwardsRecomputeBuffer = upwardsRecomputeBuffer,
        approxRideDistanceDiffThreshold = approxRideDistanceDiffThreshold,
        driverLeaderBoardExpiry = driverLeaderBoardExpiry,
        minLocationAccuracy = minLocationAccuracy,
        createdAt = createdAt,
        updatedAt = updatedAt
      }

transformDomainTransporterConfigToBeam :: TransporterConfig -> BeamTC.TransporterConfig
transformDomainTransporterConfigToBeam TransporterConfig {..} =
  BeamTC.TransporterConfigT
    { BeamTC.merchantId = getId merchantId,
      BeamTC.pickupLocThreshold = pickupLocThreshold,
      BeamTC.dropLocThreshold = dropLocThreshold,
      BeamTC.rideTimeEstimatedThreshold = rideTimeEstimatedThreshold,
      BeamTC.includeDriverCurrentlyOnRide = includeDriverCurrentlyOnRide,
      BeamTC.defaultPopupDelay = defaultPopupDelay,
      BeamTC.popupDelayToAddAsPenalty = popupDelayToAddAsPenalty,
      BeamTC.thresholdCancellationScore = thresholdCancellationScore,
      BeamTC.minRidesForCancellationScore = minRidesForCancellationScore,
      BeamTC.thresholdCancellationPercentageToUnlist = thresholdCancellationPercentageToUnlist,
      BeamTC.minRidesToUnlist = minRidesToUnlist,
      BeamTC.mediaFileUrlPattern = mediaFileUrlPattern,
      BeamTC.mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
      BeamTC.referralLinkPassword = referralLinkPassword,
      BeamTC.fcmUrl = showBaseUrl $ FCM.fcmUrl fcmConfig,
      BeamTC.fcmServiceAccount = FCM.fcmServiceAccount fcmConfig,
      BeamTC.fcmTokenKeyPrefix = FCM.fcmTokenKeyPrefix fcmConfig,
      BeamTC.onboardingTryLimit = onboardingTryLimit,
      BeamTC.onboardingRetryTimeInHours = onboardingRetryTimeInHours,
      BeamTC.checkImageExtractionForDashboard = checkImageExtractionForDashboard,
      BeamTC.searchRepeatLimit = searchRepeatLimit,
      BeamTC.actualRideDistanceDiffThreshold = actualRideDistanceDiffThreshold,
      BeamTC.upwardsRecomputeBuffer = upwardsRecomputeBuffer,
      BeamTC.approxRideDistanceDiffThreshold = approxRideDistanceDiffThreshold,
      BeamTC.driverLeaderBoardExpiry = driverLeaderBoardExpiry,
      BeamTC.minLocationAccuracy = minLocationAccuracy,
      BeamTC.createdAt = createdAt,
      BeamTC.updatedAt = updatedAt
    }
