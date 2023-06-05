{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.MerchantServiceUsageConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceUsageConfig
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantServiceUsageConfig as BeamMSUC

-- findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
-- findByMerchantId orgId =
--   Esq.findOne $ do
--     orgMapsCfg <- from $ table @MerchantServiceUsageConfigT
--     where_ $
--       orgMapsCfg ^. MerchantServiceUsageConfigTId ==. val (toKey orgId)
--     return orgMapsCfg

findByMerchantId :: L.MonadFlow m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamMerchantServiceUsageConfigToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamMSUC.merchantId $ Se.Eq merchantId]
    Nothing -> pure Nothing

-- updateMerchantServiceUsageConfig ::
--   MerchantServiceUsageConfig ->
--   SqlDB ()
-- updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ MerchantServiceUsageConfigGetDistances =. val getDistances,
--         MerchantServiceUsageConfigGetEstimatedPickupDistances =. val getEstimatedPickupDistances,
--         MerchantServiceUsageConfigGetRoutes =. val getRoutes,
--         MerchantServiceUsageConfigSnapToRoad =. val snapToRoad,
--         MerchantServiceUsageConfigGetPlaceName =. val getPlaceName,
--         MerchantServiceUsageConfigGetPlaceDetails =. val getPlaceDetails,
--         MerchantServiceUsageConfigAutoComplete =. val autoComplete,
--         MerchantServiceUsageConfigSmsProvidersPriorityList =. val (PostgresList smsProvidersPriorityList),
--         MerchantServiceUsageConfigUpdatedAt =. val now
--       ]
--     where_ $
--       tbl ^. MerchantServiceUsageConfigTId ==. val (toKey merchantId)

updateMerchantServiceUsageConfig :: (L.MonadFlow m, MonadTime m) => MerchantServiceUsageConfig -> m (MeshResult ())
updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamMSUC.getDistances getDistances,
          Se.Set BeamMSUC.getEstimatedPickupDistances getEstimatedPickupDistances,
          Se.Set BeamMSUC.getRoutes getRoutes,
          Se.Set BeamMSUC.snapToRoad snapToRoad,
          Se.Set BeamMSUC.getPlaceName getPlaceName,
          Se.Set BeamMSUC.getPlaceDetails getPlaceDetails,
          Se.Set BeamMSUC.autoComplete autoComplete,
          Se.Set BeamMSUC.smsProvidersPriorityList smsProvidersPriorityList,
          Se.Set BeamMSUC.updatedAt now
        ]
        [Se.Is BeamMSUC.merchantId (Se.Eq $ getId merchantId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamMerchantServiceUsageConfigToDomain :: BeamMSUC.MerchantServiceUsageConfig -> MerchantServiceUsageConfig
transformBeamMerchantServiceUsageConfigToDomain BeamMSUC.MerchantServiceUsageConfigT {..} = do
  MerchantServiceUsageConfig
    { merchantId = Id merchantId,
      initiateCall = initiateCall,
      getDistances = getDistances,
      getEstimatedPickupDistances = getEstimatedPickupDistances,
      getRoutes = getRoutes,
      getPickupRoutes = getPickupRoutes,
      getTripRoutes = getTripRoutes,
      snapToRoad = snapToRoad,
      getPlaceName = getPlaceName,
      getPlaceDetails = getPlaceDetails,
      autoComplete = autoComplete,
      getDistancesForCancelRide = getDistancesForCancelRide,
      smsProvidersPriorityList = smsProvidersPriorityList,
      whatsappProvidersPriorityList = whatsappProvidersPriorityList,
      verificationService = verificationService,
      updatedAt = updatedAt,
      createdAt = createdAt
    }

transformDomainMerchantServiceUsageConfigToBeam :: MerchantServiceUsageConfig -> BeamMSUC.MerchantServiceUsageConfig
transformDomainMerchantServiceUsageConfigToBeam MerchantServiceUsageConfig {..} =
  BeamMSUC.MerchantServiceUsageConfigT
    { BeamMSUC.merchantId = getId merchantId,
      BeamMSUC.initiateCall = initiateCall,
      BeamMSUC.getDistances = getDistances,
      BeamMSUC.getEstimatedPickupDistances = getEstimatedPickupDistances,
      BeamMSUC.getRoutes = getRoutes,
      BeamMSUC.getPickupRoutes = getPickupRoutes,
      BeamMSUC.getTripRoutes = getTripRoutes,
      BeamMSUC.snapToRoad = snapToRoad,
      BeamMSUC.getPlaceName = getPlaceName,
      BeamMSUC.getPlaceDetails = getPlaceDetails,
      BeamMSUC.autoComplete = autoComplete,
      BeamMSUC.getDistancesForCancelRide = getDistancesForCancelRide,
      BeamMSUC.smsProvidersPriorityList = smsProvidersPriorityList,
      BeamMSUC.whatsappProvidersPriorityList = whatsappProvidersPriorityList,
      BeamMSUC.verificationService = verificationService,
      BeamMSUC.updatedAt = updatedAt,
      BeamMSUC.createdAt = createdAt
    }
