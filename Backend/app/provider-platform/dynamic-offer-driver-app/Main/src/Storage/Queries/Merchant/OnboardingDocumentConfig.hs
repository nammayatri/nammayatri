{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant.OnboardingDocumentConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Data.List
import Domain.Types.Merchant
import Domain.Types.Merchant.OnboardingDocumentConfig
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as Domain
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (MeshError (MKeyNotFound), MeshResult)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.OnboardingDocumentConfig as BeamODC

create :: L.MonadFlow m => OnboardingDocumentConfig -> m (MeshResult ())
create config = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamODC.OnboardingDocumentConfigT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainOnboardingDocumentConfigToBeam config)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findAllByMerchantId :: Transactionable m => Id Merchant -> m [OnboardingDocumentConfig]
-- findAllByMerchantId merchantId =
--   Esq.findAll $ do
--     config <- from $ table @OnboardingDocumentConfigT
--     where_ $
--       config ^. OnboardingDocumentConfigMerchantId ==. val (toKey merchantId)
--     return config

findAllByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m [OnboardingDocumentConfig]
findAllByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamODC.OnboardingDocumentConfigT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      result <-
        KV.findAllWithKVConnector
          dbCOnf'
          updatedMeshConfig
          [Se.Is BeamODC.merchantId $ Se.Eq merchantId]
      case result of
        Left _ -> pure []
        Right result' -> mapM transformBeamOnboardingDocumentConfigToDomain result'
    Nothing -> pure []

-- update :: OnboardingDocumentConfig -> SqlDB ()
-- update config = do
--   now <- getCurrentTime
--   let supportedClassJson = getConfigJSON config.supportedVehicleClasses
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ OnboardingDocumentConfigCheckExtraction =. val config.checkExtraction,
--         OnboardingDocumentConfigCheckExpiry =. val config.checkExpiry,
--         OnboardingDocumentConfigSupportedVehicleClassesJSON =. val supportedClassJson,
--         OnboardingDocumentConfigVehicleClassCheckType =. val config.vehicleClassCheckType,
--         OnboardingDocumentConfigRcNumberPrefix =. val config.rcNumberPrefix,
--         OnboardingDocumentConfigUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. OnboardingDocumentConfigTId ==. val (toKey (config.merchantId, config.documentType))

-- -- updateDeviceToken :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe FCMRecipientToken -> m (MeshResult ())
-- updateDeviceToken (Id personId) mbDeviceToken = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamP.PersonT
--   let updatedMeshConfig = setMeshConfig modelName
--   now <- getCurrentTime
--   case dbConf of
--     Just dbConf' ->
--       KV.updateWoReturningWithKVConnector
--         dbConf'
--         updatedMeshConfig
--         [ Se.Set BeamP.deviceToken mbDeviceToken,
--           Se.Set BeamP.updatedAt now
--         ]
--         [Se.Is BeamP.id (Se.Eq personId)]
--     Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

--complete this function after tranformations is done
update :: (L.MonadFlow m, MonadTime m) => OnboardingDocumentConfig -> m (MeshResult ())
update config = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamODC.OnboardingDocumentConfigT
  let updatedMeshConfig = setMeshConfig modelName
  let supportedClassJson = BeamODC.getConfigJSON config.supportedVehicleClasses
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamODC.checkExtraction (config.checkExtraction),
          Se.Set BeamODC.checkExpiry (config.checkExpiry),
          Se.Set BeamODC.supportedVehicleClassesJSON supportedClassJson,
          Se.Set BeamODC.vehicleClassCheckType (config.vehicleClassCheckType),
          Se.Set BeamODC.rcNumberPrefix (config.rcNumberPrefix),
          Se.Set BeamODC.updatedAt now
        ]
        [Se.Is BeamODC.merchantId $ Se.Eq $ getId config.merchantId]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamOnboardingDocumentConfigToDomain :: (L.MonadFlow m, Log m) => BeamODC.OnboardingDocumentConfig -> m OnboardingDocumentConfig
transformBeamOnboardingDocumentConfigToDomain BeamODC.OnboardingDocumentConfigT {..} = do
  supportedVehicleClasses' <- maybe (throwError $ InternalError "Unable to decode OnboardingDocumentConfigT.supportedVehicleClasses") return $ case documentType of
    Domain.DL -> Domain.DLValidClasses <$> decodeFromText supportedVehicleClassesJSON
    Domain.RC -> Domain.RCValidClasses . sortOnCapcity <$> decodeFromText supportedVehicleClassesJSON
    _ -> Just $ Domain.RCValidClasses []
  pure $
    OnboardingDocumentConfig
      { merchantId = Id merchantId,
        documentType = documentType,
        checkExtraction = checkExtraction,
        checkExpiry = checkExpiry,
        supportedVehicleClasses = supportedVehicleClasses',
        vehicleClassCheckType = vehicleClassCheckType,
        rcNumberPrefix = rcNumberPrefix,
        createdAt = createdAt,
        updatedAt = updatedAt
      }
  where
    sortOnCapcity = Data.List.sortBy (\a b -> compare b.vehicleCapacity a.vehicleCapacity)

transformDomainOnboardingDocumentConfigToBeam :: OnboardingDocumentConfig -> BeamODC.OnboardingDocumentConfig
transformDomainOnboardingDocumentConfigToBeam OnboardingDocumentConfig {..} =
  BeamODC.OnboardingDocumentConfigT
    { BeamODC.merchantId = getId merchantId,
      BeamODC.documentType = documentType,
      BeamODC.checkExtraction = checkExtraction,
      BeamODC.checkExpiry = checkExpiry,
      BeamODC.supportedVehicleClassesJSON = BeamODC.getConfigJSON supportedVehicleClasses,
      BeamODC.vehicleClassCheckType = vehicleClassCheckType,
      BeamODC.rcNumberPrefix = rcNumberPrefix,
      BeamODC.createdAt = createdAt,
      BeamODC.updatedAt = updatedAt
    }
