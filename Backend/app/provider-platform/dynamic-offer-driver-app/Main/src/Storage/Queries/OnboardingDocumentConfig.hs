{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.OnboardingDocumentConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.OnboardingDocumentConfig as DODC
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.OnboardingDocumentConfig as BeamODC
import Storage.Tabular.OnboardingDocumentConfig
import qualified Storage.Tabular.VechileNew as VN

create :: OnboardingDocumentConfig -> SqlDB ()
create = Esq.create

-- create' :: L.MonadFlow m => DODC.OnboardingDocumentConfig -> m (MeshResult ())
-- create' onboardingDocumentConfig = do
--   dbConf <- L.getOption Extra.EulerPsqlDbCfg
--   case dbConf of
--     Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainOnboardingDocumentConfigToBeam onboardingDocumentConfig)
--     Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findByMerchantIdAndDocumentType :: Transactionable m => Id Merchant -> DocumentType -> m (Maybe OnboardingDocumentConfig)
findByMerchantIdAndDocumentType merchantId documentType =
  Esq.findOne $ do
    config <- from $ table @OnboardingDocumentConfigT
    where_ $
      config ^. OnboardingDocumentConfigMerchantId ==. val (toKey merchantId)
        &&. config ^. OnboardingDocumentConfigDocumentType ==. val documentType
    return config

update :: OnboardingDocumentConfig -> SqlDB ()
update config = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ OnboardingDocumentConfigCheckExtraction =. val config.checkExtraction,
        OnboardingDocumentConfigCheckExpiry =. val config.checkExpiry,
        OnboardingDocumentConfigValidVehicleClasses =. val (PostgresList config.validVehicleClasses),
        OnboardingDocumentConfigVehicleClassCheckType =. val config.vehicleClassCheckType,
        OnboardingDocumentConfigUpdatedAt =. val now
      ]
    where_ $ tbl ^. OnboardingDocumentConfigTId ==. val (toKey (config.merchantId, config.documentType))

transformBeamOnboardingDocumentConfigToDomain :: BeamODC.OnboardingDocumentConfig -> OnboardingDocumentConfig
transformBeamOnboardingDocumentConfigToDomain BeamODC.OnboardingDocumentConfigT {..} = do
  OnboardingDocumentConfig
    { merchantId = Id merchantId,
      documentType = documentType,
      checkExtraction = checkExtraction,
      checkExpiry = checkExpiry,
      validVehicleClasses = validVehicleClasses,
      vehicleClassCheckType = vehicleClassCheckType,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainOnboardingDocumentConfigToBeam :: OnboardingDocumentConfig -> BeamODC.OnboardingDocumentConfig
transformDomainOnboardingDocumentConfigToBeam OnboardingDocumentConfig {..} =
  BeamODC.OnboardingDocumentConfigT
    { BeamODC.merchantId = getId merchantId,
      BeamODC.documentType = documentType,
      BeamODC.checkExtraction = checkExtraction,
      BeamODC.checkExpiry = checkExpiry,
      BeamODC.validVehicleClasses = validVehicleClasses,
      BeamODC.vehicleClassCheckType = vehicleClassCheckType,
      BeamODC.createdAt = createdAt,
      BeamODC.updatedAt = updatedAt
    }
