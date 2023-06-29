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

import Domain.Types.Merchant
import Domain.Types.Merchant.OnboardingDocumentConfig
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Merchant.OnboardingDocumentConfig

-- updateDeviceToken :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe FCMRecipientToken -> m (MeshResult ())
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

create :: OnboardingDocumentConfig -> SqlDB ()
create = Esq.create

findAllByMerchantId :: Transactionable m => Id Merchant -> m [OnboardingDocumentConfig]
findAllByMerchantId merchantId =
  Esq.findAll $ do
    config <- from $ table @OnboardingDocumentConfigT
    where_ $
      config ^. OnboardingDocumentConfigMerchantId ==. val (toKey merchantId)
    return config

update :: OnboardingDocumentConfig -> SqlDB ()
update config = do
  now <- getCurrentTime
  let supportedClassJson = getConfigJSON config.supportedVehicleClasses
  Esq.update $ \tbl -> do
    set
      tbl
      [ OnboardingDocumentConfigCheckExtraction =. val config.checkExtraction,
        OnboardingDocumentConfigCheckExpiry =. val config.checkExpiry,
        OnboardingDocumentConfigSupportedVehicleClassesJSON =. val supportedClassJson,
        OnboardingDocumentConfigVehicleClassCheckType =. val config.vehicleClassCheckType,
        OnboardingDocumentConfigRcNumberPrefix =. val config.rcNumberPrefix,
        OnboardingDocumentConfigUpdatedAt =. val now
      ]
    where_ $ tbl ^. OnboardingDocumentConfigTId ==. val (toKey (config.merchantId, config.documentType))
