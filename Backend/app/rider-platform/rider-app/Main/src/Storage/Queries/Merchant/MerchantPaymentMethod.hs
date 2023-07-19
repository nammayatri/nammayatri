{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant.MerchantPaymentMethod
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantPaymentMethod
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantPaymentMethod as BeamMPM

-- findAllByMerchantId :: Transactionable m => Id Merchant -> m [MerchantPaymentMethod]
-- findAllByMerchantId merchantId =
--   Esq.findAll $ do
--     merchantPaymentMethod <- from $ table @MerchantPaymentMethodT
--     where_ $
--       merchantPaymentMethod ^. MerchantPaymentMethodMerchantId ==. val (toKey merchantId)
--     orderBy [desc $ merchantPaymentMethod ^. MerchantPaymentMethodPriority]
--     return merchantPaymentMethod

findAllByMerchantId :: L.MonadFlow m => Id Merchant -> m [MerchantPaymentMethod]
findAllByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamMPM.MerchantPaymentMethodT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure []) (transformBeamMerchantPaymentMethodToDomain <$>)
        <$> KV.findAllWithOptionsKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamMPM.merchantId $ Se.Eq merchantId]
          (Se.Desc BeamMPM.priority)
          Nothing
          Nothing
    Nothing -> pure []

transformBeamMerchantPaymentMethodToDomain :: BeamMPM.MerchantPaymentMethod -> MerchantPaymentMethod
transformBeamMerchantPaymentMethodToDomain BeamMPM.MerchantPaymentMethodT {..} = do
  MerchantPaymentMethod
    { id = Id id,
      merchantId = Id merchantId,
      paymentType = paymentType,
      paymentInstrument = paymentInstrument,
      collectedBy = collectedBy,
      priority = priority,
      updatedAt = updatedAt,
      createdAt = createdAt
    }

transformDomainMerchantPaymentMethodToBeam :: MerchantPaymentMethod -> BeamMPM.MerchantPaymentMethod
transformDomainMerchantPaymentMethodToBeam MerchantPaymentMethod {..} =
  BeamMPM.MerchantPaymentMethodT
    { BeamMPM.id = getId id,
      BeamMPM.merchantId = getId merchantId,
      BeamMPM.paymentType = paymentType,
      BeamMPM.paymentInstrument = paymentInstrument,
      BeamMPM.collectedBy = collectedBy,
      BeamMPM.priority = priority,
      BeamMPM.updatedAt = updatedAt,
      BeamMPM.createdAt = createdAt
    }
