{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FareProduct
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Domain.Types.FareProduct as Domain
import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle.Variant (Variant (..))
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.FareProduct as BeamFareProduct

-- findAllFareProductForVariants ::
--   Transactionable m =>
--   Id Merchant ->
--   Domain.Area ->
--   m [Domain.FareProduct]
-- findAllFareProductForVariants merchantId area =
--   Esq.findAll $ do
--     fareProduct <- from $ table @FareProductT
--     where_ $
--       fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
--         &&. fareProduct ^. FareProductArea ==. val area
--     return fareProduct

findAllFareProductForVariants ::
  L.MonadFlow m =>
  Id Merchant ->
  Domain.Area ->
  m [Domain.FareProduct]
findAllFareProductForVariants (Id merchantId) area = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFareProduct.FareProductT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.And [Se.Is BeamFareProduct.merchantId $ Se.Eq merchantId, Se.Is BeamFareProduct.area $ Se.Eq area]]
      case result of
        Right x -> pure $ transformBeamFareProductToDomain <$> x
        Left _ -> pure []
    Nothing -> pure []

-- findByMerchantVariantArea ::
--   Transactionable m =>
--   Id Merchant ->
--   Variant ->
--   Domain.Area ->
--   m (Maybe Domain.FareProduct)
-- findByMerchantVariantArea merchantId vehicleVariant area =
--   Esq.findOne $ do
--     fareProduct <- from $ table @FareProductT
--     where_ $
--       fareProduct ^. FareProductMerchantId ==. val (toKey merchantId)
--         &&. fareProduct ^. FareProductArea ==. val area
--         &&. fareProduct ^. FareProductVehicleVariant ==. val vehicleVariant
--     return fareProduct

findByMerchantVariantArea ::
  (L.MonadFlow m) =>
  Id Merchant ->
  Variant ->
  Domain.Area ->
  m (Maybe Domain.FareProduct)
findByMerchantVariantArea (Id merchantId) vehicleVariant area = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFareProduct.FareProductT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              [ Se.Is BeamFareProduct.merchantId $ Se.Eq merchantId,
                Se.Is BeamFareProduct.area $ Se.Eq area,
                Se.Is BeamFareProduct.vehicleVariant $ Se.Eq vehicleVariant
              ]
          ]
      case result of
        Right (Just x) -> pure $ Just $ transformBeamFareProductToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

transformDomainFareProductToBeam :: Domain.FareProduct -> BeamFareProduct.FareProduct
transformDomainFareProductToBeam Domain.FareProduct {..} =
  BeamFareProduct.FareProductT
    { BeamFareProduct.id = getId id,
      merchantId = getId merchantId,
      farePolicyId = getId farePolicyId,
      vehicleVariant = vehicleVariant,
      area = area,
      flow = flow
    }

transformBeamFareProductToDomain :: BeamFareProduct.FareProduct -> Domain.FareProduct
transformBeamFareProductToDomain BeamFareProduct.FareProductT {..} =
  Domain.FareProduct
    { id = Id id,
      merchantId = Id merchantId,
      farePolicyId = Id farePolicyId,
      vehicleVariant = vehicleVariant,
      area = area,
      flow = flow
    }
