{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FareProduct
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FareProduct
import qualified Domain.Types.FareProduct as Domain
import Domain.Types.Merchant (Merchant)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.FareProduct as BeamFP

findAllFareProductForVariants ::
  MonadFlow m =>
  Id Merchant ->
  Domain.Area ->
  m [Domain.FareProduct]
findAllFareProductForVariants (Id merchantId) area = findAllWithKV [Se.And [Se.Is BeamFP.merchantId $ Se.Eq merchantId, Se.Is BeamFP.area $ Se.Eq area]]

findAllFareProductForFlow ::
  MonadFlow m =>
  Id Merchant ->
  Domain.FlowType ->
  m [Domain.FareProduct]
findAllFareProductForFlow (Id merchantId) flow = findAllWithKV [Se.And [Se.Is BeamFP.merchantId $ Se.Eq merchantId, Se.Is BeamFP.flow $ Se.Eq flow]]

findByMerchantVariantArea ::
  MonadFlow m =>
  Id Merchant ->
  Variant ->
  Domain.Area ->
  m (Maybe Domain.FareProduct)
findByMerchantVariantArea (Id merchantId) vehicleVariant area =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamFP.merchantId $ Se.Eq merchantId,
          Se.Is BeamFP.area $ Se.Eq area,
          Se.Is BeamFP.vehicleVariant $ Se.Eq vehicleVariant
        ]
    ]

instance ToTType' BeamFP.FareProduct FareProduct where
  toTType' FareProduct {..} = do
    BeamFP.FareProductT
      { BeamFP.id = getId id,
        merchantId = getId merchantId,
        farePolicyId = getId farePolicyId,
        vehicleVariant = vehicleVariant,
        area = area,
        flow = flow
      }

instance FromTType' BeamFP.FareProduct FareProduct where
  fromTType' BeamFP.FareProductT {..} = do
    pure $
      Just
        Domain.FareProduct
          { id = Id id,
            merchantId = Id merchantId,
            farePolicyId = Id farePolicyId,
            vehicleVariant = vehicleVariant,
            area = area,
            flow = flow
          }
