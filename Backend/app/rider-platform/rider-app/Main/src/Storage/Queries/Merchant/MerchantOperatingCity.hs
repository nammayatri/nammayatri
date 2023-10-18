{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.MerchantOperatingCity
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantOperatingCity as BeamMOC

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity)
findById (Id id) = findOneWithKV [Se.Is BeamMOC.id $ Se.Eq id]

findByMerchantShortIdAndCity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => ShortId DM.Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
findByMerchantShortIdAndCity (ShortId merchantShortId) city = findOneWithKV [Se.And [Se.Is BeamMOC.merchantShortId $ Se.Eq merchantShortId, Se.Is BeamMOC.city $ Se.Eq city]]

findByMerchantIdAndCity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Context.City -> m (Maybe MerchantOperatingCity)
findByMerchantIdAndCity (Id merchantId) city = findOneWithKV [Se.And [Se.Is BeamMOC.merchantId $ Se.Eq merchantId, Se.Is BeamMOC.city $ Se.Eq city]]

instance FromTType' BeamMOC.MerchantOperatingCity MerchantOperatingCity where
  fromTType' BeamMOC.MerchantOperatingCityT {..} = do
    pure $
      Just
        MerchantOperatingCity
          { id = Id id,
            merchantId = Id merchantId,
            merchantShortId = ShortId merchantShortId,
            ..
          }

instance ToTType' BeamMOC.MerchantOperatingCity MerchantOperatingCity where
  toTType' MerchantOperatingCity {..} = do
    BeamMOC.MerchantOperatingCityT
      { BeamMOC.id = getId id,
        BeamMOC.merchantId = getId merchantId,
        BeamMOC.merchantShortId = getShortId merchantShortId,
        BeamMOC.city = city
      }
