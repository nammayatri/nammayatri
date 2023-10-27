{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.KioskLocation where

import Domain.Types.KioskLocation
import Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.KioskLocation as BeamK

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KioskLocation -> m ()
create = createWithKV

fetchAllKioskLocationsByMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m [KioskLocation]
fetchAllKioskLocationsByMerchant _ = findAllWithKV [Se.Is BeamK.merchantId $ Se.Not $ Se.Eq ""]

instance FromTType' BeamK.KioskLocation KioskLocation where
  fromTType' BeamK.KioskLocationT {..} = do
    pure $
      Just
        KioskLocation
          { id = Id id,
            merchantId = Id merchantId,
            address = address,
            landmark = landmark,
            contact = contact,
            longitude = longitude,
            latitude = latitude
          }

instance ToTType' BeamK.KioskLocation KioskLocation where
  toTType' KioskLocation {..} = do
    BeamK.KioskLocationT
      { BeamK.id = getId id,
        BeamK.merchantId = getId merchantId,
        BeamK.address = address,
        BeamK.landmark = landmark,
        BeamK.contact = contact,
        BeamK.longitude = longitude,
        BeamK.latitude = latitude
      }
