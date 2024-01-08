{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Coins.PurchaseHistory where

import Domain.Types.Coins.PurchaseHistory
import qualified Domain.Types.Person as SP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Coins.PurchaseHistory as BeamDC

getPurchasedHistory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> Maybe Integer -> Maybe Integer -> m [PurchaseHistory]
getPurchasedHistory (Id driverId) mbLimit mbOffset = do
  let limitVal = maybe 10 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
  findAllWithOptionsKV
    [Se.Is BeamDC.driverId $ Se.Eq driverId]
    (Se.Desc BeamDC.createdAt)
    (Just limitVal)
    (Just offsetVal)

createPurchaseHistory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PurchaseHistory -> m ()
createPurchaseHistory = createWithKV

instance FromTType' BeamDC.PurchaseHistory PurchaseHistory where
  fromTType' BeamDC.PurchaseHistoryT {..} = do
    pure $
      Just
        PurchaseHistory
          { id = Id id,
            driverId = driverId,
            createdAt = createdAt,
            numCoins = numCoins,
            merchantId = merchantId,
            merchantOptCityId = merchantOptCityId,
            cash = cash,
            title = title,
            updatedAt = updatedAt
          }

instance ToTType' BeamDC.PurchaseHistory PurchaseHistory where
  toTType' PurchaseHistory {..} = do
    BeamDC.PurchaseHistoryT
      { BeamDC.id = getId id,
        BeamDC.driverId = driverId,
        BeamDC.merchantId = merchantId,
        BeamDC.merchantOptCityId = merchantOptCityId,
        BeamDC.createdAt = createdAt,
        BeamDC.numCoins = numCoins,
        BeamDC.cash = cash,
        BeamDC.title = title,
        BeamDC.updatedAt = updatedAt
      }
