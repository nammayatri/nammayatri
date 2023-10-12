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

getPurchasedHistory :: MonadFlow m => Id SP.Person -> Maybe Int -> Maybe Int -> Bool -> m [PurchaseHistory]
getPurchasedHistory (Id driverId) mbLimit mbOffset hasQuatityLeft = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamDC.driverId $ Se.Eq driverId]
            <> [Se.Is BeamDC.quantityLeft $ Se.GreaterThan 0 | hasQuatityLeft]
        )
    ]
    (Se.Asc BeamDC.createdAt)
    mbLimit
    mbOffset

createPurchaseHistory :: MonadFlow m => PurchaseHistory -> m ()
createPurchaseHistory = createWithKV

updateQuantityLeft :: MonadFlow m => Id PurchaseHistory -> Int -> m ()
updateQuantityLeft purchaseId quantityLeft = do
  updateWithKV
    [Se.Set BeamDC.quantityLeft quantityLeft]
    [Se.Is BeamDC.id $ Se.Eq purchaseId.getId]

instance FromTType' BeamDC.PurchaseHistory PurchaseHistory where
  fromTType' BeamDC.PurchaseHistoryT {..} = do
    pure $
      Just
        PurchaseHistory
          { id = id,
            driverId = driverId,
            coinPlanId = coinPlanId,
            createdAt = createdAt,
            numCoins = numCoins,
            quantity = quantity,
            quantityLeft = quantityLeft
          }

instance ToTType' BeamDC.PurchaseHistory PurchaseHistory where
  toTType' PurchaseHistory {..} = do
    BeamDC.PurchaseHistoryT
      { BeamDC.id = id,
        BeamDC.driverId = driverId,
        BeamDC.coinPlanId = coinPlanId,
        BeamDC.createdAt = createdAt,
        BeamDC.numCoins = numCoins,
        BeamDC.quantity = quantity,
        BeamDC.quantityLeft = quantityLeft
      }
