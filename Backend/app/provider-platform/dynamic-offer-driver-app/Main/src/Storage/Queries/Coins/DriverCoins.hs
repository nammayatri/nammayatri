{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Coins.DriverCoins where

import Domain.Types.Coins.DriverCoins
import qualified Domain.Types.Merchant as DM
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT (DriverCoinsEventType (..), DriverCoinsFunctionType (..))
import qualified Sequelize as Se
import qualified Storage.Beam.Coins.DriverCoins as BeamDC

fetchCoins :: MonadFlow m => DCT.DriverCoinsFunctionType -> Id DM.Merchant -> m (Maybe DriverCoins)
fetchCoins eventType (Id merchantId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDC.fn $ Se.Eq eventType,
          Se.Is BeamDC.merchantId $ Se.Eq merchantId
        ]
    ]

fetchFunctionsOnEventbasis :: MonadFlow m => DCT.DriverCoinsEventType -> Id DM.Merchant -> m [DriverCoins]
fetchFunctionsOnEventbasis eventType (Id merchantId) = do
  let dbEventName =
        case eventType of
          DCT.Rating {} -> "Rating"
          DCT.EndRide {} -> "EndRide"
          DCT.Cancellation {} -> "Cancellation"
          DCT.Referral {} -> "Referral"
          DCT.LeaderBoard -> "LeaderBoard"
          DCT.Training -> "Training"
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDC.eventName $ Se.Eq dbEventName,
          Se.Is BeamDC.merchantId $ Se.Eq merchantId,
          Se.Is BeamDC.active $ Se.Eq True
        ]
    ]

getCoinInfo :: MonadFlow m => Id DM.Merchant -> m [DriverCoins]
getCoinInfo (Id merchantId) = findAllWithKV [Se.Is BeamDC.merchantId $ Se.Eq merchantId]

instance FromTType' BeamDC.DriverCoins DriverCoins where
  fromTType' BeamDC.DriverCoinsT {..} = do
    pure $
      Just
        DriverCoins
          { id = id,
            eventName = eventName,
            fn = fn,
            merchantId = merchantId,
            coins = coins,
            expirationAt = expirationAt,
            trackExpiry = trackExpiry,
            active = active
          }

instance ToTType' BeamDC.DriverCoins DriverCoins where
  toTType' DriverCoins {..} = do
    BeamDC.DriverCoinsT
      { BeamDC.id = id,
        BeamDC.eventName = eventName,
        BeamDC.fn = fn,
        BeamDC.merchantId = merchantId,
        BeamDC.coins = coins,
        BeamDC.expirationAt = expirationAt,
        BeamDC.trackExpiry = trackExpiry,
        BeamDC.active = active
      }
