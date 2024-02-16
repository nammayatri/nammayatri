{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Coins.CoinsConfig where

import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT (DriverCoinsEventType (..), DriverCoinsFunctionType (..))
import qualified Sequelize as Se
import qualified Storage.Beam.Coins.CoinsConfig as BeamDC

fetchCoins :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsFunctionType -> Id DM.Merchant -> m (Maybe CoinsConfig)
fetchCoins eventFunction (Id merchantId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDC.eventFunction $ Se.Eq eventFunction,
          Se.Is BeamDC.merchantId $ Se.Eq merchantId
        ]
    ]

fetchFunctionsOnEventbasis :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m [CoinsConfig]
fetchFunctionsOnEventbasis eventType (Id merchantId) (Id merchantOptCityId) = do
  let dbEventName =
        case eventType of
          DCT.Rating {} -> "Rating"
          DCT.EndRide {} -> "EndRide"
          DCT.Cancellation {} -> "Cancellation"
          DCT.DriverToCustomerReferral {} -> "DriverToCustomerReferral"
          DCT.CustomerToDriverReferral {} -> "CustomerToDriverReferral"
          DCT.LeaderBoard -> "LeaderBoard"
          DCT.Training -> "Training"
          DCT.BulkUploadEvent -> "BulkUploadEvent"
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDC.eventName $ Se.Eq dbEventName,
          Se.Is BeamDC.merchantId $ Se.Eq merchantId,
          Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamDC.active $ Se.Eq True
        ]
    ]

getCoinInfo :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m [CoinsConfig]
getCoinInfo (Id merchantId) = findAllWithKV [Se.Is BeamDC.merchantId $ Se.Eq merchantId]

instance FromTType' BeamDC.CoinsConfig CoinsConfig where
  fromTType' BeamDC.CoinsConfigT {..} = do
    pure $
      Just
        CoinsConfig
          { id = Id id,
            eventName = eventName,
            eventFunction = eventFunction,
            merchantId = merchantId,
            merchantOptCityId = merchantOptCityId,
            coins = coins,
            expirationAt = expirationAt,
            active = active
          }

instance ToTType' BeamDC.CoinsConfig CoinsConfig where
  toTType' CoinsConfig {..} = do
    BeamDC.CoinsConfigT
      { BeamDC.id = getId id,
        BeamDC.eventName = eventName,
        BeamDC.eventFunction = eventFunction,
        BeamDC.merchantId = merchantId,
        BeamDC.merchantOptCityId = merchantOptCityId,
        BeamDC.coins = coins,
        BeamDC.expirationAt = expirationAt,
        BeamDC.active = active
      }
