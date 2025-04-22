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

import API.Types.ProviderPlatform.Management.Endpoints.CoinsConfig (UpdateReq (..))
import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
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

fetchCoinConfigByFunctionAndMerchant :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m (Maybe CoinsConfig)
fetchCoinConfigByFunctionAndMerchant eventFunction (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDC.eventFunction $ Se.Eq eventFunction,
          Se.Is BeamDC.merchantId $ Se.Eq merchantId,
          Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamDC.active $ Se.Eq True,
          Se.Is BeamDC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id CoinsConfig -> m (Maybe CoinsConfig)
findById (Id coinsConfigId) =
  findOneWithKV [Se.Is BeamDC.id $ Se.Eq coinsConfigId]

createCoinEntries :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => CoinsConfig -> m ()
createCoinEntries = createWithKV

updateCoinEntries :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => UpdateReq -> m ()
updateCoinEntries UpdateReq {..} =
  updateWithKV
    [ Se.Set BeamDC.active active,
      Se.Set BeamDC.expirationAt expirationAt,
      Se.Set BeamDC.coins coins
    ]
    [Se.Is BeamDC.id $ Se.Eq $ getId entriesId]

fetchFunctionsOnEventbasis :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m [CoinsConfig]
fetchFunctionsOnEventbasis eventType (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  let dbEventName = show eventType
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDC.eventName $ Se.Eq dbEventName,
          Se.Is BeamDC.merchantId $ Se.Eq merchantId,
          Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamDC.active $ Se.Eq True,
          Se.Is BeamDC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

fetchConfigOnEventAndFunctionBasis :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m (Maybe CoinsConfig)
fetchConfigOnEventAndFunctionBasis eventType eventFunction (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  let dbEventName = show eventType
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDC.eventFunction $ Se.Eq eventFunction,
          Se.Is BeamDC.eventName $ Se.Eq dbEventName,
          Se.Is BeamDC.merchantId $ Se.Eq merchantId,
          Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamDC.active $ Se.Eq True,
          Se.Is BeamDC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

getCoinInfo :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m [CoinsConfig]
getCoinInfo (Id merchantId) = findAllWithKV [Se.Is BeamDC.merchantId $ Se.Eq merchantId]

getActiveCoinConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> m [CoinsConfig]
getActiveCoinConfigs (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDC.merchantId $ Se.Eq merchantId,
          Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamDC.active $ Se.Eq True,
          Se.Is BeamDC.vehicleCategory $ Se.Eq (Just vehicleCategory)
        ]
    ]

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
            active = active,
            vehicleCategory = vehicleCategory
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
        BeamDC.active = active,
        BeamDC.vehicleCategory = vehicleCategory
      }
