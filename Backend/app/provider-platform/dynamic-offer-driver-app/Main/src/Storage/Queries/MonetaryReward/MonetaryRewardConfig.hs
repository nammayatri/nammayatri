{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MonetaryReward.MonetaryRewardConfig where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MonetaryReward.MonetaryRewardConfig
import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Sequelize as Se
import qualified Storage.Beam.MonetaryReward.MonetaryRewardConfig as BeamMRC

fetchMonetaryRewardConfigByFunctionAndMerchant :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m (Maybe MonetaryRewardConfig)
fetchMonetaryRewardConfigByFunctionAndMerchant eventFunction (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMRC.eventFunction $ Se.Eq eventFunction,
          Se.Is BeamMRC.merchantId $ Se.Eq merchantId,
          Se.Is BeamMRC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MonetaryRewardConfig -> m (Maybe MonetaryRewardConfig)
findById (Id monetaryRewardConfigId) =
  findOneWithKV [Se.Is BeamMRC.id $ Se.Eq monetaryRewardConfigId]

createMonetaryRewardEntries :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MonetaryRewardConfig -> m ()
createMonetaryRewardEntries = createWithKV

fetchMonetaryRewardFunctionsOnEventbasis :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m [MonetaryRewardConfig]
fetchMonetaryRewardFunctionsOnEventbasis eventType (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  let dbEventName = show eventType
  findAllWithKV
    [ Se.And
        [ Se.Is BeamMRC.eventName $ Se.Eq dbEventName,
          Se.Is BeamMRC.merchantId $ Se.Eq merchantId,
          Se.Is BeamMRC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

fetchMonetaryRewardConfigOnEventAndFunctionBasis :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m (Maybe MonetaryRewardConfig)
fetchMonetaryRewardConfigOnEventAndFunctionBasis eventType eventFunction (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  let dbEventName = show eventType
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMRC.eventFunction $ Se.Eq eventFunction,
          Se.Is BeamMRC.eventName $ Se.Eq dbEventName,
          Se.Is BeamMRC.merchantId $ Se.Eq merchantId,
          Se.Is BeamMRC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

getActiveMonetaryRewardConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> m [MonetaryRewardConfig]
getActiveMonetaryRewardConfigs (Id merchantId) (Id merchantOptCityId) vehicleCategory = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamMRC.merchantId $ Se.Eq merchantId,
          Se.Is BeamMRC.merchantOptCityId $ Se.Eq merchantOptCityId,
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq (Just vehicleCategory)
        ]
    ]

instance FromTType' BeamMRC.MonetaryRewardConfig MonetaryRewardConfig where
  fromTType' BeamMRC.MonetaryRewardConfigT {..} = do
    pure $
      Just
        MonetaryRewardConfig
          { id = Id id,
            eventName = eventName,
            eventFunction = eventFunction,
            merchantId = merchantId,
            merchantOptCityId = merchantOptCityId,
            monetaryRewardAmount = monetaryRewardAmount,
            expirationAt = expirationAt,
            active = active,
            vehicleCategory = vehicleCategory
          }

instance ToTType' BeamMRC.MonetaryRewardConfig MonetaryRewardConfig where
  toTType' MonetaryRewardConfig {..} = do
    BeamMRC.MonetaryRewardConfigT
      { BeamMRC.id = getId id,
        BeamMRC.eventName = eventName,
        BeamMRC.eventFunction = eventFunction,
        BeamMRC.merchantId = merchantId,
        BeamMRC.merchantOptCityId = merchantOptCityId,
        BeamMRC.monetaryRewardAmount = monetaryRewardAmount,
        BeamMRC.expirationAt = expirationAt,
        BeamMRC.active = active,
        BeamMRC.vehicleCategory = vehicleCategory
      }
