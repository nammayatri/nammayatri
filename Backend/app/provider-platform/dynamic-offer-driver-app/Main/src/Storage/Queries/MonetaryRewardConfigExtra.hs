{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MonetaryRewardConfigExtra where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MonetaryRewardConfig
import Domain.Types.VehicleCategory as DTV
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.DriverCoins.Types as DCT
import qualified Sequelize as Se
import qualified Storage.Beam.MonetaryRewardConfig as BeamMRC
import Storage.Queries.OrphanInstances.MonetaryRewardConfig ()

fetchMonetaryRewardConfigByFunctionAndMerchant :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m (Maybe MonetaryRewardConfig)
fetchMonetaryRewardConfigByFunctionAndMerchant eventFunction merchantId merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMRC.eventFunction $ Se.Eq eventFunction,
          Se.Is BeamMRC.merchantId $ Se.Eq (getId merchantId),
          Se.Is BeamMRC.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

fetchMonetaryRewardFunctionsOnEventbasis :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m [MonetaryRewardConfig]
fetchMonetaryRewardFunctionsOnEventbasis eventType merchantId merchantOperatingCityId vehicleCategory = do
  let dbEventName = show eventType
  findAllWithKV
    [ Se.And
        [ Se.Is BeamMRC.eventName $ Se.Eq dbEventName,
          Se.Is BeamMRC.merchantId $ Se.Eq (getId merchantId),
          Se.Is BeamMRC.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

fetchMonetaryRewardConfigOnEventAndFunctionBasis :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DCT.DriverCoinsEventType -> DCT.DriverCoinsFunctionType -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe DTV.VehicleCategory -> m (Maybe MonetaryRewardConfig)
fetchMonetaryRewardConfigOnEventAndFunctionBasis eventType eventFunction merchantId merchantOperatingCityId vehicleCategory = do
  let dbEventName = show eventType
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMRC.eventFunction $ Se.Eq eventFunction,
          Se.Is BeamMRC.eventName $ Se.Eq dbEventName,
          Se.Is BeamMRC.merchantId $ Se.Eq (getId merchantId),
          Se.Is BeamMRC.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

getActiveMonetaryRewardConfigs :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTV.VehicleCategory -> m [MonetaryRewardConfig]
getActiveMonetaryRewardConfigs merchantId merchantOperatingCityId vehicleCategory = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamMRC.merchantId $ Se.Eq (getId merchantId),
          Se.Is BeamMRC.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is BeamMRC.active $ Se.Eq True,
          Se.Is BeamMRC.vehicleCategory $ Se.Eq (Just vehicleCategory)
        ]
    ]
