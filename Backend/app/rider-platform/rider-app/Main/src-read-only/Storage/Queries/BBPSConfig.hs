{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BBPSConfig where

import qualified Domain.Types.BBPSConfig
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BBPSConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPSConfig.BBPSConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BBPSConfig.BBPSConfig] -> m ())
createMany = traverse_ create

findByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.BBPSConfig.BBPSConfig))
findByMerchantId merchantId = do findOneWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.BBPSConfig.BBPSConfig))
findByPrimaryKey merchantId = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPSConfig.BBPSConfig -> m ())
updateByPrimaryKey (Domain.Types.BBPSConfig.BBPSConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bbpsAgentId bbpsAgentId,
      Se.Set Beam.bbpsServerUrl bbpsServerUrl,
      Se.Set Beam.bbpsSignatureKey (bbpsSignatureKey & unEncrypted),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]

instance FromTType' Beam.BBPSConfig Domain.Types.BBPSConfig.BBPSConfig where
  fromTType' (Beam.BBPSConfigT {..}) = do
    pure $
      Just
        Domain.Types.BBPSConfig.BBPSConfig
          { bbpsAgentId = bbpsAgentId,
            bbpsServerUrl = bbpsServerUrl,
            bbpsSignatureKey = Encrypted bbpsSignatureKey,
            merchantId = Kernel.Types.Id.Id merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BBPSConfig Domain.Types.BBPSConfig.BBPSConfig where
  toTType' (Domain.Types.BBPSConfig.BBPSConfig {..}) = do
    Beam.BBPSConfigT
      { Beam.bbpsAgentId = bbpsAgentId,
        Beam.bbpsServerUrl = bbpsServerUrl,
        Beam.bbpsSignatureKey = bbpsSignatureKey & unEncrypted,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
