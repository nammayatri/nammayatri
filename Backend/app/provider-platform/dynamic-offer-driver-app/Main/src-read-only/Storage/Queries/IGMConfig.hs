{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IGMConfig where

import qualified Domain.Types.IGMConfig
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IGMConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IGMConfig.IGMConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IGMConfig.IGMConfig] -> m ())
createMany = traverse_ create

findByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.IGMConfig.IGMConfig))
findByMerchantId (Kernel.Types.Id.Id merchantId) = do findOneWithKV [Se.Is Beam.merchantId $ Se.Eq merchantId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IGMConfig.IGMConfig -> m (Maybe Domain.Types.IGMConfig.IGMConfig))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IGMConfig.IGMConfig -> m ())
updateByPrimaryKey (Domain.Types.IGMConfig.IGMConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.expectedResolutionTime expectedResolutionTime,
      Se.Set Beam.expectedResponseTime expectedResponseTime,
      Se.Set Beam.groEmail groEmail,
      Se.Set Beam.groName groName,
      Se.Set Beam.groPhone groPhone,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IGMConfig Domain.Types.IGMConfig.IGMConfig where
  fromTType' (Beam.IGMConfigT {..}) = do
    pure $
      Just
        Domain.Types.IGMConfig.IGMConfig
          { expectedResolutionTime = expectedResolutionTime,
            expectedResponseTime = expectedResponseTime,
            groEmail = groEmail,
            groName = groName,
            groPhone = groPhone,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IGMConfig Domain.Types.IGMConfig.IGMConfig where
  toTType' (Domain.Types.IGMConfig.IGMConfig {..}) = do
    Beam.IGMConfigT
      { Beam.expectedResolutionTime = expectedResolutionTime,
        Beam.expectedResponseTime = expectedResponseTime,
        Beam.groEmail = groEmail,
        Beam.groName = groName,
        Beam.groPhone = groPhone,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
