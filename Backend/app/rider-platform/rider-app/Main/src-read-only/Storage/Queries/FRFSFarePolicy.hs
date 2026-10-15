{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSFarePolicy where

import qualified Domain.Types.FRFSFarePolicy
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSFarePolicy as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSFarePolicy.FRFSFarePolicy] -> m ())
createMany = traverse_ create

findAllByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy] -> m [Domain.Types.FRFSFarePolicy.FRFSFarePolicy])
findAllByIds id = do findAllWithKV [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> id)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> m (Maybe Domain.Types.FRFSFarePolicy.FRFSFarePolicy))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> m (Maybe Domain.Types.FRFSFarePolicy.FRFSFarePolicy))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> m ())
updateByPrimaryKey (Domain.Types.FRFSFarePolicy.FRFSFarePolicy {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type _type,
      Se.Set Beam.applicableDiscountIds (Kernel.Types.Id.getId <$> applicableDiscountIds),
      Se.Set Beam.cessCharge cessCharge,
      Se.Set Beam.description description,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSFarePolicy Domain.Types.FRFSFarePolicy.FRFSFarePolicy where
  fromTType' (Beam.FRFSFarePolicyT {..}) = do
    pure $
      Just
        Domain.Types.FRFSFarePolicy.FRFSFarePolicy
          { _type = _type,
            applicableDiscountIds = Kernel.Types.Id.Id <$> applicableDiscountIds,
            cessCharge = cessCharge,
            description = description,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSFarePolicy Domain.Types.FRFSFarePolicy.FRFSFarePolicy where
  toTType' (Domain.Types.FRFSFarePolicy.FRFSFarePolicy {..}) = do
    Beam.FRFSFarePolicyT
      { Beam._type = _type,
        Beam.applicableDiscountIds = Kernel.Types.Id.getId <$> applicableDiscountIds,
        Beam.cessCharge = cessCharge,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
