{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StateEntryPermitCharges where

import qualified Domain.Types.StateEntryPermitCharges
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.StateEntryPermitCharges as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges] -> m ())
createMany = traverse_ create

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges -> m (Maybe Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges -> m ())
updateByPrimaryKey (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.geomId (Kernel.Types.Id.getId geomId),
      Se.Set Beam.name name,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.StateEntryPermitCharges Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges where
  fromTType' (Beam.StateEntryPermitChargesT {..}) = do
    pure $
      Just
        Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges
          { amount = amount,
            createdAt = createdAt,
            geomId = Kernel.Types.Id.Id geomId,
            id = Kernel.Types.Id.Id id,
            name = name,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.StateEntryPermitCharges Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges where
  toTType' (Domain.Types.StateEntryPermitCharges.StateEntryPermitCharges {..}) = do
    Beam.StateEntryPermitChargesT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.geomId = Kernel.Types.Id.getId geomId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
