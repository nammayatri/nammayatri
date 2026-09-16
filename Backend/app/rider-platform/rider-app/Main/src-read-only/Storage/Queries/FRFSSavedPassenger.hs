{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSSavedPassenger where

import qualified Domain.Types.FRFSSavedPassenger
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSavedPassenger as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger] -> m ([Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger]))
findAllByIds id = do findAllWithKV [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> id)]

findAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger]))
findAllByRiderId riderId = do findAllWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger -> m (Maybe Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger -> m ())
updateByPrimaryKey (Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.age age,
      Se.Set Beam.gender gender,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSSavedPassenger Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger where
  fromTType' (Beam.FRFSSavedPassengerT {..}) = do
    pure $
      Just
        Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger
          { age = age,
            createdAt = createdAt,
            gender = gender,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            riderId = Kernel.Types.Id.Id riderId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSSavedPassenger Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger where
  toTType' (Domain.Types.FRFSSavedPassenger.FRFSSavedPassenger {..}) = do
    Beam.FRFSSavedPassengerT
      { Beam.age = age,
        Beam.createdAt = createdAt,
        Beam.gender = gender,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.updatedAt = updatedAt
      }
