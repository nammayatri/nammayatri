{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DeletedPerson where

import qualified Domain.Types.DeletedPerson
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DeletedPerson as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DeletedPerson.DeletedPerson -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DeletedPerson.DeletedPerson] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DeletedPerson.DeletedPerson))
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DeletedPerson.DeletedPerson -> m ())
updateByPrimaryKey (Domain.Types.DeletedPerson.DeletedPerson {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientOsType clientOsType,
      Se.Set Beam.deviceId deviceId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reasonToDelete reasonToDelete,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

instance FromTType' Beam.DeletedPerson Domain.Types.DeletedPerson.DeletedPerson where
  fromTType' (Beam.DeletedPersonT {..}) = do
    pure $
      Just
        Domain.Types.DeletedPerson.DeletedPerson
          { clientOsType = clientOsType,
            createdAt = createdAt,
            deviceId = deviceId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            reasonToDelete = reasonToDelete,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DeletedPerson Domain.Types.DeletedPerson.DeletedPerson where
  toTType' (Domain.Types.DeletedPerson.DeletedPerson {..}) = do
    Beam.DeletedPersonT
      { Beam.clientOsType = clientOsType,
        Beam.createdAt = createdAt,
        Beam.deviceId = deviceId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.reasonToDelete = reasonToDelete,
        Beam.updatedAt = updatedAt
      }
