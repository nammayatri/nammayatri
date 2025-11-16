{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonDisability where

import qualified Domain.Types.Person
import qualified Domain.Types.PersonDisability
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonDisability as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonDisability.PersonDisability -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PersonDisability.PersonDisability] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonDisability.PersonDisability))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateDisabilityByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDisabilityByPersonId disabilityId tag description personId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.disabilityId disabilityId,
      Se.Set Beam.tag tag,
      Se.Set Beam.description description,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonDisability.PersonDisability))
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonDisability.PersonDisability -> m ())
updateByPrimaryKey (Domain.Types.PersonDisability.PersonDisability {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.disabilityId disabilityId,
      Se.Set Beam.tag tag,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

instance FromTType' Beam.PersonDisability Domain.Types.PersonDisability.PersonDisability where
  fromTType' (Beam.PersonDisabilityT {..}) = do
    pure $
      Just
        Domain.Types.PersonDisability.PersonDisability
          { createdAt = Kernel.Prelude.fromMaybe updatedAt createdAt,
            description = description,
            disabilityId = disabilityId,
            personId = Kernel.Types.Id.Id personId,
            tag = tag,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PersonDisability Domain.Types.PersonDisability.PersonDisability where
  toTType' (Domain.Types.PersonDisability.PersonDisability {..}) = do
    Beam.PersonDisabilityT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.description = description,
        Beam.disabilityId = disabilityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.tag = tag,
        Beam.updatedAt = updatedAt
      }
