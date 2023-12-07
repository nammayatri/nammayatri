{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ServiceCategory where

import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.ServiceCategory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ServiceCategory.ServiceCategory -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.ServiceCategory.ServiceCategory] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m (Maybe (Domain.Types.ServiceCategory.ServiceCategory))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m (Maybe (Domain.Types.ServiceCategory.ServiceCategory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> [Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory] -> Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m ()
updateByPrimaryKey allowedSeats availableSeats description name peopleCategory (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.allowedSeats allowedSeats,
      Se.Set Beam.availableSeats availableSeats,
      Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.peopleCategory (Kernel.Types.Id.getId <$> peopleCategory)
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

instance FromTType' Beam.ServiceCategory Domain.Types.ServiceCategory.ServiceCategory where
  fromTType' Beam.ServiceCategoryT {..} = do
    pure $
      Just
        Domain.Types.ServiceCategory.ServiceCategory
          { allowedSeats = allowedSeats,
            availableSeats = availableSeats,
            description = description,
            id = Kernel.Types.Id.Id id,
            name = name,
            peopleCategory = Kernel.Types.Id.Id <$> peopleCategory
          }

instance ToTType' Beam.ServiceCategory Domain.Types.ServiceCategory.ServiceCategory where
  toTType' Domain.Types.ServiceCategory.ServiceCategory {..} = do
    Beam.ServiceCategoryT
      { Beam.allowedSeats = allowedSeats,
        Beam.availableSeats = availableSeats,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.peopleCategory = Kernel.Types.Id.getId <$> peopleCategory
      }
