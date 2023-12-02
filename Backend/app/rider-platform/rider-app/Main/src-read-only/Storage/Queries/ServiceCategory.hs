{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ServiceCategory where

import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory as Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.ServiceCategory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ServiceCategory.ServiceCategory -> m ()
create = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory -> m (Maybe (Domain.Types.ServiceCategory.ServiceCategory))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
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
