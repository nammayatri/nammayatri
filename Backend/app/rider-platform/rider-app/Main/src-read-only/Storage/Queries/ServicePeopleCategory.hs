{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ServicePeopleCategory where

import qualified Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.ServicePeopleCategory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.ServicePeopleCategory.ServicePeopleCategory] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m (Maybe (Domain.Types.ServicePeopleCategory.ServicePeopleCategory))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m (Maybe (Domain.Types.ServicePeopleCategory.ServicePeopleCategory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m ()
updateByPrimaryKey description name pricePerUnit (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.pricePerUnit pricePerUnit
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

instance FromTType' Beam.ServicePeopleCategory Domain.Types.ServicePeopleCategory.ServicePeopleCategory where
  fromTType' Beam.ServicePeopleCategoryT {..} = do
    pure $
      Just
        Domain.Types.ServicePeopleCategory.ServicePeopleCategory
          { description = description,
            id = Kernel.Types.Id.Id id,
            name = name,
            pricePerUnit = pricePerUnit
          }

instance ToTType' Beam.ServicePeopleCategory Domain.Types.ServicePeopleCategory.ServicePeopleCategory where
  toTType' Domain.Types.ServicePeopleCategory.ServicePeopleCategory {..} = do
    Beam.ServicePeopleCategoryT
      { Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.pricePerUnit = pricePerUnit
      }
