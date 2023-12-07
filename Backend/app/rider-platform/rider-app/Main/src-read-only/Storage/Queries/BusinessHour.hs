{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BusinessHour where

import qualified Domain.Types.BusinessHour
import qualified Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessHour as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.BusinessHour.BusinessHour -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.BusinessHour.BusinessHour] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m (Maybe (Domain.Types.BusinessHour.BusinessHour))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m (Maybe (Domain.Types.BusinessHour.BusinessHour))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.BusinessHour.BusinessHourType -> [Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory] -> Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m ()
updateByPrimaryKey btype categoryId (Kernel.Types.Id.Id id) = do
  updateWithKV
    [ Se.Set Beam.btype btype,
      Se.Set Beam.categoryId (Kernel.Types.Id.getId <$> categoryId)
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

instance FromTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  fromTType' Beam.BusinessHourT {..} = do
    pure $
      Just
        Domain.Types.BusinessHour.BusinessHour
          { btype = btype,
            categoryId = Kernel.Types.Id.Id <$> categoryId,
            id = Kernel.Types.Id.Id id
          }

instance ToTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  toTType' Domain.Types.BusinessHour.BusinessHour {..} = do
    Beam.BusinessHourT
      { Beam.btype = btype,
        Beam.categoryId = Kernel.Types.Id.getId <$> categoryId,
        Beam.id = Kernel.Types.Id.getId id
      }
