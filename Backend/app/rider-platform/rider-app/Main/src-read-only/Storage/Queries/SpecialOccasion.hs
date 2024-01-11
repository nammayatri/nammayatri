{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SpecialOccasion where

import qualified Data.Time.Calendar
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SpecialOccasion
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialOccasion as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.SpecialOccasion.SpecialOccasion -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.SpecialOccasion.SpecialOccasion] -> m ()
createMany = traverse_ createWithKV

findAllSpecialOccasionByEntityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> m ([Domain.Types.SpecialOccasion.SpecialOccasion])
findAllSpecialOccasionByEntityId entityId date = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.date $ Se.Eq date
        ]
    ]

findSpecialOccasionByEntityIdAndDate :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> m (Maybe (Domain.Types.SpecialOccasion.SpecialOccasion))
findSpecialOccasionByEntityIdAndDate entityId date = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.date $ Se.Eq date
        ]
    ]

findSpecialOccasionByEntityIdAndDayOfWeek :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.SpecialOccasion.SpecialOccasion))
findSpecialOccasionByEntityIdAndDayOfWeek entityId dayOfWeek = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.dayOfWeek $ Se.Eq dayOfWeek
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.SpecialOccasion.SpecialOccasion -> m (Maybe (Domain.Types.SpecialOccasion.SpecialOccasion))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.SpecialOccasion.SpecialOccasion -> m ()
updateByPrimaryKey Domain.Types.SpecialOccasion.SpecialOccasion {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.businessHours $ (Kernel.Types.Id.getId <$> businessHours),
      Se.Set Beam.date $ date,
      Se.Set Beam.dayOfWeek $ dayOfWeek,
      Se.Set Beam.description $ description,
      Se.Set Beam.entityId $ entityId,
      Se.Set Beam.specialDayType $ specialDayType,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.SpecialOccasion Domain.Types.SpecialOccasion.SpecialOccasion where
  fromTType' Beam.SpecialOccasionT {..} = do
    pure $
      Just
        Domain.Types.SpecialOccasion.SpecialOccasion
          { businessHours = Kernel.Types.Id.Id <$> businessHours,
            date = date,
            dayOfWeek = dayOfWeek,
            description = description,
            entityId = entityId,
            id = Kernel.Types.Id.Id id,
            specialDayType = specialDayType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SpecialOccasion Domain.Types.SpecialOccasion.SpecialOccasion where
  toTType' Domain.Types.SpecialOccasion.SpecialOccasion {..} = do
    Beam.SpecialOccasionT
      { Beam.businessHours = Kernel.Types.Id.getId <$> businessHours,
        Beam.date = date,
        Beam.dayOfWeek = dayOfWeek,
        Beam.description = description,
        Beam.entityId = entityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.specialDayType = specialDayType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
