{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultiModalCalendar where

import qualified Domain.Types.MultiModalCalendar
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MultiModalCalendar as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalCalendar.MultiModalCalendar -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MultiModalCalendar.MultiModalCalendar] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MultiModalCalendar.MultiModalCalendar -> m (Maybe Domain.Types.MultiModalCalendar.MultiModalCalendar))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MultiModalCalendar.MultiModalCalendar -> m (Maybe Domain.Types.MultiModalCalendar.MultiModalCalendar))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalCalendar.MultiModalCalendar -> m ())
updateByPrimaryKey (Domain.Types.MultiModalCalendar.MultiModalCalendar {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.endDate endDate,
      Se.Set Beam.friday friday,
      Se.Set Beam.monday monday,
      Se.Set Beam.saturday saturday,
      Se.Set Beam.startDate startDate,
      Se.Set Beam.sunday sunday,
      Se.Set Beam.thursday thursday,
      Se.Set Beam.tuesday tuesday,
      Se.Set Beam.wednesday wednesday,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MultiModalCalendar Domain.Types.MultiModalCalendar.MultiModalCalendar where
  fromTType' (Beam.MultiModalCalendarT {..}) = do
    pure $
      Just
        Domain.Types.MultiModalCalendar.MultiModalCalendar
          { endDate = endDate,
            friday = friday,
            id = Kernel.Types.Id.Id id,
            monday = monday,
            saturday = saturday,
            startDate = startDate,
            sunday = sunday,
            thursday = thursday,
            tuesday = tuesday,
            wednesday = wednesday,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MultiModalCalendar Domain.Types.MultiModalCalendar.MultiModalCalendar where
  toTType' (Domain.Types.MultiModalCalendar.MultiModalCalendar {..}) = do
    Beam.MultiModalCalendarT
      { Beam.endDate = endDate,
        Beam.friday = friday,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.monday = monday,
        Beam.saturday = saturday,
        Beam.startDate = startDate,
        Beam.sunday = sunday,
        Beam.thursday = thursday,
        Beam.tuesday = tuesday,
        Beam.wednesday = wednesday,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
