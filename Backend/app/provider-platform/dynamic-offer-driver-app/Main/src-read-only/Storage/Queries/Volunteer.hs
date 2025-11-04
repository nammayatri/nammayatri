{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Volunteer where

import qualified Data.Text
import qualified Domain.Types.Volunteer
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Volunteer as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Volunteer.Volunteer -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Volunteer.Volunteer] -> m ())
createMany = traverse_ create

findAllByPlace :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m [Domain.Types.Volunteer.Volunteer])
findAllByPlace place = do findAllWithKV [Se.Is Beam.place $ Se.Eq place]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Volunteer.Volunteer -> m (Maybe Domain.Types.Volunteer.Volunteer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Volunteer.Volunteer -> m (Maybe Domain.Types.Volunteer.Volunteer))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Volunteer.Volunteer -> m ())
updateByPrimaryKey (Domain.Types.Volunteer.Volunteer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.place place,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Volunteer Domain.Types.Volunteer.Volunteer where
  fromTType' (Beam.VolunteerT {..}) = do
    pure $
      Just
        Domain.Types.Volunteer.Volunteer
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            place = place,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Volunteer Domain.Types.Volunteer.Volunteer where
  toTType' (Domain.Types.Volunteer.Volunteer {..}) = do
    Beam.VolunteerT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.place = place,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
