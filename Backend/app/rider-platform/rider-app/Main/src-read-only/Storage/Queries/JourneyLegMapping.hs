{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLegMapping where

import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.JourneyLegMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyLegMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLegMapping.JourneyLegMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyLegMapping.JourneyLegMapping] -> m ())
createMany = traverse_ create

findAllLegsMappingByJourneyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Kernel.Prelude.Bool -> m [Domain.Types.JourneyLegMapping.JourneyLegMapping])
findAllLegsMappingByJourneyId limit offset journeyId isDeleted = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId),
          Se.Is Beam.isDeleted $ Se.Eq isDeleted
        ]
    ]
    (Se.Asc Beam.sequenceNumber)
    limit
    offset

findByJourneyIdAndSequenceNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Kernel.Prelude.Int -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.JourneyLegMapping.JourneyLegMapping))
findByJourneyIdAndSequenceNumber journeyId sequenceNumber isDeleted = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId),
          Se.Is Beam.sequenceNumber $ Se.Eq sequenceNumber,
          Se.Is Beam.isDeleted $ Se.Eq isDeleted
        ]
    ]

findByJourneyLegId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m (Maybe Domain.Types.JourneyLegMapping.JourneyLegMapping))
findByJourneyLegId journeyLegId = do findOneWithKV [Se.Is Beam.journeyLegId $ Se.Eq (Kernel.Types.Id.getId journeyLegId)]

updateIsDeleted :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateIsDeleted isDeleted journeyLegId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isDeleted isDeleted, Se.Set Beam.updatedAt _now] [Se.Is Beam.journeyLegId $ Se.Eq (Kernel.Types.Id.getId journeyLegId)]

updateJourneyLegId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> Kernel.Types.Id.Id Domain.Types.JourneyLegMapping.JourneyLegMapping -> m ())
updateJourneyLegId journeyLegId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.journeyLegId (Kernel.Types.Id.getId journeyLegId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLegMapping.JourneyLegMapping -> m (Maybe Domain.Types.JourneyLegMapping.JourneyLegMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLegMapping.JourneyLegMapping -> m ())
updateByPrimaryKey (Domain.Types.JourneyLegMapping.JourneyLegMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isDeleted isDeleted,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.journeyLegId (Kernel.Types.Id.getId journeyLegId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.sequenceNumber sequenceNumber,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.JourneyLegMapping Domain.Types.JourneyLegMapping.JourneyLegMapping where
  fromTType' (Beam.JourneyLegMappingT {..}) = do
    pure $
      Just
        Domain.Types.JourneyLegMapping.JourneyLegMapping
          { id = Kernel.Types.Id.Id id,
            isDeleted = isDeleted,
            journeyId = Kernel.Types.Id.Id journeyId,
            journeyLegId = Kernel.Types.Id.Id journeyLegId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            sequenceNumber = sequenceNumber,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyLegMapping Domain.Types.JourneyLegMapping.JourneyLegMapping where
  toTType' (Domain.Types.JourneyLegMapping.JourneyLegMapping {..}) = do
    Beam.JourneyLegMappingT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeleted = isDeleted,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.journeyLegId = Kernel.Types.Id.getId journeyLegId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.sequenceNumber = sequenceNumber,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
