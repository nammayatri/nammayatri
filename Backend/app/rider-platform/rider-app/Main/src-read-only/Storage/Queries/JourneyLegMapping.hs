{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLegMapping (module Storage.Queries.JourneyLegMapping, module ReExport) where

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
import Storage.Queries.JourneyLegMappingExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLegMapping.JourneyLegMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyLegMapping.JourneyLegMapping] -> m ())
createMany = traverse_ create

findAllLegsMappingByJourneyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Kernel.Prelude.Bool -> m ([Domain.Types.JourneyLegMapping.JourneyLegMapping]))
findAllLegsMappingByJourneyId journeyId isDeleted = do findAllWithKV [Se.And [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId), Se.Is Beam.isDeleted $ Se.Eq isDeleted]]

findByJourneyIdAndSequenceNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> Kernel.Prelude.Int -> m (Maybe Domain.Types.JourneyLegMapping.JourneyLegMapping))
findByJourneyIdAndSequenceNumber journeyId sequenceNumber = do findOneWithKV [Se.And [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId), Se.Is Beam.sequenceNumber $ Se.Eq sequenceNumber]]

findByJourneyLegId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m (Maybe Domain.Types.JourneyLegMapping.JourneyLegMapping))
findByJourneyLegId journeyLegId = do findOneWithKV [Se.Is Beam.journeyLegId $ Se.Eq (Kernel.Types.Id.getId journeyLegId)]

updateIsDeleted :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateIsDeleted isDeleted journeyLegId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isDeleted isDeleted, Se.Set Beam.updatedAt _now] [Se.Is Beam.journeyLegId $ Se.Eq (Kernel.Types.Id.getId journeyLegId)]

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
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
