{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.CohortJourneyMapping (module Lib.IncentiveJourney.Storage.Queries.CohortJourneyMapping, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow
import qualified Lib.IncentiveJourney.Storage.Beam.CohortJourneyMapping as Beam
import Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping -> m ())
create = createWithKV

createMany :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping] -> m ())
createMany = traverse_ create

findByCohortId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails -> m [Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping])
findByCohortId cohortId = do findAllWithKV [Se.Is Beam.cohortId $ Se.Eq (Kernel.Types.Id.getId cohortId)]

findByCohortIdAndJourneyId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m (Maybe Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping))
findByCohortIdAndJourneyId cohortId journeyId = do findOneWithKV [Se.And [Se.Is Beam.cohortId $ Se.Eq (Kernel.Types.Id.getId cohortId), Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)]]

findById ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping -> m (Maybe Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByJourneyId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m [Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping])
findByJourneyId journeyId = do findAllWithKV [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)]

findByPrimaryKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping -> m (Maybe Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping -> m ())
updateByPrimaryKey (Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cohortId (Kernel.Types.Id.getId cohortId),
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.startDate startDate,
      Se.Set Beam.streakEndRewardExpirationAt streakEndRewardExpirationAt,
      Se.Set Beam.streakEndRewardType streakEndRewardType,
      Se.Set Beam.streakEndRewardValue streakEndRewardValue,
      Se.Set Beam.streakRange streakRange,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
