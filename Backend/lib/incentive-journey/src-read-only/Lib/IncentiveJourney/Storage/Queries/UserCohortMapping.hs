{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.UserCohortMapping (module Lib.IncentiveJourney.Storage.Queries.UserCohortMapping, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow
import qualified Lib.IncentiveJourney.Storage.Beam.UserCohortMapping as Beam
import Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping -> m ())
create = createWithKV

createMany :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping] -> m ())
createMany = traverse_ create

findById ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping -> m (Maybe Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByUserId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Person -> m ([Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping]))
findByUserId userId = do findAllWithKV [Se.Is Beam.userId $ Se.Eq (Kernel.Types.Id.getId userId)]

findByUserIdAndCohortMappingId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Person -> Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping -> m (Maybe Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping))
findByUserIdAndCohortMappingId userId cohortMappingId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.userId $ Se.Eq (Kernel.Types.Id.getId userId),
          Se.Is Beam.cohortMappingId $ Se.Eq (Kernel.Types.Id.getId cohortMappingId)
        ]
    ]

findByPrimaryKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping -> m (Maybe Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping -> m ())
updateByPrimaryKey (Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cohortMappingId (Kernel.Types.Id.getId cohortMappingId),
      Se.Set Beam.isTestGroup isTestGroup,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.userId (Kernel.Types.Id.getId userId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
