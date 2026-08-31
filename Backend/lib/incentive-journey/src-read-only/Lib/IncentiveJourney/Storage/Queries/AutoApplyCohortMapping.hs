{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMapping (module Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMapping, module ReExport) where

import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping
import qualified Lib.IncentiveJourney.Storage.Beam.AutoApplyCohortMapping as Beam
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow
import Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMappingExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping -> m ())
create = createWithKV

createMany :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping] -> m ())
createMany = traverse_ create

findByCohortJourneyMappingId ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping -> m ([Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping]))
findByCohortJourneyMappingId cohortJourneyMappingId = do findAllWithKV [Se.Is Beam.cohortJourneyMappingId $ Se.Eq (Kernel.Types.Id.getId cohortJourneyMappingId)]

findById ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping -> m (Maybe Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping -> m (Maybe Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping -> m ())
updateByPrimaryKey (Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping.AutoApplyCohortMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowIfNoMapping allowIfNoMapping,
      Se.Set Beam.cohortJourneyMappingId (Kernel.Types.Id.getId cohortJourneyMappingId),
      Se.Set Beam.enabled enabled,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory (((Kernel.Prelude.fmap (Data.Text.pack . Kernel.Prelude.show))) vehicleCategory)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
