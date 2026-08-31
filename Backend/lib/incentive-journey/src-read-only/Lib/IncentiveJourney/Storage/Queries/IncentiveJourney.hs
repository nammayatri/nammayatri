{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourney (module Lib.IncentiveJourney.Storage.Queries.IncentiveJourney, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourney as Beam
import Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m ())
create = createWithKV

createMany :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney] -> m ())
createMany = traverse_ create

findAll :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Maybe Int -> Maybe Int -> m [Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney])
findAll limit offset = do findAllWithOptionsKV [] (Se.Desc Beam.createdAt) limit offset

findById ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m (Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m (Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.IncentiveJourney.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney -> m ())
updateByPrimaryKey (Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourney {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.description description, Se.Set Beam.journeyType journeyType, Se.Set Beam.name name, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
