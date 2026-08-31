{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestoneExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyMilestone as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.IncentiveJourneyMilestone ()
import qualified Sequelize as Se

findByIds :: (BeamFlow m r) => [Id DIJM.IncentiveJourneyMilestone] -> m [DIJM.IncentiveJourneyMilestone]
findByIds ids
  | null ids = pure []
  | otherwise =
    findAllWithKV
      [Se.Is Beam.id $ Se.In (map getId ids)]
