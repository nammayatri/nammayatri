{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyMilestoneExtra where

import Data.Aeson (Value)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (getCurrentTime)
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

-- | Live NetOff / slab edits: write rewardMetadata only (condition/reward type stay fixed).
-- Callers must pass Just when replacing; omit at the action layer keeps the stored value.
updateRewardMetadata ::
  (BeamFlow m r) =>
  Id DIJM.IncentiveJourneyMilestone ->
  Maybe Value ->
  m ()
updateRewardMetadata milestoneId rewardMetadata = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.rewardMetadata rewardMetadata,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId milestoneId)]
