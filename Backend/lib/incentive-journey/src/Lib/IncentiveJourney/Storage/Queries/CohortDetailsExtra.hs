{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.CohortDetailsExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.CohortDetails as DCD
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.CohortDetails as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.CohortDetails ()
import qualified Sequelize as Se

-- | Create a cohort row. Name should be unique for ops clarity (enforced softly via findByName at call sites).
createCohortDetails :: (BeamFlow m r) => Text -> m DCD.CohortDetails
createCohortDetails name = do
  now <- getCurrentTime
  cohortId <- generateGUID
  let row =
        DCD.CohortDetails
          { id = cohortId,
            name = name,
            createdAt = now,
            updatedAt = now
          }
  createWithKV row
  pure row

findCohortDetailsById :: (BeamFlow m r) => Id DCD.CohortDetails -> m (Maybe DCD.CohortDetails)
findCohortDetailsById cohortId =
  findOneWithKV [Se.Is Beam.id $ Se.Eq (getId cohortId)]

findByIds :: (BeamFlow m r) => [Id DCD.CohortDetails] -> m [DCD.CohortDetails]
findByIds ids
  | null ids = pure []
  | otherwise =
    findAllWithKV
      [Se.Is Beam.id $ Se.In (map getId ids)]
