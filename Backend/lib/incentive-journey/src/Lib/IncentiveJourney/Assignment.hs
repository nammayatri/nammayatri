{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Assignment
  ( JourneyAssignment (..),
    findAssignmentsByUserId,
  )
where

import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMapping as QUCM

-- | User assignment joined with the cohort↔journey window + streak-end reward config.
data JourneyAssignment = JourneyAssignment
  { userCohortMapping :: DUCM.UserCohortMapping,
    cohortJourneyMapping :: DCJM.CohortJourneyMapping
  }

findAssignmentsByUserId :: (BeamFlow m r) => Id Common.Person -> m [JourneyAssignment]
findAssignmentsByUserId userId = do
  ucms <- QUCM.findByUserId userId
  cjms <- QCJMExtra.findByIds (map (.cohortMappingId) ucms)
  pure $
    [ JourneyAssignment {userCohortMapping = ucm, cohortJourneyMapping = cjm}
      | ucm <- ucms,
        Just cjm <- [find (\c -> c.id == ucm.cohortMappingId) cjms]
    ]
