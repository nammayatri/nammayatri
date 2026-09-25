{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Assignment
  ( JourneyAssignment (..),
  )
where

import Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM

-- | User assignment joined with the cohort↔journey window + streak-end reward config.
data JourneyAssignment = JourneyAssignment
  { userCohortMapping :: DUCM.UserCohortMapping,
    cohortJourneyMapping :: DCJM.CohortJourneyMapping
  }
  deriving (Generic, Show, ToJSON, FromJSON)
