{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.IncentiveJourney (module Reexport) where

import qualified Data.Text as T
import Lib.IncentiveJourney.Storage.Beam.CohortDetails as Reexport
import Lib.IncentiveJourney.Storage.Beam.CohortJourneyMapping as Reexport
import Lib.IncentiveJourney.Storage.Beam.IncentiveJourney as Reexport
import Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyMilestone as Reexport
import Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyStats as Reexport
import Lib.IncentiveJourney.Storage.Beam.UserCohortMapping as Reexport
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName IncentiveJourneyT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName IncentiveJourneyMilestoneT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName IncentiveJourneyStatsT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName CohortDetailsT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName CohortJourneyMappingT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName UserCohortMappingT where
  schemaName _ = T.pack currentSchemaName
