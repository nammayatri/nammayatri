{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Storage.Beam.BeamFlow where

import Kernel.Beam.Lib.UtilsTH as Reexport
import Kernel.Types.Common as Reexport hiding (id)
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Storage.Beam.CohortDetails as BeamCD
import qualified Lib.IncentiveJourney.Storage.Beam.CohortJourneyMapping as BeamCJM
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourney as BeamIJ
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyMilestone as BeamIJM
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourneyStats as BeamIJS
import qualified Lib.IncentiveJourney.Storage.Beam.UserCohortMapping as BeamUCM

type BeamFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasIncentiveJourneyTablesSchema
  )

type HasIncentiveJourneyTablesSchema =
  ( HasSchemaName BeamIJ.IncentiveJourneyT,
    HasSchemaName BeamIJM.IncentiveJourneyMilestoneT,
    HasSchemaName BeamIJS.IncentiveJourneyStatsT,
    HasSchemaName BeamCD.CohortDetailsT,
    HasSchemaName BeamCJM.CohortJourneyMappingT,
    HasSchemaName BeamUCM.UserCohortMappingT
  )
