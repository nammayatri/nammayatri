{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.UserCohortMapping where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping
import qualified Lib.IncentiveJourney.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data UserCohortMapping = UserCohortMapping
  { cohortMappingId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping.CohortJourneyMapping,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.UserCohortMapping.UserCohortMapping,
    isTestGroup :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime,
    userId :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.Common.Person
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
