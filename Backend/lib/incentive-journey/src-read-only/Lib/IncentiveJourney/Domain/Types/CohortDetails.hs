{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.CohortDetails where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CohortDetails = CohortDetails
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails,
    name :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
