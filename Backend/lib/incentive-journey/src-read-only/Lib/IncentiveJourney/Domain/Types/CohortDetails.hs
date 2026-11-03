{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Domain.Types.CohortDetails where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data CohortDetails = CohortDetails
  { category :: Kernel.Prelude.Text,
    cohortRule :: Kernel.Prelude.Maybe Data.Aeson.Value,
    cohortType :: Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortType,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.IncentiveJourney.Domain.Types.CohortDetails.CohortDetails,
    name :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CohortType = Assigned | OptIn deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''CohortType)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''CohortType)
