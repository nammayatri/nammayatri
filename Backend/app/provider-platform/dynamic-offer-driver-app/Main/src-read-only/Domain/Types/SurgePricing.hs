{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SurgePricing where

import Data.Aeson
import qualified Domain.Types.Common
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SurgePricing = SurgePricing
  { dayOfWeek :: Kernel.Prelude.Text,
    hourOfDay :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.SurgePricing.SurgePricing,
    sourceHex :: Kernel.Prelude.Text,
    surgeMultiplier :: Kernel.Types.Common.Centesimal,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
