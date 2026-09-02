{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IntercityPlatformFeeSlab where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data IntercityPlatformFeeSlab = IntercityPlatformFeeSlab
  { cgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    maxDistanceMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    minDistanceMeters :: Kernel.Prelude.Int,
    platformFee :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
