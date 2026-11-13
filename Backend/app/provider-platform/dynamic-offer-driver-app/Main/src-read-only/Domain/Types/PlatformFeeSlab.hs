{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PlatformFeeSlab where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data PlatformFeeSlab = PlatformFeeSlab
  { cgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    maxDistanceMeters :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    minDistanceMeters :: Kernel.Prelude.Int,
    platformFee :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    slabSetId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
