{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FullFarePolicyProgressiveDetailsPerMinRateSection = FullFarePolicyProgressiveDetailsPerMinRateSection
  { currency :: Kernel.Types.Common.Currency,
    farePolicyId :: Kernel.Prelude.Text,
    perMinRate :: Kernel.Types.Common.HighPrecMoney,
    rideDuration :: Kernel.Types.Common.Minutes,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
