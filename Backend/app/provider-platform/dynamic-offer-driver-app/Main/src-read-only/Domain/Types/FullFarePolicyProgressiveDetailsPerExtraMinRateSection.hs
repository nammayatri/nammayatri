{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection where

import qualified Domain.Types.FarePolicy
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FullFarePolicyProgressiveDetailsPerExtraMinRateSection = FullFarePolicyProgressiveDetailsPerExtraMinRateSection
  { farePolicyId :: Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy,
    perExtraMinRate :: Kernel.Types.Common.HighPrecMoney,
    startMin :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
