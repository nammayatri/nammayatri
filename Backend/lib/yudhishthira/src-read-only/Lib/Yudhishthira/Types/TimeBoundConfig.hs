{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.TimeBoundConfig where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data TimeBoundConfig = TimeBoundConfig
  { merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    timeBoundDomain :: Lib.Yudhishthira.Types.LogicDomain,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
