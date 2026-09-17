{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.AvailableForRides where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data AvailableForRidesRes = AvailableForRidesRes
  { activationsAllowedPerDay :: Kernel.Prelude.Int,
    activationsUsedToday :: Kernel.Prelude.Int,
    requestsAllowed :: Kernel.Prelude.Int,
    validTill :: Kernel.Prelude.UTCTime,
    validityMinutes :: Kernel.Types.Common.Minutes
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
