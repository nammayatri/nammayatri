module Beckn.Types.SlidingWindowLimiter where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude

data RegistrationHitsOptions = RegistrationHitsOptions
  { limit :: Int,
    limitResetTime :: Int
  }
  deriving (Generic, FromDhall)
