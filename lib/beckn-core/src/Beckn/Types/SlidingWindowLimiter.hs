module Beckn.Types.SlidingWindowLimiter where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude

data APIRateLimitOptions = APIRateLimitOptions
  { limit :: Int,
    limitResetTimeInSec :: Int
  }
  deriving (Generic, FromDhall)
