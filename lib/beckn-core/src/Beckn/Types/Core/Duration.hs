module Beckn.Types.Core.Duration where

import Beckn.Utils.Common
import EulerHS.Prelude

newtype Duration = Duration Text
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Duration where
  example =
    Duration "P1D"
