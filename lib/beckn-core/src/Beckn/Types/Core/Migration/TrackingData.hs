module Beckn.Types.Core.Migration.TrackingData where

import Beckn.Types.Core.Migration.GPS
import EulerHS.Prelude

newtype TrackingData = TrackingData GPS
  deriving (Generic, Show, FromJSON, ToJSON)
