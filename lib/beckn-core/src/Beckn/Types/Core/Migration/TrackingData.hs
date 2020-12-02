module Beckn.Types.Core.Migration.TrackingData where

import Beckn.Types.Core.Migration.Gps
import EulerHS.Prelude

newtype TrackingData = TrackingData Gps
  deriving (Generic, Show, FromJSON, ToJSON)
