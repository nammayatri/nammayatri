module Core.Search.Fulfillment where

import Beckn.Prelude
import Core.Search.LocationGps
import Beckn.Utils.GenericPretty

data Fulfillment = Fulfillment
  { start :: StartInfo,
    end :: EndInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

data StartInfo = StartInfo
  { location :: LocationGps,
    time :: StartTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

newtype StartTime = StartTime {range :: TimeRange}
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

data TimeRange = TimeRange
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

newtype EndInfo = EndInfo
  { location :: LocationGps
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)
