module Core.Search where

import Beckn.Prelude
import Core.Location
import Data.Aeson

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Fulfillment = Fulfillment
  { start :: StartInfo,
    end :: EndInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data StartInfo = StartInfo
  { location :: LocationGps,
    time :: StartTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype StartTime = StartTime {range :: TimeRange}
  deriving (Generic, Show, ToJSON, FromJSON)

data TimeRange = TimeRange
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype EndInfo = EndInfo
  { location :: LocationGps
  }
  deriving (Generic, Show, ToJSON, FromJSON)
