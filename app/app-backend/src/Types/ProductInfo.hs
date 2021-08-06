module Types.ProductInfo where

import EulerHS.Prelude
import Types.Common

data ProductInfo = ProductInfo
  { provider :: Maybe Provider,
    tracker :: Maybe Tracker
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Tracker = Tracker
  { trip :: Trip,
    tracking :: Maybe Tracking
  }
  deriving (Generic, FromJSON, ToJSON, Show)
