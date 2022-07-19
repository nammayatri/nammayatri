module Beckn.Types.Core.Taxi.Rating
  ( module Beckn.Types.Core.Taxi.Rating,
  )
where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data RatingMessage = RatingMessage
  { id :: Text,
    value :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
