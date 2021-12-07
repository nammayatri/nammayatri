module Beckn.Types.Core.Cabs.Rating
  ( module Beckn.Types.Core.Cabs.Rating,
  )
where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data RatingMessage = RatingMessage
  { id :: Text,
    value :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
