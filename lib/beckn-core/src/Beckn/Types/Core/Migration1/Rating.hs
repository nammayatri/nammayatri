module Beckn.Types.Core.Migration1.Rating
  ( module Beckn.Types.Core.Migration1.Rating,
  )
where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data RatingMessage = RatingMessage
  { id :: Text,
    value :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
