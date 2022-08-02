module Beckn.Types.Core.Taxi.Rating
  ( module Beckn.Types.Core.Taxi.Rating,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Rating.FeedbackForm as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data RatingMessage = RatingMessage
  { id :: Text,
    value :: Int,
    feedback_form :: FeedbackForm
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
