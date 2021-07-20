module Types.API.Feedback
  ( FeedbackReq (..),
    FeedbackRes,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data FeedbackReq = FeedbackReq
  { rideId :: Text,
    rating :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type FeedbackRes = APISuccess
