module Types.API.Feedback
  ( FeedbackReq (..),
    FeedbackRes,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude

data FeedbackReq = FeedbackReq
  { rideId :: Id SRide.Ride,
    rating :: Int,
    feedbackDetails :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type FeedbackRes = APISuccess
