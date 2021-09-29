module Types.API.Feedback
  ( FeedbackReq (..),
    FeedbackRes,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Types.Storage.Ride as SRide

data FeedbackReq = FeedbackReq
  { rideId :: Id SRide.Ride,
    rating :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type FeedbackRes = APISuccess
