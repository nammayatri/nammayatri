module Types.API.Feedback
  ( FeedbackReq (..),
    FeedbackRes,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import EulerHS.Prelude

data FeedbackReq = FeedbackReq
  { productInstanceId :: Text,
    rating :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type FeedbackRes = APISuccess
