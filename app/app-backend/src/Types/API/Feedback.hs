module Types.API.Feedback
  ( FeedbackReq (..),
    FeedbackRes,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import EulerHS.Prelude

data FeedbackReq = FeedbackReq
  { caseId :: Text,
    productInstanceId :: Text,
    rating :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type FeedbackRes = APISuccess
