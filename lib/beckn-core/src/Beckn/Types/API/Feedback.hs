module Beckn.Types.API.Feedback
  ( FeedbackAPI,
    FeedbackReq (..),
    FeedbackRes,
    FeedbackReqMessage (..),
  )
where

import qualified Beckn.Types.Common as Common
import qualified Beckn.Types.Core.Context as Core
import qualified Beckn.Types.Core.Description as Core
import qualified Beckn.Types.Core.Rating as Core
import Beckn.Utils.Servant.HeaderAuth (APIKeyAuth)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, type (:>))

type FeedbackAPI v =
  "feedback"
    :> APIKeyAuth v
    :> ReqBody '[JSON] FeedbackReq
    :> Post '[JSON] FeedbackRes

data FeedbackReq = FeedbackReq
  { context :: Core.Context,
    message :: FeedbackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type FeedbackRes = Common.AckResponse

data FeedbackReqMessage = FeedbackReqMessage
  { order_id :: Text,
    rating :: Core.Rating,
    description :: Core.Description
  }
  deriving (Generic, Show, FromJSON, ToJSON)
