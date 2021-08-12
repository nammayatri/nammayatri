module Beckn.Types.Core.API.Feedback where

import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Core
import qualified Beckn.Types.Core.Description as Core
import qualified Beckn.Types.Core.Rating as Core
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, type (:>))

type FeedbackAPI =
  "feedback"
    :> ReqBody '[JSON] FeedbackReq
    :> Post '[JSON] FeedbackRes

feedback :: Proxy FeedbackAPI
feedback = Proxy

data FeedbackReq = FeedbackReq
  { context :: Core.Context,
    message :: FeedbackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type FeedbackRes = AckResponse

data FeedbackReqMessage = FeedbackReqMessage
  { order_id :: Text,
    rating :: Core.Rating,
    description :: Core.Description
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
