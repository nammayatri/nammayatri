module API.Internal.FeedbackForm
  ( API,
    handler,
  )
where

import Domain.Action.Internal.FeedbackForm
import Domain.Types.Feedback.FeedbackForm
import qualified Domain.Types.Ride as R
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "beckn"
    :> "booking"
    :> ( "feedback"
           :> ReqBody '[JSON] FeedbackFormReq
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler = updateFeedbackFormResult

updateFeedbackFormResult :: FeedbackFormReq -> FlowHandler APISuccess
updateFeedbackFormResult req = withFlowHandlerAPI $ do
  feedbackSubmitApiRateLimitOptions <- asks (.apiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (feedbackSubmitHitsCountKey req.rideId) feedbackSubmitApiRateLimitOptions
  saveFeedbackFormResult req

feedbackSubmitHitsCountKey :: Id R.Ride -> Text
feedbackSubmitHitsCountKey rideId = "FeedbackSubmitHits:" <> getId rideId <> ":hitsCount"
