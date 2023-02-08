module Core.ACL.Rating (buildRatingReq) where

import qualified Beckn.Types.Core.Taxi.Rating as Rating
import qualified Domain.Action.UI.Feedback as DFeedback
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common

buildRatingReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DFeedback.FeedbackRes ->
  m (BecknReq Rating.RatingMessage)
buildRatingReq DFeedback.FeedbackRes {..} = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  msgId <- generateGUID
  context <- buildTaxiContext Context.RATING msgId Nothing bapIDs.cabs bapURIs.cabs (Just providerId) (Just providerUrl)
  let message =
        Rating.RatingMessage
          { id = bppBookingId.getId,
            value = ratingValue,
            feedback_form =
              Rating.FeedbackForm
                { question = "Evaluate your ride experience.",
                  answer = feedbackDetails
                }
          }
  pure $ BecknReq context message
