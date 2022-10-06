module Core.ACL.Rating (buildRatingReq) where

import Beckn.Prelude
import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Rating as Rating
import qualified Domain.Action.UI.Feedback as DFeedback
import Environment
import Utils.Common

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
