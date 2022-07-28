module Core.ACL.Rating where

import Beckn.Product.Validation.Context
import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import Beckn.Types.Id
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Beckn.Utils.Common
import qualified Domain.Action.Beckn.Rating as DRating
import EulerHS.Prelude hiding (state)
import Tools.Error

buildRatingReq ::
  (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Rating.RatingReq ->
  m DRating.DRatingReq
buildRatingReq subscriber req = do
  let context = req.context
  validateContext Context.RATING context
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  pure
    DRating.DRatingReq
      { bookingId = Id req.message.id,
        ratingValue = req.message.value,
        feedbackDetails = req.message.feedback_form.answer
      }
