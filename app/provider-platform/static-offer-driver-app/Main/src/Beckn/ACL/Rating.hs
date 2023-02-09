module Beckn.ACL.Rating where

import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import qualified Domain.Action.Beckn.Rating as DRating
import EulerHS.Prelude hiding (state)
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildRatingReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
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
