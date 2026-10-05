-- Reads the v2.1.0 /rating shape (message.order_id, ratings[].ref_type/ref_id, message.feedbacks[]) and patches it over Layer 1's parse,
-- which only knows our own BAP's convention (ratings[0].id = bookingId, feedback inside ratings[].feedback_form).
module Beckn.OnDemand.Transformer.OndcScheduledRide.Rating
  ( ondcScheduledRideParser,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Rating as DRating
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

-- | Sets bookingId, ratingValue and feedbackDetails on Layer 1's DRatingReq from the v2.1.0 fields, wherever the BAP sent them;
-- anything it did not send keeps Layer 1's value, so our own BAP's /rating keeps working unchanged.
ondcScheduledRideParser :: Spec.RatingReqMessage -> DRating.DRatingReq -> DRating.DRatingReq
ondcScheduledRideParser message dRatingReq =
  dRatingReq
    { DRating.bookingId = maybe dRatingReq.bookingId Id message.ratingReqMessageOrderId,
      DRating.ratingValue = fromMaybe dRatingReq.ratingValue driverRatingValue,
      DRating.feedbackDetails = maybe dRatingReq.feedbackDetails (\comment -> [Just comment, Nothing, Nothing]) driverFeedbackComment
    }
  where
    ratings = fromMaybe [] message.ratingReqMessageRatings
    feedbacks = fromMaybe [] message.ratingReqMessageFeedbacks
    -- The driver's rating is the one about the AGENT, else the FULFILLMENT (the ride). ITEM/ORDER/PROVIDER ratings are not the driver's.
    driverRating = find (refTypeIs Enums.REF_AGENT . (.ratingRefType)) ratings <|> find (refTypeIs Enums.REF_FULFILLMENT . (.ratingRefType)) ratings
    driverRatingValue = driverRating >>= (.ratingValue) >>= readMaybe . T.unpack
    -- feedbackDetails slot 0 is the free-text review (see Domain.Action.Beckn.Rating.handler); slots 1 and 2 (assistance offered, issue id) are our BAP's extensions.
    driverFeedbackComment = do
      rating <- driverRating
      feedback <- find (\f -> f.feedbackRefType == rating.ratingRefType && f.feedbackRefId == rating.ratingRefId) feedbacks
      feedback.feedbackComment

    refTypeIs :: Enums.RatingRefType -> Maybe Text -> Bool
    refTypeIs expected = (== Just expected) . (>>= readRefType)
    readRefType = readMaybe . T.unpack . ("REF_" <>)
