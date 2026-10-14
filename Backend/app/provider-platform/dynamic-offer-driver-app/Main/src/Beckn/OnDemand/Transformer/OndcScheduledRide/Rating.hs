-- Standalone /rating parser for ONDC scheduled-ride pilot merchants. Unlike Layer 1 (Beckn.ACL.Rating.buildRatingReqV2) it does not
-- require ratings[0].id or an Int ratings[0].value: it reads the v2.1.0 shape (message.order_id, ratings[].ref_type/ref_id,
-- message.feedbacks[]) and falls back to our own BAP's convention (ratings[0].id = bookingId, feedback in ratings[].feedback_form)
-- for whatever the BAP did not send, so our own BAP's /rating keeps working for pilot merchants.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Rating
  ( buildRatingReq,
  )
where

import qualified Beckn.ACL.Rating as ACL
import Beckn.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Rating as DRating
import EulerHS.Prelude hiding (all, find, id, readMaybe)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildRatingReq ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadFlow m
  ) =>
  Subscriber.Subscriber ->
  Spec.RatingReq ->
  m DRating.DRatingReq
buildRatingReq subscriber req = do
  ACL.validateRatingContext subscriber req
  let message = req.ratingReqMessage
      ratings = fromMaybe [] message.ratingReqMessageRatings
      feedbacks = fromMaybe [] message.ratingReqMessageFeedbacks
  driverRating <- getDriverRating ratings & fromMaybeM (InvalidRequest "Missing driver rating (ref_type AGENT)")
  -- on_confirm sets order.id = booking.id, so order_id is the booking id; ratings[0].id is our own BAP's convention.
  bookingId <- (message.ratingReqMessageOrderId <|> (listToMaybe ratings >>= (.ratingId))) & fromMaybeM (InvalidRequest "Missing order_id")
  ratingValueText <- driverRating.ratingValue & fromMaybeM (InvalidRequest "Missing ratingValue")
  ratingValue <- parseRatingValue ratingValueText & fromMaybeM (InvalidRequest $ "Invalid ratingValue: " <> ratingValueText)
  pure
    DRating.DRatingReq
      { bookingId = Id bookingId,
        ratingValue = ratingValue,
        feedbackDetails = getFeedbackDetails feedbacks driverRating,
        shouldFavDriver = getShouldFavouriteDriver driverRating,
        riderPhoneNum = getRiderPhoneNumber driverRating,
        filePath = getFilePath driverRating,
        riderName = getRiderName driverRating
      }

-- | The driver's rating is the one about the AGENT. ITEM/ORDER/FULFILLMENT/PROVIDER ratings are not the driver's.
-- If no rating carries a ref_type at all, the BAP follows our own convention and ratings[0] is the driver's.
getDriverRating :: [Spec.Rating] -> Maybe Spec.Rating
getDriverRating ratings =
  find (refTypeIs Enums.REF_AGENT . (.ratingRefType)) ratings
    <|> (guard (all (isNothing . (.ratingRefType)) ratings) >> listToMaybe ratings)

-- | Accepts integral values ("4") and decimal ones ("4.5", rounded half up) since v2.1.0 does not restrict value to an integer.
-- NaN, Infinity and anything outside 1..5 are rejected.
parseRatingValue :: Text -> Maybe Int
parseRatingValue txt = do
  value <- (readMaybe str :: Maybe Int) <|> (roundDecimal =<< (readMaybe str :: Maybe Double))
  value <$ guard (ACL.isValidRatingValue value)
  where
    str = T.unpack (T.strip txt)
    -- range-check before rounding so huge values can't overflow Int in floor
    roundDecimal d = do
      guard (not (isNaN d || isInfinite d) && d >= 1 && d <= 5)
      pure (floor (d + 0.5))

-- | feedbackDetails slot 0 is the free-text review (see Domain.Action.Beckn.Rating.handler); slots 1 and 2 (assistance offered, issue id)
-- are our BAP's extensions and only come from feedback_form. The v2.1.0 feedbacks[] comment, keyed to the driver rating, wins for slot 0.
getFeedbackDetails :: [Spec.Feedback] -> Spec.Rating -> [Maybe Text]
getFeedbackDetails feedbacks driverRating =
  case ACL.tfFeedbackDetails driverRating of
    (formComment : rest) -> (v21Comment <|> formComment) : rest
    [] -> [v21Comment, Nothing, Nothing]
  where
    v21Comment = do
      ratingRefType <- driverRating.ratingRefType
      feedback <- find (matchesRating ratingRefType) feedbacks
      feedback.feedbackComment
    matchesRating ratingRefType f =
      (normaliseRefType <$> f.feedbackRefType) == Just (normaliseRefType ratingRefType)
        && fromMaybe True ((==) <$> f.feedbackRefId <*> driverRating.ratingRefId)

refTypeIs :: Enums.RatingRefType -> Maybe Text -> Bool
refTypeIs expected = (== Just expected) . (>>= readRefType)
  where
    readRefType = readMaybe . T.unpack . ("REF_" <>) . normaliseRefType

normaliseRefType :: Text -> Text
normaliseRefType = T.toUpper . T.strip
