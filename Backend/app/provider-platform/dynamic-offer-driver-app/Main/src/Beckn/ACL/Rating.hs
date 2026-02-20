{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Rating where

import Beckn.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Rating as DRating
import EulerHS.Prelude (safeHead)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildRatingReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Subscriber.Subscriber ->
  Spec.RatingReq ->
  m DRating.DRatingReq
buildRatingReqV2 subscriber req = do
  let context = req.ratingReqContext
  ContextV2.validateContext Context.RATING context
  bap_id <- context.contextBapId & fromMaybeM (InvalidRequest "Missing bap_id")
  unless (subscriber.subscriber_id == bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  ratings <- req.ratingReqMessage.ratingReqMessageRatings & fromMaybeM (InvalidRequest "Missing ratings")
  rating <- safeHead ratings & fromMaybeM (InvalidRequest "Missing rating")
  bookingId <- rating.ratingId & fromMaybeM (InvalidRequest "Missing ratingId")
  ratingValueText <- rating.ratingValue & fromMaybeM (InvalidRequest "Missing ratingValue")
  let mbRatingValue = readMaybe $ T.unpack ratingValueText
      mbShouldFavDriver = getShouldFavouriteDriver rating
      mbRiderPhoneNum = getRiderPhoneNumber rating
      mbFilePath = getFilePath rating
      mbRiderName = getRiderName rating
  ratingValue <- mbRatingValue & fromMaybeM (InvalidRequest "Invalid ratingValue")
  pure
    DRating.DRatingReq
      { bookingId = Id bookingId,
        ratingValue = ratingValue,
        feedbackDetails = tfFeedbackDetails rating,
        shouldFavDriver = mbShouldFavDriver,
        riderPhoneNum = mbRiderPhoneNum,
        filePath = mbFilePath,
        riderName = mbRiderName
      }

tfFeedbackDetails :: Spec.Rating -> [Maybe Text]
tfFeedbackDetails rating = do
  let feedbackForms = rating.ratingFeedbackForm
      feedbackFormList = fold feedbackForms
      feedback_form = find (\form -> form.feedbackFormQuestion == "Evaluate your ride experience.") feedbackFormList
      wasOfferedAssistance = find (\form -> form.feedbackFormQuestion == "Was Assistance Offered?") feedbackFormList
      mbIssueId = find (\form -> form.feedbackFormQuestion == "Get IssueId.") feedbackFormList
  [ feedback_form >>= (.feedbackFormAnswer),
    wasOfferedAssistance >>= (.feedbackFormAnswer),
    mbIssueId >>= (.feedbackFormAnswer)
    ]
