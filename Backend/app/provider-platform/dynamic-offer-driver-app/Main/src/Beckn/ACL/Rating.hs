{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Rating where

import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import Beckn.Types.Core.Taxi.Rating.FeedbackForm
import qualified Domain.Action.Beckn.Rating as DRating
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
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
  let feedbackFormParsedAsObject = readMaybe (show req.message.feedback_form) :: Maybe FeedbackForm
  let feedbackFormParsedAsArray = readMaybe (show req.message.feedback_form) :: Maybe [FeedbackForm]
  feedbackFormList <- do
    case (feedbackFormParsedAsObject, feedbackFormParsedAsArray) of
      (Just object, _) -> return [object]
      (Nothing, Just array) -> return array
      (_, _) -> return []
  let feedback_form = find (\form -> form.question == "Evaluate your ride experience.") feedbackFormList
  let wasOfferedAssistance = find (\form -> form.question == "Was Assistance Offered?") feedbackFormList
  validateContext Context.RATING context
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  pure
    DRating.DRatingReq
      { bookingId = Id $ req.message.id,
        ratingValue = req.message.value,
        feedbackDetails =
          [ case feedback_form of
              Just feedbck_form -> feedbck_form.answer
              _ -> Nothing,
            case wasOfferedAssistance of
              Just offeredAssistance -> offeredAssistance.answer
              _ -> Nothing
          ],
        issueId = req.message.issueId
      }
