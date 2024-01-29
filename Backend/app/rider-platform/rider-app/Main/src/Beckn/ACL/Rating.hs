{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Rating (buildRatingReq) where

import qualified Beckn.Types.Core.Taxi.Rating as Rating
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.UI.Feedback as DFeedback
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common

buildRatingReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DFeedback.FeedbackRes ->
  m (BecknReq Rating.RatingMessage)
buildRatingReq DFeedback.FeedbackRes {..} = do
  msgId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- buildTaxiContext Context.RATING msgId (Just transactionId) merchant.bapId bapUrl (Just providerId) (Just providerUrl) merchant.defaultCity merchant.country False
  let message =
        Rating.RatingMessage
          { id = bppBookingId.getId,
            value = ratingValue,
            feedback_form =
              [ Rating.FeedbackForm
                  { question = "Evaluate your ride experience.",
                    answer = feedbackDetails
                  },
                Rating.FeedbackForm
                  { question = "Was Assistance Offered?",
                    answer = case wasOfferedAssistance of
                      Just True -> Just "True"
                      Just False -> Just "False"
                      _ -> Nothing
                  },
                Rating.FeedbackForm
                  { question = "Get IssueId.",
                    answer = issueId
                  }
              ]
          }
  pure $ BecknReq context message

_buildRatingReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DFeedback.FeedbackRes ->
  m Spec.RatingReq
_buildRatingReqV2 res@DFeedback.FeedbackRes {..} = do
  msgId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  context <- ContextV2.buildContextV2 Context.RATING Context.MOBILITY msgId (Just transactionId) merchant.bapId bapUrl (Just providerId) (Just providerUrl) merchant.defaultCity merchant.country
  let message = _tfMessage res
  pure $
    Spec.RatingReq
      { ratingReqContext = context,
        ratingReqMessage = message
      }

_tfMessage :: DFeedback.FeedbackRes -> Spec.RatingReqMessage
_tfMessage res = do
  Spec.RatingReqMessage
    { ratingReqMessageRatings = Just [tfRating res]
    }

tfRating :: DFeedback.FeedbackRes -> Spec.Rating
tfRating res@DFeedback.FeedbackRes {..} = do
  Spec.Rating
    { ratingId = Just $ bppBookingId.getId,
      ratingValue = Just $ show ratingValue,
      ratingRatingCategory = Nothing,
      ratingFeedbackForm = Just $ _tfFeedbackForm res
    }

_tfFeedbackForm :: DFeedback.FeedbackRes -> [Spec.FeedbackForm]
_tfFeedbackForm DFeedback.FeedbackRes {..} = do
  [ Spec.FeedbackForm
      { feedbackFormQuestion = "Evaluate your ride experience.",
        feedbackFormAnswer = feedbackDetails
      },
    Spec.FeedbackForm
      { feedbackFormQuestion = "Was Assistance Offered?",
        feedbackFormAnswer = case wasOfferedAssistance of
          Just True -> Just "True"
          Just False -> Just "False"
          _ -> Nothing
      },
    Spec.FeedbackForm
      { feedbackFormQuestion = "Get IssueId.",
        feedbackFormAnswer = issueId
      }
    ]
