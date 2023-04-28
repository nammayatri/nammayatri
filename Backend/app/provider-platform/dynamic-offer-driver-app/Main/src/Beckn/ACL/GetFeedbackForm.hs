{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.GetFeedbackForm where

import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import Beckn.Types.Core.Taxi.Rating.FeedbackForm
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Storage.Queries.FeedbackForm
import Storage.Queries.RatingCategories
import Tools.Error

buildFeedbackFormResp ::
  (HasFlowEnv m r '["coreVersion" ::: Text], Esq.EsqDBReplicaFlow m r) =>
  Subscriber.Subscriber ->
  Rating.GetFeedbackFormReq ->
  m Rating.GetFeedbackFormResp
buildFeedbackFormResp subscriber req = do
  let ctx = req.context
  validateContext Context.GET_FEEDBACK_FORM ctx
  unless (subscriber.subscriber_id == ctx.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == ctx.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let mes = req.message
  categoryId <- Esq.runInReplica $ findByCategoryName (mes.rating_category) >>= fromMaybeM (RatingCategroyDoesNotExist $ show mes.rating_category)
  feedbackForm <- Esq.runInReplica $ findByCategoryIdAndRatingValue categoryId mes.rating_value >>= fromMaybeM (FeedbackFormDoesNotExist (show mes.rating_category) mes.rating_value)
  return $ FeedbackFormAPIEntity {id = mes.rating_value, question = feedbackForm.question, answer_type = feedbackForm.answer_type}
