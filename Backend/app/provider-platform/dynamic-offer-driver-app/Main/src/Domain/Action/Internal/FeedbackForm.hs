{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FeedbackForm where

import Data.Foldable ()
import qualified Domain.Types.Feedback.Feedback as DFeedback
import Domain.Types.Feedback.FeedbackForm (FeedbackAnswer (FeedbackAnswer), FeedbackFormReq)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.Queries.Feedback.Feedback as QFeedback
import qualified Storage.Queries.Feedback.FeedbackBadge as QFeedbackBadge
import qualified Storage.Queries.Ride as QRide

saveFeedbackFormResult :: FeedbackFormReq -> Flow APISuccess
saveFeedbackFormResult feedbackFormReq = do
  let rideId = feedbackFormReq.rideId
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  feedbackChipsList <- getFeedbackAnswers feedbackFormReq
  addFeedback feedbackChipsList rideId ride.driverId
  updateFeedbackBadge feedbackChipsList ride.driverId
  pure Success

getFeedbackAnswers :: Applicative m => FeedbackFormReq -> m [Text]
getFeedbackAnswers req = do
  let feedbackList = req.feedback
  let answerLists = getAnswerLists feedbackList
  pure $ concat answerLists
  where
    getAnswerLists :: [FeedbackAnswer] -> [[Text]]
    getAnswerLists = map (\(FeedbackAnswer _ answers) -> answers)

addFeedback :: [Text] -> Id DRide.Ride -> Id DP.Person -> Flow ()
addFeedback feedbackChipsList rideId driverId = do
  unless (null feedbackChipsList) $ do
    newFeedbacks <- generateFeedback feedbackChipsList
    QFeedback.create newFeedbacks
  where
    generateFeedback :: MonadFlow m => [Text] -> m DFeedback.Feedback
    generateFeedback badges = do
      id <- Id <$> L.generateGUID
      now <- getCurrentTime
      pure $
        DFeedback.Feedback
          { createdAt = now,
            ..
          }

updateFeedbackBadge :: [Text] -> Id DP.Person -> Flow ()
updateFeedbackBadge feedbackChipsList driverId = do
  unless (null feedbackChipsList) $
    traverse_ insertOrUpdate feedbackChipsList
  where
    insertOrUpdate badge = do
      feedbackBadge <- QFeedbackBadge.findFeedbackBadgeForDriver driverId badge
      case feedbackBadge of
        Just feedbackBadgeItem -> do
          let badgeCount = feedbackBadgeItem.badgeCount + 1
          QFeedbackBadge.updateFeedbackBadge feedbackBadgeItem badgeCount
        Nothing -> do
          newFeedbackBadge <- buildFeedbackBadge driverId badge
          QFeedbackBadge.createFeedbackBadge newFeedbackBadge

buildFeedback :: MonadFlow m => Id DRide.Ride -> Id DP.Person -> [Text] -> m DFeedback.Feedback
buildFeedback rideId driverId badges = do
  -- handle badge
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  pure $
    DFeedback.Feedback
      { createdAt = now,
        ..
      }

buildFeedbackBadge :: MonadFlow m => Id DP.Person -> Text -> m DFeedback.FeedbackBadge
buildFeedbackBadge driverId badge = do
  let badgeCount = 1
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  pure $
    DFeedback.FeedbackBadge
      { createdAt = now,
        updatedAt = now,
        ..
      }
