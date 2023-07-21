{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Feedback.FeedbackBadge where

import Data.Text
import Domain.Types.Feedback.Feedback (FeedbackBadge)
import Domain.Types.Person (Person)
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Time
import Storage.Tabular.Feedback.FeedbackBadge
import Prelude

createFeedbackBadge :: FeedbackBadge -> SqlDB ()
createFeedbackBadge = Esq.create

findFeedbackBadgeForDriver :: Transactionable m => Id Person -> Text -> m (Maybe FeedbackBadge)
findFeedbackBadgeForDriver driverId badge = findOne $ do
  feedbackBadge <- from $ table @FeedbackBadgeT
  where_ $ feedbackBadge ^. FeedbackBadgeDriverId ==. val (toKey driverId) &&. feedbackBadge ^. FeedbackBadgeBadge ==. val badge
  pure feedbackBadge

updateFeedbackBadge :: FeedbackBadge -> Int -> SqlDB ()
updateFeedbackBadge feedbackBadge newBadgeCount = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ FeedbackBadgeBadgeCount =. val newBadgeCount,
        FeedbackBadgeUpdatedAt =. val now
      ]
    where_ $
      tbl ^. FeedbackBadgeTId ==. val (toKey feedbackBadge.id)
        &&. tbl ^. FeedbackBadgeDriverId ==. val (toKey feedbackBadge.driverId)

findAllFeedbackBadgeForDriver :: Transactionable m => Id Person -> m [FeedbackBadge]
findAllFeedbackBadgeForDriver driverId = findAll $ do
  feedbackBadges <- from $ table @FeedbackBadgeT
  where_ $ feedbackBadges ^. FeedbackBadgeDriverId ==. val (toKey driverId)
  pure feedbackBadges
