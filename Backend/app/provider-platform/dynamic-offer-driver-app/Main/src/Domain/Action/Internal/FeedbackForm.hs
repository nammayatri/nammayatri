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
import qualified Domain.Types.Feedback as DFeedback
import Domain.Types.Feedback.FeedbackForm (BadgeMetadata (..), FeedbackAnswer (FeedbackAnswer), FeedbackFormReq (..))
import qualified Domain.Types.FeedbackBadge as DFeedbackBadge
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Prelude as P
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as ST
import qualified SharedLogic.Allocator as Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as QTC
import qualified Storage.Queries.Feedback as QFeedback
import qualified Storage.Queries.FeedbackBadge as QFeedbackBadge
import qualified Storage.Queries.FeedbackBadgeExtra as QFeedbackBadgeExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Notifications as Notify

saveFeedbackFormResult :: FeedbackFormReq -> Flow APISuccess
saveFeedbackFormResult feedbackFormReq = do
  let rideId = feedbackFormReq.rideId
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  config <- QTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  let shouldSendPN = maybe False (.enableFeedbackNotification) config.feedbackNotificationConfig
  case feedbackFormReq.badges of
    Just badgeMetadataList | not (null badgeMetadataList) -> do
      addFeedbackWithBadgeKeys badgeMetadataList ride ride.driverId feedbackFormReq.rating
      updateFeedbackBadgeWithKeys badgeMetadataList ride.driverId ride.merchantId ride.merchantOperatingCityId
      when shouldSendPN $
        whenJust feedbackFormReq.rating $ \rating -> do
          scheduleFeedbackPN ride rating (Just badgeMetadataList) config.feedbackNotificationConfig
    _ -> do
      feedbackChipsList <- getFeedbackAnswers feedbackFormReq
      addFeedback feedbackChipsList ride ride.driverId feedbackFormReq.rating
      updateFeedbackBadgeOld feedbackChipsList ride.driverId ride.merchantId ride.merchantOperatingCityId
      when shouldSendPN $
        whenJust feedbackFormReq.rating $ \rating -> do
          scheduleFeedbackPN ride rating Nothing config.feedbackNotificationConfig

  pure Success

-- OLD FLOW: For backward compatibility (will be deprecated)
getFeedbackAnswers :: Applicative m => FeedbackFormReq -> m [Text]
getFeedbackAnswers req = do
  let feedbackList = fromMaybe [] req.feedback
  let answerLists = getAnswerLists feedbackList
  pure $ concat answerLists
  where
    getAnswerLists :: [FeedbackAnswer] -> [[Text]]
    getAnswerLists = map (\(FeedbackAnswer _ answers) -> answers)

addFeedback ::
  [Text] ->
  DRide.Ride ->
  Id DP.Person ->
  Maybe Int ->
  Flow ()
addFeedback feedbackChipsList ride driverId rating = do
  unless (null feedbackChipsList) $ do
    newFeedbacks <- generateFeedbackList feedbackChipsList
    QFeedback.createMany newFeedbacks
  where
    generateFeedbackList :: MonadFlow m => [Text] -> m [DFeedback.Feedback]
    generateFeedbackList = mapM (buildFeedback ride driverId rating)

addFeedbackWithBadgeKeys ::
  [BadgeMetadata] ->
  DRide.Ride ->
  Id DP.Person ->
  Maybe Int ->
  Flow ()
addFeedbackWithBadgeKeys badgeMetadataList ride driverId rating = do
  unless (null badgeMetadataList) $ do
    newFeedbacks <- generateFeedbackListWithKeys badgeMetadataList
    QFeedback.createMany newFeedbacks
  where
    generateFeedbackListWithKeys :: MonadFlow m => [BadgeMetadata] -> m [DFeedback.Feedback]
    generateFeedbackListWithKeys = mapM (buildFeedbackWithKey ride driverId rating)

buildFeedbackWithKey ::
  MonadFlow m =>
  DRide.Ride ->
  Id DP.Person ->
  Maybe Int ->
  BadgeMetadata ->
  m DFeedback.Feedback
buildFeedbackWithKey ride driverId rating badgeMetadata = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  pure $
    DFeedback.Feedback
      { badge = badgeMetadata.badgeText,
        badgeKey = Just badgeMetadata.badgeKey,
        createdAt = now,
        rideId = ride.id,
        driverId = driverId,
        merchantId = ride.merchantId,
        merchantOperatingCityId = Just ride.merchantOperatingCityId,
        ..
      }

updateFeedbackBadgeWithKeys :: [BadgeMetadata] -> Id DP.Person -> Maybe (Id DM.Merchant) -> Id DMOC.MerchantOperatingCity -> Flow ()
updateFeedbackBadgeWithKeys badgeMetadataList driverId merchantId merchantOperatingCityId = do
  unless (null badgeMetadataList) $
    traverse_ insertOrUpdateWithKey badgeMetadataList
  where
    insertOrUpdateWithKey :: BadgeMetadata -> Flow ()
    insertOrUpdateWithKey badgeMetadata = do
      feedbackBadge <- QFeedbackBadgeExtra.findFeedbackBadgeByKey driverId badgeMetadata.badgeKey
      case feedbackBadge of
        Just feedbackBadgeItem -> do
          let badgeCount = feedbackBadgeItem.badgeCount + 1
          QFeedbackBadgeExtra.updateFeedbackBadge feedbackBadgeItem badgeCount
        Nothing -> do
          newFeedbackBadge <- buildFeedbackBadgeWithKey driverId badgeMetadata merchantId merchantOperatingCityId
          QFeedbackBadge.create newFeedbackBadge

buildFeedbackBadgeWithKey :: MonadFlow m => Id DP.Person -> BadgeMetadata -> Maybe (Id DM.Merchant) -> Id DMOC.MerchantOperatingCity -> m DFeedbackBadge.FeedbackBadge
buildFeedbackBadgeWithKey driverId badgeMetadata merchantId merchantOperatingCityId = do
  let badgeCount = 1
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  pure $
    DFeedbackBadge.FeedbackBadge
      { id = id,
        badge = badgeMetadata.badgeText,
        badgeKey = Just badgeMetadata.badgeKey,
        badgeCount = badgeCount,
        driverId = driverId,
        createdAt = now,
        updatedAt = now,
        merchantId = merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId
      }

-- OLD FLOW: For backward compatibility (DEPRECATED)
updateFeedbackBadgeOld :: [Text] -> Id DP.Person -> Maybe (Id DM.Merchant) -> Id DMOC.MerchantOperatingCity -> Flow ()
updateFeedbackBadgeOld feedbackChipsList driverId merchantId merchantOperatingCityId = do
  unless (null feedbackChipsList) $
    traverse_ insertOrUpdate feedbackChipsList
  where
    insertOrUpdate badge = do
      feedbackBadge <- QFeedbackBadgeExtra.findFeedbackBadgeForDriver driverId badge
      case feedbackBadge of
        Just feedbackBadgeItem -> do
          let badgeCount = feedbackBadgeItem.badgeCount + 1
          QFeedbackBadgeExtra.updateFeedbackBadge feedbackBadgeItem badgeCount
        Nothing -> do
          newFeedbackBadge <- buildFeedbackBadge driverId badge merchantId merchantOperatingCityId
          QFeedbackBadge.create newFeedbackBadge

buildFeedback ::
  MonadFlow m =>
  DRide.Ride ->
  Id DP.Person ->
  Maybe Int ->
  Text ->
  m DFeedback.Feedback
buildFeedback ride driverId rating badge = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  pure $
    DFeedback.Feedback
      { id = id,
        badge = badge,
        badgeKey = Nothing,
        createdAt = now,
        rideId = ride.id,
        driverId = driverId,
        merchantId = ride.merchantId,
        merchantOperatingCityId = Just ride.merchantOperatingCityId,
        ..
      }

buildFeedbackBadge :: MonadFlow m => Id DP.Person -> Text -> Maybe (Id DM.Merchant) -> Id DMOC.MerchantOperatingCity -> m DFeedbackBadge.FeedbackBadge
buildFeedbackBadge driverId badge merchantId merchantOperatingCityId = do
  let badgeCount = 1
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  pure $
    DFeedbackBadge.FeedbackBadge
      { id = id,
        badge = badge,
        badgeKey = Nothing,
        badgeCount = badgeCount,
        driverId = driverId,
        createdAt = now,
        updatedAt = now,
        merchantId = merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId
      }

scheduleFeedbackPN :: DRide.Ride -> Int -> Maybe [BadgeMetadata] -> Maybe DTC.FeedbackNotificationConfig -> Flow ()
scheduleFeedbackPN ride rating mbBadgeMetadataList mbFeedbackConfig = do
  whenJust mbFeedbackConfig $ \feedbackConfig -> do
    now <- getCurrentTime
    let selectedBadgeKey = case mbBadgeMetadataList of
          Just badgeList -> selectPNBadge badgeList
          Nothing -> Nothing
        tripEndTime = fromMaybe now ride.tripEndTime
        targetTime = addUTCTime (fromIntegral feedbackConfig.feedbackNotificationDelayInSec) tripEndTime
        delaySeconds = diffUTCTime targetTime now
    if delaySeconds <= 0
      then sendFeedbackPNImmediately ride.driverId ride.merchantOperatingCityId selectedBadgeKey rating
      else do
        let jobData =
              Allocator.SendFeedbackPNJobData
                { driverId = ride.driverId,
                  badgeKey = selectedBadgeKey,
                  rating = rating
                }
        ST.createJobIn @_ @'Allocator.SendFeedbackPN ride.merchantId (Just ride.merchantOperatingCityId) delaySeconds jobData

sendFeedbackPNImmediately :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Int -> Flow ()
sendFeedbackPNImmediately driverId merchantOpCityId mbBadgeKey rating = do
  driver <- B.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  badgeCount <- case mbBadgeKey of
    Just key -> do
      mbBadge <- QFeedbackBadgeExtra.findFeedbackBadgeByKey driverId key
      pure $ (.badgeCount) <$> mbBadge
    Nothing -> pure Nothing
  let entityData =
        Notify.FeedbackBadgeEntityData
          { badgeKey = mbBadgeKey,
            rating = rating,
            badgeCount = badgeCount
          }
  Notify.sendFeedbackBadgeNotification merchantOpCityId driver entityData

selectPNBadge :: [BadgeMetadata] -> Maybe Text
selectPNBadge badges =
  let sendPNBadges = filter (.sendPN) badges
      sortedBadges = sortOn (\b -> (fromMaybe 999 b.priority, b.badgeKey)) sendPNBadges
   in (.badgeKey) <$> P.listToMaybe sortedBadges
