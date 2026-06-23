{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.Rewards.Consumer
  ( evaluateRewardsForRider,
    handleMessage,
  )
where

import qualified Domain.Action.Rewards.Coupon as Coupon
import qualified Domain.Action.Rewards.Evaluator as Eval
import qualified Domain.Action.Rewards.Producer as Producer
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import qualified Domain.Types.RewardUnlock as DRU
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RewardCampaignExtra as QRCmpE
import qualified Storage.Queries.RewardCohort as QRC
import qualified Storage.Queries.RewardUnlock as QRU
import qualified Storage.Queries.RewardUnlockExtra as QRUE
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Rewards.RedisPool as Pool
import qualified Tools.Rewards.RiderContextReader as Ctx

maxPushesPerMessage :: Int
maxPushesPerMessage = 5

dedupTtlSeconds :: Int
dedupTtlSeconds = 3600

dedupKey :: Id Producer.RewardEvent -> Text
dedupKey eid = "rewards:event-dedup:" <> eid.getId

handleMessage ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Hedis.HedisFlow m r,
    ServiceFlow m r
  ) =>
  Producer.RewardEvalRequested ->
  m ()
handleMessage Producer.RewardEvalRequested {eventId, riderId, merchantOperatingCityId, completedAt} = do
  firstTime <- Hedis.setNxExpire (dedupKey eventId) dedupTtlSeconds ("1" :: Text)
  if not firstTime
    then
      logInfo $
        "rewards.consume.duplicate eventId="
          <> eventId.getId
          <> " riderId="
          <> riderId.getId
    else do
      result <-
        try @_ @SomeException $
          evaluateRewardsForRider riderId merchantOperatingCityId completedAt
      case result of
        Right _ -> pure ()
        Left e -> do
          void $ Hedis.del (dedupKey eventId)
          logError $
            "rewards.consume.failed eventId="
              <> eventId.getId
              <> " riderId="
              <> riderId.getId
              <> " err="
              <> show e
          throwM e

evaluateRewardsForRider ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Hedis.HedisFlow m r, ServiceFlow m r) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  m ()
evaluateRewardsForRider riderId moCityId completedAt = do
  context <- Ctx.readRiderContext riderId
  activeCampaigns <- QRCmpE.findAllActiveInCityAtTime moCityId completedAt
  foldM_
    ( \pushesSoFar campaign -> do
        cohorts <- QRC.findAllByCampaign campaign.id
        existing <- QRU.findByPersonAndCampaign riderId campaign.id
        let existingNonReclaimed = [u.cohortId | u <- existing, u.status /= DRU.Reclaimed]
        matched <- Eval.evaluateCohorts context cohorts
        let toUnlock = filter (`notElem` existingNonReclaimed) matched
        foldM
          ( \sentSoFar cohortId -> do
              cohort <-
                case filter (\c -> c.id == cohortId) cohorts of
                  (x : _) -> pure x
                  _ -> throwError $ InternalError "Consumer: matched cohort not in cohorts list"
              mCode <- Coupon.claimCoupon campaign cohort riderId
              case mCode of
                Nothing -> pure sentSoFar
                Just code -> do
                  now <- getCurrentTime
                  uid <- generateGUID
                  let unlock =
                        DRU.RewardUnlock
                          { id = uid,
                            personId = riderId,
                            campaignId = campaign.id,
                            cohortId = cohortId,
                            merchantId = Just campaign.merchantId,
                            merchantOperatingCityId = Just campaign.merchantOperatingCityId,
                            unlockedAt = now,
                            couponCode = Just code,
                            couponSource = campaign.couponSourceType,
                            couponValidTill =
                              case cohort.couponValidityDays of
                                Just days -> Just $ addUTCTime (fromIntegral (days * 86400)) now
                                Nothing -> campaign.endsAt,
                            status = DRU.Active,
                            viewedAt = Nothing,
                            claimedAt = Nothing,
                            redeemedAt = Nothing,
                            reclaimedAt = Nothing,
                            createdAt = now,
                            updatedAt = now
                          }
                  inserted <- QRUE.createIfNoActive unlock
                  if inserted
                    then do
                      Coupon.finalizeClaim campaign cohort code
                      if sentSoFar < maxPushesPerMessage
                        then do
                          sendUnlockPush riderId campaign cohort code
                          pure (sentSoFar + 1)
                        else pure sentSoFar
                    else do
                      when (campaign.couponSourceType == DRCmp.Pool) $ do
                        Pool.pushBackToPool campaign.id cohort.id code
                        Pool.removeFromInflight campaign.id cohort.id code
                      pure sentSoFar
          )
          pushesSoFar
          toUnlock
    )
    0
    activeCampaigns

sendUnlockPush ::
  (MonadFlow m, EsqDBFlow m r, ServiceFlow m r) =>
  Id DP.Person ->
  DRCmp.RewardCampaign ->
  DRC.RewardCohort ->
  Text ->
  m ()
sendUnlockPush rid campaign cohort code = do
  fork "rewards unlock push" $ do
    result <- try @_ @SomeException $ do
      person <- QPerson.findById rid >>= fromMaybeM (PersonDoesNotExist rid.getId)
      let (title, body) =
            case campaign.claimMode of
              DRCmp.AutoClaim ->
                ( "You've unlocked " <> cohort.rewardTitle <> "!",
                  "You've unlocked " <> cohort.rewardTitle <> " from " <> campaign.sponsorName <> ". Code: " <> code <> ". Tap to use."
                )
              DRCmp.ManualClaim ->
                ( "You've unlocked " <> cohort.rewardTitle <> "!",
                  "You've unlocked " <> cohort.rewardTitle <> " from " <> campaign.sponsorName <> ". Tap to view."
                )
          notificationData =
            Notification.NotificationReq
              { category = Notification.TRIGGER_FCM,
                subCategory = Nothing,
                showNotification = Notification.SHOW,
                messagePriority = Nothing,
                entity = Notification.Entity Notification.Person rid.getId EmptyDynamicParam,
                body = body,
                title = title,
                dynamicParams = EmptyDynamicParam,
                auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
                ttl = Nothing,
                sound = Just "default"
              }
      Notify.notifyPerson person.merchantId person.merchantOperatingCityId person.id notificationData Nothing
    case result of
      Right _ ->
        logInfo $
          "rewards.unlock.push.sent riderId=" <> rid.getId <> " campaign=" <> campaign.name <> " cohort=" <> cohort.rewardTitle
      Left e ->
        logError $
          "rewards.unlock.push.failed riderId=" <> rid.getId <> " campaign=" <> campaign.name <> " cohort=" <> cohort.rewardTitle <> " err=" <> show e
