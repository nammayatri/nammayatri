{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.UI.Rewards
  ( getRewards,
    postRewardsClaim,
    postRewardsRedeemed,
    getLiveReward,
  )
where

import qualified API.Types.UI.Rewards as API
import qualified Data.Aeson as A
import qualified Domain.Action.Rewards.LiveReward as LiveReward
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardUnlock as DRU
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.LiveReward as CQLR
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RewardCampaign as QRCmp
import qualified Storage.Queries.RewardCohort as QRC
import qualified Storage.Queries.RewardUnlock as QRU
import qualified Storage.Queries.RewardUnlockExtra as QRUE
import Tools.Error
import qualified Tools.Rewards.RedisPool as Pool
import qualified Tools.Rewards.RiderContextReader as Ctx

getRewards ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  m [API.RewardUnlockSummary]
getRewards (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "personId missing from token")
  now <- getCurrentTime
  QRUE.markActiveAsExpiredIfValidityPassed personId now
  unlocks <- QRU.findByPerson personId
  let visible =
        filter
          ( \u ->
              u.status `elem` [DRU.Active, DRU.Redeemed]
                && maybe True (>= now) u.couponValidTill
          )
          unlocks
  forM visible $ \u -> do
    campaign <- QRCmp.findById u.campaignId >>= fromMaybeM (InternalError "Campaign missing")
    cohort <- QRC.findById u.cohortId >>= fromMaybeM (InternalError "Cohort missing")
    let shouldSetViewed = isNothing u.viewedAt
        shouldSetClaimedAuto =
          campaign.claimMode == DRCmp.AutoClaim && isNothing u.claimedAt
    when (shouldSetViewed || shouldSetClaimedAuto) $
      QRUE.updateViewAndClaimTimestamps
        u.id
        (if shouldSetViewed then Just now else Nothing)
        (if shouldSetClaimedAuto then Just now else Nothing)
    let revealCode = campaign.claimMode == DRCmp.AutoClaim
    pure
      API.RewardUnlockSummary
        { unlockId = u.id,
          campaignName = campaign.name,
          sponsorName = campaign.sponsorName,
          sponsorLogoUrl = campaign.sponsorLogoUrl,
          cohortName = cohort.name,
          rewardTitle = cohort.rewardTitle,
          rewardImageUrl = cohort.rewardImageUrl,
          unlockedAt = u.unlockedAt,
          status = u.status,
          couponValidTill = u.couponValidTill,
          couponCode = if revealCode then u.couponCode else Nothing,
          redemptionTargetType = campaign.redemptionTargetType,
          redemptionTargetUrl = campaign.redemptionTargetUrl,
          claimedAt = u.claimedAt,
          presentation = cohort.presentation
        }

postRewardsClaim ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DRU.RewardUnlock ->
  m API.ClaimCouponResp
postRewardsClaim (mbPersonId, _merchantId) unlockId = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "personId missing from token")
  u <- QRU.findById unlockId >>= fromMaybeM (InvalidRequest "Unlock not found")
  unless (u.personId == personId) $ throwError AccessDenied
  campaign <- QRCmp.findById u.campaignId >>= fromMaybeM (InternalError "Campaign missing")
  when (isNothing u.claimedAt) $ do
    now <- getCurrentTime
    QRUE.updateViewAndClaimTimestamps unlockId Nothing (Just now)
  pure
    API.ClaimCouponResp
      { couponCode = u.couponCode,
        redemptionTargetType = campaign.redemptionTargetType,
        redemptionTargetUrl = campaign.redemptionTargetUrl,
        couponValidTill = u.couponValidTill
      }

postRewardsRedeemed ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DRU.RewardUnlock ->
  m APISuccess
postRewardsRedeemed (mbPersonId, _merchantId) unlockId = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "personId missing from token")
  u <- QRU.findById unlockId >>= fromMaybeM (InvalidRequest "Unlock not found")
  unless (u.personId == personId) $ throwError AccessDenied
  when (u.status == DRU.Active) $ do
    now <- getCurrentTime
    QRUE.markRedeemed unlockId now
  pure Success

-- | The home-screen live reward card of the rider's first reward they can still earn: the
-- city's card candidates (cached, see "Storage.CachedQueries.LiveReward"),
-- live at this moment, that the rider can still unlock under the cohort's repeat cap,
-- whose audience rule (if any) matches the rider, and, for Pool campaigns, whose
-- coupon pool isn't empty. Read-only, unlike 'getRewards', so the home screen can call
-- it on every visit. See "Domain.Action.Rewards.LiveReward" for the pure steps.
getLiveReward ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Flow API.LiveRewardCardResp
getLiveReward (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "personId missing from token")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let moCityId = person.merchantOperatingCityId
  enabled <- maybe False (.enableRewardsManagement) <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = moCityId.getId}) Nothing
  if not enabled
    then pure $ API.LiveRewardCardResp Nothing
    else do
      now <- getCurrentTime
      candidates <- filter (LiveReward.isLiveAt now) <$> CQLR.findCandidatesByCity moCityId
      if null candidates
        then pure $ API.LiveRewardCardResp Nothing
        else do
          unlocks <- QRU.findByPerson personId
          -- The rider context costs a few Redis reads; only audience rules need it.
          riderContext <-
            if any (isJust . (.targetingJsonLogic)) candidates
              then Ctx.readRiderContext personId Nothing
              else pure A.Null
          API.LiveRewardCardResp . fmap (.card) <$> firstWithCoupons (LiveReward.eligibleLiveRewards riderContext unlocks candidates)

-- | The first candidate with a coupon to give. Pool campaigns hand out
-- pre-uploaded codes, so an empty pool means the ride would unlock nothing
-- ('rewards.claim.empty'); checked lazily, one LLEN per Pool candidate tried.
firstWithCoupons :: [LiveReward.LiveRewardCandidate] -> Flow (Maybe LiveReward.LiveRewardCandidate)
firstWithCoupons [] = pure Nothing
firstWithCoupons (c : rest) = do
  hasCoupons <-
    if c.isPoolSourced
      then (> 0) <$> Pool.poolSize c.campaignId c.cohortId
      else pure True
  if hasCoupons then pure (Just c) else firstWithCoupons rest
