{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.UI.Rewards
  ( getRewards,
    postRewardsClaim,
    postRewardsRedeemed,
  )
where

import qualified API.Types.UI.Rewards as API
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardUnlock as DRU
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RewardCampaign as QRCmp
import qualified Storage.Queries.RewardCohort as QRC
import qualified Storage.Queries.RewardUnlock as QRU
import qualified Storage.Queries.RewardUnlockExtra as QRUE
import Tools.Error

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
