{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.UI.Rewards
  ( getRewards,
    postRewardsClaim,
    postRewardsRedeemed,
    getRewardsHomeCard,
  )
where

import qualified API.Types.UI.Rewards as API
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import qualified Domain.Types.RewardUnlock as DRU
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RewardCampaign as QRCmp
import qualified Storage.Queries.RewardCampaignExtra as QRCmpE
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

-- | The home-screen promo card of the rider's first reward they have not unlocked yet,
-- read from the cohort's @presentation.homeCard@ (campaigns, then cohorts, by displayOrder).
-- Read-only, unlike 'getRewards', so the home screen can call it on every visit. Any unlock
-- of the cohort that is not Reclaimed (Active, Redeemed or expired) hides its card for good.
getRewardsHomeCard ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Flow API.RewardHomeCardResp
getRewardsHomeCard (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "personId missing from token")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let moCityId = person.merchantOperatingCityId
  enabled <- maybe False (.enableRewardsManagement) <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = moCityId.getId}) Nothing
  if not enabled
    then pure $ API.RewardHomeCardResp Nothing
    else do
      now <- getCurrentTime
      campaigns <- sortOn (.displayOrder) <$> QRCmpE.findAllActiveInCityAtTime moCityId now
      candidates <- fmap concat . forM campaigns $ \campaign -> do
        cohorts <- sortOn (.displayOrder) <$> QRC.findAllByCampaign campaign.id
        pure [(cohort.id, card) | cohort <- cohorts, Just card <- [homeCardOf cohort]]
      if null candidates
        then pure $ API.RewardHomeCardResp Nothing
        else do
          unlocks <- QRU.findByPerson personId
          let unlockedCohortIds = [u.cohortId | u <- unlocks, u.status /= DRU.Reclaimed]
          pure . API.RewardHomeCardResp $ snd <$> find (\(cohortId, _) -> cohortId `notElem` unlockedCohortIds) candidates

homeCardOf :: DRC.RewardCohort -> Maybe API.RewardHomeCard
homeCardOf cohort = case cohort.presentation of
  Just (A.Object o) | Just v <- KM.lookup "homeCard" o, A.Success card <- A.fromJSON v -> Just card
  _ -> Nothing
