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
    evaluateRewardsIfEnabled,
    recordValidRideAndEvaluateRewardsIfEnabled,
  )
where

import qualified Domain.Action.Rewards.CampaignBlock as Block
import qualified Domain.Action.Rewards.Coupon as Coupon
import qualified Domain.Action.Rewards.Evaluator as Eval
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import qualified Domain.Types.RewardContext as DRCtx
import qualified Domain.Types.RewardUnlock as DRU
import qualified Domain.Types.Ride as DRide
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RewardCampaignExtra as QRCmpE
import qualified Storage.Queries.RewardCohort as QRC
import qualified Storage.Queries.RewardUnlock as QRU
import qualified Storage.Queries.RewardUnlockExtra as QRUE
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Rewards.RedisPool as Pool
import qualified Tools.Rewards.RiderContextReader as Ctx
import qualified Tools.Rewards.ValidRideLog as ValidRideLog

maxPushesPerMessage :: Int
maxPushesPerMessage = 5

-- | Gated entry point: evaluate rewards for a rider iff the city has rewards
-- management enabled. Mirrors the old producer's gate so it is a drop-in for both
-- the ride-completion and triggerEval call sites. Callers should run it in a fork.
evaluateRewardsIfEnabled ::
  ( MonadFlow m,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    EsqDBFlow m r,
    CacheFlow m r,
    Hedis.HedisFlow m r,
    ServiceFlow m r,
    EncFlow m r
  ) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  DRCtx.RewardRideInput ->
  m ()
evaluateRewardsIfEnabled riderId moCityId completedAt rideInput = do
  enabled <- isRewardsEnabled moCityId
  when enabled $ evaluateRewardsForRider riderId moCityId completedAt rideInput

-- | Ride-completion entry point. Only in cities with rewards enabled: records a
-- valid ride in the campaign-block valid-ride log, then evaluates rewards. The
-- ride is recorded first so it is included in its own evaluation's counts.
-- Cities without rewards write nothing to the log.
recordValidRideAndEvaluateRewardsIfEnabled ::
  ( MonadFlow m,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    EsqDBFlow m r,
    CacheFlow m r,
    Hedis.HedisFlow m r,
    ServiceFlow m r,
    EncFlow m r
  ) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  UTCTime ->
  DRCtx.RewardRideInput ->
  m ()
recordValidRideAndEvaluateRewardsIfEnabled riderId moCityId rideId completedAt rideInput = do
  enabled <- isRewardsEnabled moCityId
  when enabled $ do
    when (rideInput.isValid == Just True) $
      void $ withTryCatch "rewards:recordValidRide" $ ValidRideLog.recordValidRide riderId moCityId rideId completedAt
    evaluateRewardsForRider riderId moCityId completedAt rideInput

isRewardsEnabled :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Bool
isRewardsEnabled moCityId =
  maybe False (.enableRewardsManagement) <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = moCityId.getId}) Nothing

evaluateRewardsForRider ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Hedis.HedisFlow m r, ServiceFlow m r, EncFlow m r, HasFlowEnv m r '["maxNotificationShards" ::: Int]) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  DRCtx.RewardRideInput ->
  m ()
evaluateRewardsForRider riderId moCityId completedAt rideInput = do
  riderContext <- Ctx.readRiderContext riderId rideInput
  activeCampaigns <- QRCmpE.findAllActiveInCityAtTime moCityId completedAt
  logInfo $
    "rewards.eval.start riderId="
      <> riderId.getId
      <> " moCityId="
      <> moCityId.getId
      <> " activeCampaignCount="
      <> show (length activeCampaigns)
      <> " context="
      <> encodeToText (DRCtx.rewardContextToLogicInput riderContext)
  foldM_
    ( \pushesSoFar campaign -> do
        cohorts <- QRC.findAllByCampaign campaign.id
        existing <- QRU.findByPersonAndCampaign riderId campaign.id
        campaignContext <- Ctx.withCampaignBlockCounts riderId campaign completedAt riderContext
        let context = DRCtx.rewardContextToLogicInput campaignContext
        let existingNonReclaimed = [u.cohortId | u <- existing, u.status /= DRU.Reclaimed]
        matched <- Eval.evaluateCohorts context cohorts
        let isRepeatableCohort c = isJust c.maxUnlocksPerCohort || isJust c.unlockResetPeriod
        let isRepeatableCohortId cid = maybe False isRepeatableCohort (find (\c -> c.id == cid) cohorts)
        let toUnlock = filter (\cid -> cid `notElem` existingNonReclaimed || isRepeatableCohortId cid) matched
        when (not (null cohorts)) $
          logInfo $
            "rewards.eval.campaign riderId="
              <> riderId.getId
              <> " campaignId="
              <> campaign.id.getId
              <> " validRidesThisCampaignDay="
              <> show campaignContext.validRidesThisCampaignDay
              <> " validRidesThisCampaignWeek="
              <> show campaignContext.validRidesThisCampaignWeek
              <> " matchedCohortIds="
              <> show (map (.getId) matched)
              <> " toUnlockCohortIds="
              <> show (map (.getId) toUnlock)
        foldM
          ( \sentSoFar cohortId -> do
              cohort <-
                case filter (\c -> c.id == cohortId) cohorts of
                  (x : _) -> pure x
                  _ -> throwError $ InternalError "Consumer: matched cohort not in cohorts list"
              now <- getCurrentTime
              uid <- generateGUID
              let candidate =
                    DRU.RewardUnlock
                      { id = uid,
                        personId = riderId,
                        campaignId = campaign.id,
                        cohortId = cohortId,
                        merchantId = Just campaign.merchantId,
                        merchantOperatingCityId = Just campaign.merchantOperatingCityId,
                        unlockedAt = now,
                        couponCode = Nothing,
                        couponSource = campaign.couponSourceType,
                        couponValidTill =
                          case cohort.couponValidityDays of
                            Just days -> Just $ addUTCTime (fromIntegral (days * 86400)) now
                            Nothing -> campaign.endsAt,
                        status = DRU.Active,
                        unlockSeq = Nothing,
                        viewedAt = Nothing,
                        claimedAt = Nothing,
                        redeemedAt = Nothing,
                        reclaimedAt = Nothing,
                        createdAt = now,
                        updatedAt = now
                      }
              -- Gate on nextUnlockDecision (pure, no I/O) before claiming a
              -- coupon: a repeatable cohort that has already hit its cap would
              -- otherwise claim (and immediately have to return) a coupon on
              -- every matching evaluation. createNextUnlock still re-checks
              -- and inserts, as the race-safety backstop against the unique
              -- index.
              let mBlockStart = Block.unlockBlockStart campaign.startsAt cohort completedAt
              case QRUE.nextUnlockDecision cohort mBlockStart existing candidate of
                Nothing -> do
                  when (isRepeatableCohort cohort) $
                    logInfo $
                      "rewards.unlock.cap_reached riderId="
                        <> riderId.getId
                        <> " cohortId="
                        <> cohortId.getId
                        <> " maxUnlocksPerCohort="
                        <> show cohort.maxUnlocksPerCohort
                        <> " unlockResetPeriod="
                        <> show cohort.unlockResetPeriod
                        <> " blockStart="
                        <> show mBlockStart
                  pure sentSoFar
                Just _ -> do
                  mCode <- Coupon.claimCoupon campaign cohort riderId
                  case mCode of
                    Nothing -> do
                      logInfo $
                        "rewards.claim.empty riderId="
                          <> riderId.getId
                          <> " campaignId="
                          <> campaign.id.getId
                          <> " cohortId="
                          <> cohortId.getId
                          <> " couponSource="
                          <> show campaign.couponSourceType
                      pure sentSoFar
                    Just code -> do
                      let unlock = candidate {DRU.couponCode = Just code}
                      inserted <- QRUE.createNextUnlock cohort mBlockStart existing unlock
                      if inserted
                        then do
                          Coupon.finalizeClaim campaign cohort code
                          if sentSoFar < maxPushesPerMessage
                            then do
                              sendUnlockPush riderId campaign cohort code
                              pure (sentSoFar + 1)
                            else pure sentSoFar
                        else do
                          logInfo $
                            "rewards.unlock.skipped.duplicate riderId="
                              <> riderId.getId
                              <> " campaignId="
                              <> campaign.id.getId
                              <> " cohortId="
                              <> cohortId.getId
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
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, ServiceFlow m r, EncFlow m r, HasFlowEnv m r '["maxNotificationShards" ::: Int]) =>
  Id DP.Person ->
  DRCmp.RewardCampaign ->
  DRC.RewardCohort ->
  Text ->
  m ()
sendUnlockPush rid campaign cohort code = do
  fork "rewards unlock notify" $ do
    result <- try @_ @SomeException $ do
      person <- QPerson.findById rid >>= fromMaybeM (PersonDoesNotExist rid.getId)
      Notify.notifyRewardUnlock person cohort.rewardTitle campaign.sponsorName code
    case result of
      Right _ ->
        logInfo $
          "rewards.unlock.push.sent riderId=" <> rid.getId <> " campaign=" <> campaign.name <> " cohort=" <> cohort.rewardTitle
      Left e ->
        logError $
          "rewards.unlock.push.failed riderId=" <> rid.getId <> " campaign=" <> campaign.name <> " cohort=" <> cohort.rewardTitle <> " err=" <> show e
