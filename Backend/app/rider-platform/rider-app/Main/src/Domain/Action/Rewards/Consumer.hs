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
  )
where

import qualified Domain.Action.Rewards.Coupon as Coupon
import qualified Domain.Action.Rewards.Evaluator as Eval
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import qualified Domain.Types.RewardUnlock as DRU
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
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

-- | Gated entry point: evaluate rewards for a rider iff the city has rewards
-- management enabled. Mirrors the old producer's gate so it is a drop-in for both
-- the ride-completion and triggerEval call sites. Callers should run it in a fork.
evaluateRewardsIfEnabled ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Hedis.HedisFlow m r,
    ServiceFlow m r,
    EncFlow m r
  ) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  Maybe Bool ->
  m ()
evaluateRewardsIfEnabled riderId moCityId completedAt mbIsValidRide = do
  enabled <- maybe False (.enableRewardsManagement) <$> getConfig (RiderDimensions {merchantOperatingCityId = moCityId.getId})
  when enabled $ evaluateRewardsForRider riderId moCityId completedAt mbIsValidRide

evaluateRewardsForRider ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Hedis.HedisFlow m r, ServiceFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  Maybe Bool ->
  m ()
evaluateRewardsForRider riderId moCityId completedAt mbIsValidRide = do
  context <- Ctx.readRiderContext riderId mbIsValidRide
  activeCampaigns <- QRCmpE.findAllActiveInCityAtTime moCityId completedAt
  foldM_
    ( \pushesSoFar campaign -> do
        cohorts <- QRC.findAllByCampaign campaign.id
        existing <- QRU.findByPersonAndCampaign riderId campaign.id
        let existingNonReclaimed = [u.cohortId | u <- existing, u.status /= DRU.Reclaimed]
        matched <- Eval.evaluateCohorts context cohorts
        let isRepeatableCohortId cid = maybe False (isJust . (.maxUnlocksPerCohort)) (find (\c -> c.id == cid) cohorts)
        let toUnlock = filter (\cid -> cid `notElem` existingNonReclaimed || isRepeatableCohortId cid) matched
        when (not (null cohorts)) $
          logInfo $
            "rewards.eval.campaign riderId="
              <> riderId.getId
              <> " campaignId="
              <> campaign.id.getId
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
              case QRUE.nextUnlockDecision cohort existing candidate of
                Nothing -> do
                  whenJust cohort.maxUnlocksPerCohort $ \n ->
                    logInfo $
                      "rewards.unlock.cap_reached riderId="
                        <> riderId.getId
                        <> " cohortId="
                        <> cohortId.getId
                        <> " maxUnlocksPerCohort="
                        <> show n
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
                      inserted <- QRUE.createNextUnlock cohort existing unlock
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
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, ServiceFlow m r, EncFlow m r) =>
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
