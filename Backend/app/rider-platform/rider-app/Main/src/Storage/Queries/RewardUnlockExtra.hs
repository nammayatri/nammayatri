{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardUnlockExtra
  ( findByCampaignCohortAndCode,
    createNextUnlock,
    nextUnlockDecision,
    hasUnlockCapacity,
    updateViewAndClaimTimestamps,
    markRedeemed,
    markActiveAsExpiredIfValidityPassed,
    findAllByCampaign,
  )
where

import Control.Exception (SomeException)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import qualified Domain.Types.RewardUnlock as DRU
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RewardUnlock as Beam
import Storage.Queries.OrphanInstances.RewardUnlock ()

findByCampaignCohortAndCode ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  Text ->
  m (Maybe DRU.RewardUnlock)
findByCampaignCohortAndCode campaignId cohortId code =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.campaignId (Se.Eq campaignId.getId),
          Se.Is Beam.cohortId (Se.Eq cohortId.getId),
          Se.Is Beam.couponCode (Se.Eq (Just code)),
          Se.Is Beam.status (Se.Not (Se.Eq DRU.Reclaimed))
        ]
    ]

-- | Decide whether a candidate unlock for this cohort should be created, and
-- if so, at what 'unlockSeq'. Exported (and kept pure) so the cap/seq logic is
-- directly unit testable without a DB, mirroring
-- 'Domain.Action.Rewards.Evaluator.evaluateCohortsPure'.
--
-- 'existing' is the caller's already-fetched unlocks for this rider+campaign
-- (all cohorts); this function filters down to the candidate's cohort.
nextUnlockDecision ::
  DRC.RewardCohort ->
  Maybe UTCTime ->
  [DRU.RewardUnlock] ->
  DRU.RewardUnlock ->
  Maybe Int
nextUnlockDecision cohort mBlockStart existing unlock
  | not (hasUnlockCapacity cohort.maxUnlocksPerCohort mBlockStart unlock.cohortId existing) = Nothing
  | isNothing cohort.maxUnlocksPerCohort && isNothing mBlockStart = Just 1
  | otherwise = Just (maximum (0 : map (fromMaybe 1 . (.unlockSeq)) forCohort) + 1)
  where
    -- Every row for the cohort, not just the current block's, so a seq is never reused.
    forCohort = filter (\u -> u.cohortId == unlock.cohortId) existing

-- | Whether the rider can still unlock this cohort: below its repeat cap
-- ('Just n' live unlocks), or with no live unlock at all for a one-shot cohort
-- ('Nothing'). Live = any status but 'Reclaimed'. With a block start (cohorts
-- whose cap resets daily/weekly, see 'Domain.Action.Rewards.CampaignBlock'),
-- only unlocks from that block on count, so the cap applies per block. The
-- single source of the cap rule, shared by 'nextUnlockDecision' and the
-- home-screen live reward card so the card can't outlive (or die before) the
-- reward it advertises.
hasUnlockCapacity ::
  Maybe Int ->
  Maybe UTCTime ->
  Id DRC.RewardCohort ->
  [DRU.RewardUnlock] ->
  Bool
hasUnlockCapacity maxUnlocks mBlockStart cohortId existing =
  length liveForCohort < fromMaybe 1 maxUnlocks
  where
    liveForCohort =
      filter
        ( \u ->
            u.cohortId == cohortId
              && u.status /= DRU.Reclaimed
              && maybe True (u.unlockedAt >=) mBlockStart
        )
        existing

-- | Create the next unlock row for a rider+cohort, honoring the cohort's
-- repeat cap ('maxUnlocksPerCohort'). Non-repeatable cohorts ('Nothing') keep
-- today's one-shot sticky behavior: at most one non-'Reclaimed' row ever, at
-- 'unlockSeq' 1. Repeatable cohorts ('Just n') allow up to 'n' non-'Reclaimed'
-- rows, each with a fresh, never-reused 'unlockSeq'. Cohorts with a reset
-- period ('mBlockStart' set) apply that cap (1 when 'Nothing') per block, also
-- with fresh seqs, so a rider can unlock again in each new block.
--
-- 'existing' is the caller's already-fetched existing unlocks for this
-- rider+campaign (across all cohorts); 'unlock' is the candidate row, whose
-- 'unlockSeq' is overwritten with the computed value before insert.
--
-- The 'try'/'createWithKV' pattern is the race-safety backstop against the
-- partial unique index on (personId, campaignId, cohortId, unlockSeq).
createNextUnlock ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DRC.RewardCohort ->
  Maybe UTCTime ->
  [DRU.RewardUnlock] ->
  DRU.RewardUnlock ->
  m Bool
createNextUnlock cohort mBlockStart existing unlock =
  case nextUnlockDecision cohort mBlockStart existing unlock of
    Nothing -> do
      whenJust cohort.maxUnlocksPerCohort $ \n ->
        logInfo $
          "rewards.unlock.cap_reached riderId="
            <> unlock.personId.getId
            <> " cohortId="
            <> unlock.cohortId.getId
            <> " maxUnlocksPerCohort="
            <> show n
      pure False
    Just seqNo -> do
      result <- try @_ @SomeException $ createWithKV unlock {DRU.unlockSeq = Just seqNo}
      case result of
        Right _ -> pure True
        Left _ -> pure False

updateViewAndClaimTimestamps ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRU.RewardUnlock ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m ()
updateViewAndClaimTimestamps unlockId mViewed mClaimed =
  when (isJust mViewed || isJust mClaimed) $ do
    now <- getCurrentTime
    updateOneWithKV
      ( catMaybes
          [ mViewed >>= \t -> Just (Se.Set Beam.viewedAt (Just t)),
            mClaimed >>= \t -> Just (Se.Set Beam.claimedAt (Just t)),
            Just (Se.Set Beam.updatedAt now)
          ]
      )
      [Se.Is Beam.id (Se.Eq unlockId.getId)]

markRedeemed ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRU.RewardUnlock ->
  UTCTime ->
  m ()
markRedeemed unlockId now =
  updateOneWithKV
    [ Se.Set Beam.status DRU.Redeemed,
      Se.Set Beam.redeemedAt (Just now),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id (Se.Eq unlockId.getId)]

markActiveAsExpiredIfValidityPassed ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  UTCTime ->
  m ()
markActiveAsExpiredIfValidityPassed personId now =
  void $
    updateWithKV
      [ Se.Set Beam.status DRU.ExpiredUnredeemed,
        Se.Set Beam.updatedAt now
      ]
      [ Se.Is Beam.personId (Se.Eq personId.getId),
        Se.Is Beam.status (Se.Eq DRU.Active),
        Se.Is Beam.couponValidTill (Se.LessThan (Just now))
      ]

findAllByCampaign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRCmp.RewardCampaign ->
  m [DRU.RewardUnlock]
findAllByCampaign campaignId =
  findAllWithKV [Se.Is Beam.campaignId (Se.Eq campaignId.getId)]
