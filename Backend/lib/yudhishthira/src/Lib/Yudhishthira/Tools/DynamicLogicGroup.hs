{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shared per-transaction "experiment group" machinery for app dynamic logic.
--
-- Every experiment rollout carries an @experimentGroup@ tag linking it to a
-- registered group ('AppDynamicLogicExperimentGroup'), which owns a
-- @rolloutPercentage@. Resolution uses two per-transaction tosses, each decided
-- exactly once (keyed by 'TxnIdKey', locked in Redis) and reused by every domain:
--
--   1. A /group toss/ partitions @1..100@ among the city's active groups and
--      selects at most one group (@A@ xor @B@ xor none) -- groups are mutually
--      exclusive; the remainder is 'NoGroupSelected'.
--   2. A /group-rollout toss/ (a second @1..100@, shared across every domain in
--      the selected group) gates each domain by its own rollout @percentageRollout@:
--      the domain serves its tagged experiment iff the toss is within its share,
--      otherwise that domain falls back to base. Sharing one toss keeps the group's
--      domains correlated (a lower-% domain's "on" set is a subset of a higher-%
--      one's) while still letting each domain carry its own percentage.
--
-- The no-group outcome, a domain not tagged with the selected group, and the
-- no-transaction case all fall back to @chooseFn@ over the ungrouped rollouts
-- (the base/control version).
module Lib.Yudhishthira.Tools.DynamicLogicGroup
  ( GroupSelection (..),
    chooseWithGroups,
  )
where

import Data.List (sortOn)
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.Prelude
import Kernel.Randomizer (getRandomInRange)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common (logDebug)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicExperimentGroup as CADLEG
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup (AppDynamicLogicExperimentGroup)
import Lib.Yudhishthira.Types.AppDynamicLogicRollout (AppDynamicLogicRollout)

-- | The per-transaction group decision: either exactly one experiment group was
-- selected, or the toss landed in the ungrouped remainder (no group).
data GroupSelection = GroupSelected Text | NoGroupSelected
  deriving (Generic, Show, Eq)

instance ToJSON GroupSelection

instance FromJSON GroupSelection

selectedGroupKey :: Text -> Text -> Text
selectedGroupKey txnId cityId = "dynamicLogicSelectedGroup:" <> txnId <> ":" <> cityId

-- | Key for the transaction's shared group-rollout toss (the second toss, used by
-- every domain in the selected group to gate against its own @percentageRollout@).
groupRolloutTossKey :: Text -> Text -> Text
groupRolloutTossKey txnId cityId = "dynamicLogicGroupRolloutToss:" <> txnId <> ":" <> cityId

selectedGroupTtlSec :: Int
selectedGroupTtlSec = 7200

-- | Group-aware rollout selection via two per-transaction tosses. First the group
-- toss selects at most one group ('getOrDecideSelectedGroup'); then, for a domain
-- tagged with the selected group, the shared group-rollout toss
-- ('getOrDecideGroupRolloutToss') gates it by its own @percentageRollout@ -- within
-- share serves the experiment, over share falls back to base. A domain without the
-- selected group's tag, the no-group outcome, and the no-transaction case all fall
-- back to @chooseFn@ over the ungrouped rollouts (the base/control version).
chooseWithGroups ::
  (BeamFlow m r) =>
  [AppDynamicLogicRollout] ->
  ([AppDynamicLogicRollout] -> m (Maybe AppDynamicLogicRollout)) ->
  m (Maybe AppDynamicLogicRollout)
chooseWithGroups applicable chooseFn = do
  mbTxnId <- L.getOptionLocal TxnIdKey
  case (mbTxnId, listToMaybe applicable) of
    (Just txnId, Just anyRollout)
      | any (isJust . (.experimentGroup)) applicable -> do
        let cityId = anyRollout.merchantOperatingCityId
        selection <- getOrDecideSelectedGroup txnId cityId
        case selectedRollout selection of
          Just r -> do
            -- Group won; the shared second toss now gates this domain by its own rollout %.
            rolloutToss <- getOrDecideGroupRolloutToss txnId cityId
            if rolloutToss <= r.percentageRollout
              then do
                logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId <> " selection=" <> show selection <> " rolloutToss=" <> show rolloutToss <> " <= " <> show r.percentageRollout <> " -> grouped version=" <> show r.version
                pure (Just r)
              else do
                logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId <> " selection=" <> show selection <> " rolloutToss=" <> show rolloutToss <> " > " <> show r.percentageRollout <> " -> fallback (base)"
                chooseFn (ungroupedRollouts applicable)
          Nothing -> do
            logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId <> " selection=" <> show selection <> " -> fallback (base)"
            chooseFn (ungroupedRollouts applicable)
    _ -> do
      logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " no txnId/grouped rollouts -> plain toss (grouping disabled)"
      chooseFn applicable
  where
    -- The applicable rollout tagged with the selected group, if any.
    selectedRollout (GroupSelected g) = find (\r -> r.experimentGroup == Just g) applicable
    selectedRollout NoGroupSelected = Nothing
    -- Drop every group-tagged rollout so a losing group's version is never served;
    -- what remains is the ungrouped base/control rollout(s).
    ungroupedRollouts = filter (isNothing . (.experimentGroup))

-- | Read the transaction's group selection from Redis, or decide it once: load
-- the city's active groups, toss once over their cumulative percentages, and
-- lock the result. Concurrent first-resolvers race on @setNx@; the stored value
-- wins and is read back so every domain agrees.
getOrDecideSelectedGroup ::
  (BeamFlow m r) =>
  Text ->
  Id LYT.MerchantOperatingCity ->
  m GroupSelection
getOrDecideSelectedGroup txnId cityId = do
  let key = selectedGroupKey txnId (getId cityId)
  mbCached <- Hedis.withCrossAppRedis $ Hedis.safeGet key
  case mbCached of
    Just sel -> do
      logDebug $ "DYNAMIC_LOGIC_GROUP: txnId=" <> txnId <> " city=" <> getId cityId <> " reuse selection=" <> show sel
      pure sel
    Nothing -> do
      groups <- activeGroupsForCity cityId
      tossed <- tossGroup groups
      void $ Hedis.withCrossAppRedis $ Hedis.setNxExpire key selectedGroupTtlSec tossed
      finalSel <- fromMaybe tossed <$> Hedis.withCrossAppRedis (Hedis.safeGet key)
      logDebug $
        "DYNAMIC_LOGIC_GROUP: txnId=" <> txnId <> " city=" <> getId cityId
          <> " activeGroups="
          <> show (map (\g -> (g.groupName, g.rolloutPercentage)) groups)
          <> " tossedSelection="
          <> show tossed
          <> " finalSelection="
          <> show finalSel
      pure finalSel

-- | Read the transaction's shared group-rollout toss, or decide it once: a single
-- @1..100@ draw locked in Redis and reused by every domain in the selected group,
-- so the per-domain rollout gates stay correlated within the transaction. Concurrent
-- first-resolvers race on @setNx@; the stored value wins and is read back.
getOrDecideGroupRolloutToss ::
  (BeamFlow m r) =>
  Text ->
  Id LYT.MerchantOperatingCity ->
  m Int
getOrDecideGroupRolloutToss txnId cityId = do
  let key = groupRolloutTossKey txnId (getId cityId)
  mbCached <- Hedis.withCrossAppRedis $ Hedis.safeGet key
  case mbCached of
    Just t -> pure t
    Nothing -> do
      toss <- getRandomInRange (1, 100 :: Int)
      void $ Hedis.withCrossAppRedis $ Hedis.setNxExpire key selectedGroupTtlSec toss
      fromMaybe toss <$> Hedis.withCrossAppRedis (Hedis.safeGet key)

-- | Active, selectable groups for a city (cached: in-mem + Redis). Filtering
-- (not discarded/reverted, always-on, positive percentage) lives in the cached
-- query so the whole group-centric gate shares one cache.
activeGroupsForCity ::
  (BeamFlow m r) =>
  Id LYT.MerchantOperatingCity ->
  m [AppDynamicLogicExperimentGroup]
activeGroupsForCity = CADLEG.getActiveGroupsCached

-- | One toss over @1..100@ partitioned among the groups (ordered by name for a
-- deterministic partition). A toss above the groups' cumulative share lands in
-- the ungrouped remainder -> 'NoGroupSelected'.
tossGroup ::
  (BeamFlow m r) =>
  [AppDynamicLogicExperimentGroup] ->
  m GroupSelection
tossGroup [] = pure NoGroupSelected
tossGroup groups = do
  toss <- getRandomInRange (1, 100 :: Int)
  let ordered = sortOn (.groupName) groups
      cumulative = scanl1 (+) (map (.rolloutPercentage) ordered)
      zipped = zip ordered cumulative
  pure $ case find (\(_, cumPct) -> toss <= cumPct) zipped of
    Just (g, _) -> GroupSelected g.groupName
    Nothing -> NoGroupSelected

-- | Log-friendly label for the domain being resolved (all applicable rollouts
-- share one domain). Only used for debug logging.
domainLabelOf :: [AppDynamicLogicRollout] -> Text
domainLabelOf = maybe "unknown" (show . (.domain)) . listToMaybe
