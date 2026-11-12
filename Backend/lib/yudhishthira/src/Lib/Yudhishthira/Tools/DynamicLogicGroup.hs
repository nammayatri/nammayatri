{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shared per-transaction "experiment group" machinery for app dynamic logic.
--
-- A rollout may carry an @experimentGroup@ tag that links it to a registered
-- experiment group ('AppDynamicLogicExperimentGroup'), which owns a
-- @rolloutPercentage@. Within one transaction (the same 'TxnIdKey'), a single
-- group toss partitions @1..100@ among the city's active groups and selects at
-- most one group (@A@ xor @B@ xor none). That selection is decided exactly once
-- and reused by every domain: a domain only offers the rollout tagged with the
-- selected group (plus its untagged/base rollouts), so grouped rollouts across
-- domains move together. Once a group is selected, each domain still runs its
-- own cumulative-percentage toss over @[selectedGroupRollout, base]@, so the
-- per-domain @percentageRollout@ governs experiment-vs-base within the group.
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
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicExperimentGroup as QADLEG
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

selectedGroupTtlSec :: Int
selectedGroupTtlSec = 7200

-- | v1 scope: group selection considers only always-on ("Unbounded") groups.
-- Named-window (time-bounded) groups are not yet resolved here because window
-- schedules are keyed per domain, whereas a group spans domains.
unboundedTimeBound :: Text
unboundedTimeBound = "Unbounded"

-- | Group-aware rollout selection. @chooseFn@ is the engine's own selection over
-- a candidate list (its cumulative-percentage toss); it receives a shared toss
-- value to use when one is in scope (@Just@), or draws its own when @Nothing@. If
-- a transaction id is in scope ('TxnIdKey') and at least one applicable rollout
-- is grouped, a single group is selected once per transaction (locked in Redis)
-- and every domain then offers @chooseFn@ only the rollouts tagged with the
-- selected group plus its untagged/base rollouts. To keep grouped domains
-- correlated (a user who lands in one domain's experiment lands in the others'
-- too), the within-group experiment-vs-base toss is a single per-transaction
-- value shared across every grouped domain. Ungrouped rollouts, and the
-- no-transaction case, behave exactly like @chooseFn@ alone (own toss).
chooseWithGroups ::
  (BeamFlow m r) =>
  [AppDynamicLogicRollout] ->
  (Maybe Int -> [AppDynamicLogicRollout] -> m (Maybe AppDynamicLogicRollout)) ->
  m (Maybe AppDynamicLogicRollout)
chooseWithGroups applicable chooseFn = do
  mbTxnId <- L.getOptionLocal TxnIdKey
  case (mbTxnId, listToMaybe applicable) of
    (Just txnId, Just anyRollout)
      | any (isJust . (.experimentGroup)) applicable -> do
        let cityId = anyRollout.merchantOperatingCityId
        selection <- getOrDecideSelectedGroup txnId cityId
        sharedToss <- getOrCreateTxnToss txnId
        let candidates = filter (isCandidate selection) applicable
        logDebug $
          "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId
            <> " selection="
            <> show selection
            <> " sharedToss="
            <> show sharedToss
            <> " candidateVersions="
            <> show (map (.version) candidates)
        chooseFn (Just sharedToss) candidates
    _ -> do
      logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " no txnId/grouped rollouts -> plain toss (grouping disabled)"
      chooseFn Nothing applicable
  where
    -- An untagged rollout (including base, which is ungrouped upstream) is always
    -- a candidate; a grouped rollout is a candidate only if its group was selected.
    isCandidate selection r = case r.experimentGroup of
      Nothing -> True
      Just g -> selection == GroupSelected g

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

txnWithinTossKey :: Text -> Text
txnWithinTossKey txnId = "dynamicLogicWithinToss:" <> txnId

-- | The single within-group experiment-vs-base toss, shared across every grouped
-- domain in the transaction so their experiment memberships are correlated
-- (nested by percentage). Drawn once and locked; concurrent first-resolvers race
-- on @setNx@ and the stored value wins.
getOrCreateTxnToss ::
  (BeamFlow m r) =>
  Text ->
  m Int
getOrCreateTxnToss txnId = do
  let key = txnWithinTossKey txnId
  mbToss <- Hedis.withCrossAppRedis $ Hedis.safeGet key
  case mbToss of
    Just t -> pure t
    Nothing -> do
      t <- getRandomInRange (1, 100 :: Int)
      void $ Hedis.withCrossAppRedis $ Hedis.setNxExpire key selectedGroupTtlSec t
      fromMaybe t <$> Hedis.withCrossAppRedis (Hedis.safeGet key)

-- | Active, selectable groups for a city: not discarded/reverted, always-on, and
-- with a positive rollout percentage.
activeGroupsForCity ::
  (BeamFlow m r) =>
  Id LYT.MerchantOperatingCity ->
  m [AppDynamicLogicExperimentGroup]
activeGroupsForCity cityId = do
  allGroups <- QADLEG.findAllByMerchantOpCityId cityId
  pure $ filter isActive allGroups
  where
    isActive g =
      g.experimentStatus /= Just LYT.DISCARDED
        && g.experimentStatus /= Just LYT.REVERTED
        && g.timeBounds == unboundedTimeBound
        && g.rolloutPercentage > 0

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
