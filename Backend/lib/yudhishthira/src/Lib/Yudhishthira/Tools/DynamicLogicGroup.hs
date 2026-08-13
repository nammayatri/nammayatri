{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shared per-transaction "experiment group" machinery for app dynamic logic.
--
-- A rollout may carry an @experimentGroup@ tag. Within one transaction (the same
-- 'TxnIdKey'), a group's fate is decided exactly once (IN or OUT) and then reused
-- by every domain that references it, so grouped rollouts across domains move
-- together.
module Lib.Yudhishthira.Tools.DynamicLogicGroup
  ( TxnIdKey (..),
    GroupDecision (..),
    chooseWithGroups,
  )
where

import Data.List (nub)
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (logDebug)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Yudhishthira.Types.AppDynamicLogicRollout (AppDynamicLogicRollout)

data TxnIdKey = TxnIdKey
  deriving (Generic, Typeable, Show, Eq)

instance ToJSON TxnIdKey

instance FromJSON TxnIdKey

instance OptionEntity TxnIdKey Text

-- | Whether a transaction is IN or OUT of a given experiment group.
data GroupDecision = GroupIn | GroupOut
  deriving (Generic, Show, Eq)

instance ToJSON GroupDecision

instance FromJSON GroupDecision

groupLockKey :: Text -> Text -> Text
groupLockKey txnId group = "dynamicLogicGroupLock:" <> txnId <> ":" <> group

groupLockTtlSec :: Int
groupLockTtlSec = 7200

readGroupDecision :: (BeamFlow m r) => Text -> Text -> m (Maybe GroupDecision)
readGroupDecision txnId group = Hedis.withCrossAppRedis $ Hedis.safeGet (groupLockKey txnId group)

tryLockGroup :: (BeamFlow m r) => Text -> Text -> GroupDecision -> m GroupDecision
tryLockGroup txnId group decision = do
  won <- Hedis.withCrossAppRedis $ Hedis.setNxExpire (groupLockKey txnId group) groupLockTtlSec decision
  if won
    then pure decision
    else fromMaybe decision <$> readGroupDecision txnId group

-- | Group-aware rollout selection. @chooseFn@ is the engine's own selection over a
-- candidate list (its cumulative-percentage toss). If a transaction id is in
-- scope ('TxnIdKey'), an experiment group's membership is decided once for the
-- whole transaction: the first domain that references a group tosses over the
-- applicable rollouts and locks each present group IN (if the toss selected it)
-- or OUT (otherwise); every later domain then reuses that decision -- picking an
-- already-IN group's rollout without a toss, and excluding OUT groups before
-- tossing over the rest. Ungrouped rollouts, and the no-transaction case, behave
-- exactly like @chooseFn@ alone.
chooseWithGroups ::
  (BeamFlow m r) =>
  [AppDynamicLogicRollout] ->
  ([AppDynamicLogicRollout] -> m (Maybe AppDynamicLogicRollout)) ->
  m (Maybe AppDynamicLogicRollout)
chooseWithGroups applicable chooseFn = do
  mbTxnId <- L.getOptionLocal TxnIdKey
  case mbTxnId of
    Nothing -> do
      logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " no txnId in scope -> plain toss (grouping disabled)"
      chooseFn applicable
    Just txnId -> resolveWithGroups txnId applicable chooseFn 0

-- | Log-friendly label for the domain being resolved (all applicable rollouts
-- share one domain). Only used for debug logging.
domainLabelOf :: [AppDynamicLogicRollout] -> Text
domainLabelOf = maybe "unknown" (show . (.domain)) . listToMaybe

resolveWithGroups ::
  (BeamFlow m r) =>
  Text ->
  [AppDynamicLogicRollout] ->
  ([AppDynamicLogicRollout] -> m (Maybe AppDynamicLogicRollout)) ->
  Int ->
  m (Maybe AppDynamicLogicRollout)
resolveWithGroups txnId applicable chooseFn attempt = do
  let presentGroups = nub $ mapMaybe (.experimentGroup) applicable
  decisions <- forM presentGroups $ \g -> (g,) <$> readGroupDecision txnId g
  let decidedIn = [g | (g, Just GroupIn) <- decisions]
      decidedOut = [g | (g, Just GroupOut) <- decisions]
  logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId <> " attempt=" <> show attempt <> " presentGroups=" <> show presentGroups <> " decidedIn=" <> show decidedIn <> " decidedOut=" <> show decidedOut
  case find (\r -> maybe False (`elem` decidedIn) r.experimentGroup) applicable of
    Just r -> do
      logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId <> " already-IN group=" <> show r.experimentGroup <> " -> reuse version=" <> show r.version <> " (no toss)"
      pure (Just r)
    Nothing -> do
      -- Toss over remaining rollouts (other than those decided out earlier)
      let candidates = filter (\r -> maybe True (`notElem` decidedOut) r.experimentGroup) applicable
      mbWinner <- chooseFn candidates
      let winnerGroup = mbWinner >>= (.experimentGroup)
          undecidedGroups = filter (\g -> g `notElem` decidedIn && g `notElem` decidedOut) (nub $ mapMaybe (.experimentGroup) candidates)
      -- lock every undecided group (winner's group IN, rest OUT)
      authoritative <- forM undecidedGroups $ \g -> do
        let desired = if Just g == winnerGroup then GroupIn else GroupOut
        auth <- tryLockGroup txnId g desired
        logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId <> " locked group=" <> g <> " desired=" <> show desired <> " authoritative=" <> show auth
        pure (g, auth)
      -- Race Condition Handling (1 try, 2nd time just take winner)
      let raced = any (\(g, auth) -> (Just g == winnerGroup) /= (auth == GroupIn)) authoritative
      logDebug $ "DYNAMIC_LOGIC_GROUP: domain=" <> domainLabelOf applicable <> " txnId=" <> txnId <> " tossedWinnerVersion=" <> show (fmap (.version) mbWinner) <> " winnerGroup=" <> show winnerGroup <> " raced=" <> show raced
      if raced && attempt < 2
        then resolveWithGroups txnId applicable chooseFn (attempt + 1)
        else pure mbWinner
