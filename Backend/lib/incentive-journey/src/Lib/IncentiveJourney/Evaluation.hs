{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Milestone.evaluation: condition JsonLogic only ({ "logic": … }).
-- Slabs live in rewardMetadata (not evaluation). Stats.currentMetrics: bag + in/out + slabPercent.
module Lib.IncentiveJourney.Evaluation
  ( evaluationLogic,
    netOffPayout,
    netOffDelta,
    currentMetricsObject,
    mergeCurrentMetrics,
    maxMergeCurrentMetrics,
    bumpEarningsInCurrentMetrics,
    bumpMetricCountInCurrentMetrics,
    bumpMetricCountInCurrentMetricsWithRedisFloor,
    enrichCurrentMetricsWithDerivedRates,
    metricInsideKey,
    metricOutsideKey,
    lookupNumber,
    lookupMetricInt,
    lookupOnlineMinutes,
    setSlabPercent,
    earningInsideKey,
    earningOutsideKey,
    slabPercentKey,
    campaignBehaviorMetricBaseNames,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

earningInsideKey :: Text
earningInsideKey = "earningInsideTimebound"

earningOutsideKey :: Text
earningOutsideKey = "earningOutsideTimebound"

slabPercentKey :: Text
slabPercentKey = "slabPercent"

-- | Base names for the five campaign behaviour counters (in/out suffixes applied).
campaignBehaviorMetricBaseNames :: [Text]
campaignBehaviorMetricBaseNames =
  [ "onlineMinutes",
    "rrReceived",
    "rrAccepted",
    "dxCancel",
    "dafCancel"
  ]

metricInsideKey :: Text -> Text
metricInsideKey base = base <> "InsideTimebound"

metricOutsideKey :: Text -> Text
metricOutsideKey base = base <> "OutsideTimebound"

-- | JsonLogic gate from evaluation column (key "logic").
-- Also accepts a bare JsonLogic object (no wrapper) for forward compat.
evaluationLogic :: Maybe A.Value -> Maybe A.Value
evaluationLogic mbEval = do
  val <- mbEval
  case val of
    A.Object o ->
      case AKM.lookup (AKey.fromText "logic") o of
        Just logic -> Just logic
        Nothing -> Just val -- bare logic
    _ -> Just val

-- | max(0, (guaranteeAmount - earnings) * slabPercent / 100), rounded to Int rupees.
netOffPayout :: Int -> HighPrecMoney -> Int -> Int
netOffPayout guaranteeAmount earnings slabPercent =
  max 0 (netOffDelta guaranteeAmount earnings slabPercent)

-- | (guarantee - earnings) * pct / 100 — may be negative before max(0, …) in netOffPayout.
netOffDelta :: Int -> HighPrecMoney -> Int -> Int
netOffDelta guaranteeAmount earnings slabPercent =
  let earn = round earnings :: Int
      raw = (guaranteeAmount - earn) * slabPercent
   in raw `div` 100

currentMetricsObject :: Maybe A.Value -> AKM.KeyMap A.Value
currentMetricsObject = \case
  Just (A.Object o) -> o
  _ -> AKM.empty

-- | Derived fields must not be max-merged: counters can move while an old rate/slab
-- stays higher. Strip before merge; recompute rates after (callers refresh slabPercent).
derivedCurrentMetricsKeys :: [Text]
derivedCurrentMetricsKeys =
  [ "acceptanceRate",
    "cancellationRate",
    slabPercentKey
  ]

stripDerivedCurrentMetrics :: AKM.KeyMap A.Value -> AKM.KeyMap A.Value
stripDerivedCurrentMetrics o =
  foldl' (\acc k -> AKM.delete (AKey.fromText k) acc) o derivedCurrentMetricsKeys

-- | Progressive merge: never lower an existing numeric stats field with a smaller bag value
-- (protects DB currentMetrics if Redis was wiped and rebuilt at 0).
-- Recomputes acceptance/cancellation rates from the merged counts.
mergeCurrentMetrics :: Maybe A.Value -> HM.HashMap Text Int -> Maybe A.Value
mergeCurrentMetrics mbExisting bag =
  let base = stripDerivedCurrentMetrics (currentMetricsObject mbExisting)
      merged =
        foldl'
          ( \acc (k, v) ->
              -- Campaign behaviour bag keys are flat (rrAccepted, ...). Tick path writes
              -- the InsideTimebound suffix too — keep both in sync so rates never see Inside=4, flat=2.
              let targets =
                    if k `elem` campaignBehaviorMetricBaseNames
                      then [k, metricInsideKey k]
                      else [k]
               in foldl'
                    ( \acc' tk ->
                        let aKey = AKey.fromText tk
                            existingN = case AKM.lookup aKey acc' of
                              Just (A.Number n) -> round n
                              _ -> 0
                            kept = max existingN v
                         in AKM.insert aKey (A.Number (fromIntegral kept)) acc'
                    )
                    acc
                    targets
          )
          base
          (HM.toList bag)
   in enrichCurrentMetricsWithDerivedRates (Just (A.Object merged))

-- | Union two currentMetrics blobs; numeric keys keep the higher value (lost-update guard
-- when an upsert was computed from a slightly stale read under the stats row lock).
-- Rates are recomputed from merged counts; slabPercent is stripped (caller refreshes).
maxMergeCurrentMetrics :: Maybe A.Value -> Maybe A.Value -> Maybe A.Value
maxMergeCurrentMetrics mbA mbB =
  case (mbA, mbB) of
    (Nothing, Nothing) -> Nothing
    _ ->
      let oa = stripDerivedCurrentMetrics (currentMetricsObject mbA)
          ob = stripDerivedCurrentMetrics (currentMetricsObject mbB)
          keys = AKM.keys oa <> filter (`notElem` AKM.keys oa) (AKM.keys ob)
          merged =
            foldl'
              ( \acc k ->
                  case (AKM.lookup k oa, AKM.lookup k ob) of
                    (Just (A.Number na), Just (A.Number nb)) ->
                      AKM.insert k (A.Number (max na nb)) acc
                    (Just va, Just _) -> AKM.insert k va acc
                    (Just va, Nothing) -> AKM.insert k va acc
                    (Nothing, Just vb) -> AKM.insert k vb acc
                    (Nothing, Nothing) -> acc
              )
              AKM.empty
              keys
       in enrichCurrentMetricsWithDerivedRates (Just (A.Object merged))

bumpEarningsInCurrentMetrics :: Maybe A.Value -> Bool -> HighPrecMoney -> Maybe A.Value
bumpEarningsInCurrentMetrics mbExisting insideTimeBound =
  bumpNumericInCurrentMetrics mbExisting (if insideTimeBound then earningInsideKey else earningOutsideKey)

-- | Bump a campaign behaviour counter into Inside/OutsideTimebound keys, then refresh derived rates.
bumpMetricCountInCurrentMetrics :: Maybe A.Value -> Text -> Bool -> Int -> Maybe A.Value
bumpMetricCountInCurrentMetrics mbExisting baseName insideTimeBound delta =
  let key = if insideTimeBound then metricInsideKey baseName else metricOutsideKey baseName
   in enrichCurrentMetricsWithDerivedRates $
        bumpNumericInCurrentMetrics mbExisting key (fromIntegral delta)

-- | Inside + redisFloor>0: set InsideTimebound = max(existing, redisFloor) — Redis ij:bm is SoT.
-- Idempotent under Kafka redelivery / duplicate ticks (no delta add). Outside (or redisFloor=0
-- e.g. key miss): delta-only — ij:bm has no outside counter.
bumpMetricCountInCurrentMetricsWithRedisFloor :: Maybe A.Value -> Text -> Bool -> Int -> Int -> Maybe A.Value
bumpMetricCountInCurrentMetricsWithRedisFloor mbExisting baseName insideTimeBound delta redisFloor
  | insideTimeBound && redisFloor > 0 =
    ensureMetricInsideAtLeast mbExisting baseName redisFloor
  | otherwise =
    bumpMetricCountInCurrentMetrics mbExisting baseName insideTimeBound delta

-- | Raise InsideTimebound to at least redisFloor (Redis ij:bm SoT), keep flat base in sync,
-- then refresh rates. Flat keys from ride-end merges must not lag Inside (acceptanceRate 200%).
ensureMetricInsideAtLeast :: Maybe A.Value -> Text -> Int -> Maybe A.Value
ensureMetricInsideAtLeast mbMetrics baseName redisFloor =
  let insideKey = metricInsideKey baseName
      flatKey = AKey.fromText baseName
      curInside = maybe 0 round (lookupNumber mbMetrics insideKey)
      curFlat = maybe 0 round (lookupNumber mbMetrics baseName)
      alreadyOk = redisFloor <= curInside && redisFloor <= curFlat
   in if alreadyOk
        then enrichCurrentMetricsWithDerivedRates mbMetrics
        else
          let o = currentMetricsObject mbMetrics
              updated =
                AKM.insert flatKey (A.Number (fromIntegral redisFloor)) $
                  AKM.insert (AKey.fromText insideKey) (A.Number (fromIntegral redisFloor)) o
           in enrichCurrentMetricsWithDerivedRates (Just (A.Object updated))

-- | Persist period-scoped rates next to counts in currentMetrics (for UI / settle).
-- acceptanceRate = rrAccepted*100/rrReceived; cancellationRate = (dx+daf)*100/rrAccepted.
-- Cap at 100 — counts must never imply >100% (guards flat/Inside skew).
enrichCurrentMetricsWithDerivedRates :: Maybe A.Value -> Maybe A.Value
enrichCurrentMetricsWithDerivedRates mbMetrics =
  let rrRecv = lookupMetricInt mbMetrics mempty "rrReceived"
      rrAcc = lookupMetricInt mbMetrics mempty "rrAccepted"
      cancels =
        lookupMetricInt mbMetrics mempty "dxCancel"
          + lookupMetricInt mbMetrics mempty "dafCancel"
      acceptRate =
        if rrRecv <= 0
          then 0
          else min 100 ((rrAcc * 100) `div` rrRecv)
      cancelRate =
        if rrAcc <= 0
          then if cancels <= 0 then 0 else 100
          else min 100 ((cancels * 100) `div` rrAcc)
      o = currentMetricsObject mbMetrics
      updated =
        AKM.insert (AKey.fromText "acceptanceRate") (A.Number (fromIntegral acceptRate)) $
          AKM.insert (AKey.fromText "cancellationRate") (A.Number (fromIntegral cancelRate)) o
   in Just (A.Object updated)

bumpNumericInCurrentMetrics :: Maybe A.Value -> Text -> HighPrecMoney -> Maybe A.Value
bumpNumericInCurrentMetrics mbExisting key delta =
  let o = currentMetricsObject mbExisting
      aKey = AKey.fromText key
      existing = case AKM.lookup aKey o of
        Just (A.Number n) -> realToFrac n
        _ -> 0
      updated = AKM.insert aKey (A.Number (realToFrac (existing + delta))) o
   in Just (A.Object updated)

lookupNumber :: Maybe A.Value -> Text -> Maybe HighPrecMoney
lookupNumber mbMetrics key = do
  A.Object o <- mbMetrics
  case AKM.lookup (AKey.fromText key) o of
    Just (A.Number n) -> Just (realToFrac n)
    _ -> Nothing

-- | Resolve slab metric from currentMetrics or bag (Int-ish).
-- Tick path writes *InsideTimebound / *OutsideTimebound. Prefer those over a flat key —
-- ride-end merges can leave flat "dafCancel": 0 that must not mask InsideTimebound: 1.
lookupMetricInt :: Maybe A.Value -> HM.HashMap Text Int -> Text -> Int
lookupMetricInt mbMetrics bag metricName =
  let mbInside = lookupNumber mbMetrics (metricInsideKey metricName)
      mbOutside = lookupNumber mbMetrics (metricOutsideKey metricName)
   in case (mbInside, mbOutside) of
        (Nothing, Nothing) ->
          case lookupNumber mbMetrics metricName of
            Just n -> round n
            Nothing ->
              HM.lookupDefault 0 metricName bag
                `max` HM.lookupDefault 0 (metricInsideKey metricName) bag
        _ ->
          maybe 0 round mbInside + maybe 0 round mbOutside

-- | Online minutes for MG slabs (flat "onlineMinutes", same as other metrics).
lookupOnlineMinutes :: Maybe A.Value -> HM.HashMap Text Int -> Int
lookupOnlineMinutes mbMetrics bag = lookupMetricInt mbMetrics bag "onlineMinutes"

setSlabPercent :: Maybe A.Value -> Int -> Maybe A.Value
setSlabPercent mbExisting percent =
  let o = currentMetricsObject mbExisting
      updated = AKM.insert (AKey.fromText slabPercentKey) (A.Number (fromIntegral percent)) o
   in Just (A.Object updated)
