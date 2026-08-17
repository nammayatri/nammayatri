# QAR-driven Tip Module Config Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

> **Superseded (2026-08-18):** the rider-side `TIP_MODULE_CONFIG` domain described below was replaced by an output field on the BPP `DYNAMIC_PRICING_UNIFIED` domain shipped over `on_search`; see the spec's "History" note. Tasks 1, 3 and 5 no longer apply; the API contract, RiderConfig fallback, `select2` guard and integration suite carry over.

**Goal:** Return a per-estimate `tipModuleConfig {showAfterSec, repeatIntervalSec, maxPrompts}` (plus raw `qar`) from the rider search-results API, computed by a new rider-side JSON-logic domain from the persisted `estimate.qar`, with a per-city `RiderConfig` default; and add the missing estimate-expiry guard to `/select2`.

**Architecture:** A new `LogicDomain` constructor `TIP_MODULE_CONFIG` in `lib/yudhishthira`; a new rider-app module `SharedLogic/TipModuleConfig.hs` modelled on `SharedLogic/PickupETA.hs` (input record → `getAppDynamicLogic` with a deterministic per-search toss → `runLogicsWithDebugLog` → decode `TipModuleConfig`, fail-open); the result (or `RiderConfig.tipModuleConfig`) is attached to `EstimateAPIEntity` inside `getEstimates` (`Domain/Action/UI/Quote.hs`). Dashboard verify/schema arms are registered so ops can author rules. No BPP or Beckn change.

**Tech Stack:** Haskell (GHC 9.2.7, `-Werror`), NammaDSL YAML → generated storage code, `json-logic-hs`, `lib/yudhishthira` rule engine, tasty/tasty-hunit, PostgreSQL migrations, Postman/Newman integration collections.

**Spec:** `docs/superpowers/specs/2026-08-16-tip-module-config-design.md`

## Global Constraints

- All `cabal`, `, run-generator`, `, hpack` commands run from `Backend/` **inside the nix dev shell** (repo root has `.envrc -> .envrc.backend`; `direnv allow` once, or `nix develop`). Plain `/usr/bin/cabal` cannot resolve `namma-dsl` and will fail.
- Project compiles with `-Werror` on library components: no unused imports/bindings, no incomplete patterns.
- Never edit `src-read-only/`; edit `spec/Storage/*.yaml` and regenerate with `, run-generator`.
- Type name and field names are fixed by the spec: `TipModuleConfig { showAfterSec :: Int, repeatIntervalSec :: Int, maxPrompts :: Int }`; domain string form `"TIP-MODULE-CONFIG"`.
- New API fields on `EstimateAPIEntity`: `tipModuleConfig :: Maybe TipModuleConfig`, `qar :: Maybe Double`.
- Commit messages follow `<sub-project>/<type>: <summary>`; author is Shailesh Gahlawat; **no `Co-Authored-By` trailer**.
- Working branch: `rider-app/feat/tip-module-config` (already created; spec is committed there).
- Do not start the local stack yourself; integration tasks assume the user has `, run-mobility-stack-dev` running (probe ports / ask).

---

### Task 1: `TIP_MODULE_CONFIG` domain in yudhishthira

**Files:**
- Modify: `Backend/lib/yudhishthira/src/Lib/Yudhishthira/Types.hs:305-320` (constructor), `:338-350` (`Enumerable`), `:380-392` (`generateLogicDomainShowInstances`), `:428-436` (`Show`), `:463-480` (`Read`)
- Create: `Backend/lib/yudhishthira/test/src/LogicDomainRoundTrip.hs`
- Modify: `Backend/lib/yudhishthira/test/app/Main.hs`

**Interfaces:**
- Produces: `Lib.Yudhishthira.Types.LogicDomain` constructor `TIP_MODULE_CONFIG`; `show TIP_MODULE_CONFIG == "TIP-MODULE-CONFIG"`; `readMaybe "TIP-MODULE-CONFIG" == Just TIP_MODULE_CONFIG`.

- [ ] **Step 1: Write the failing test**

Create `Backend/lib/yudhishthira/test/src/LogicDomainRoundTrip.hs`:

```haskell
module LogicDomainRoundTrip (logicDomainRoundTripTests) where

import Kernel.Prelude
import Lib.Yudhishthira.Types (LogicDomain (..), allValues)
import Test.Tasty
import Test.Tasty.HUnit

logicDomainRoundTripTests :: TestTree
logicDomainRoundTripTests =
  testGroup
    "LogicDomain TIP_MODULE_CONFIG"
    [ testCase "show uses hyphenated DB form" $
        show TIP_MODULE_CONFIG @?= "TIP-MODULE-CONFIG",
      testCase "read of the DB form yields the constructor" $
        readMaybe "TIP-MODULE-CONFIG" @?= Just TIP_MODULE_CONFIG,
      testCase "domain is enumerable (listed for dashboards)" $
        assertBool "TIP_MODULE_CONFIG missing from allValues" (TIP_MODULE_CONFIG `elem` allValues)
    ]
```

Register it in `Backend/lib/yudhishthira/test/app/Main.hs`:

```haskell
module Main (main) where

import KaalChakraJobs (kaalChakraJobsTests)
import Kernel.Prelude
import LogicDomainRoundTrip (logicDomainRoundTripTests)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs =
  return $ testGroup "Tests" [unitTests]
  where
    unitTests =
      testGroup
        "Unit tests"
        [ kaalChakraJobsTests,
          logicDomainRoundTripTests
        ]
```

- [ ] **Step 2: Run test to verify it fails**

Run (from `Backend/`, nix shell): `cabal build yudhishthira-tests 2>&1 | tail -20`
Expected: compile error `Data constructor not in scope: TIP_MODULE_CONFIG`.

- [ ] **Step 3: Add the constructor and instances**

In `Backend/lib/yudhishthira/src/Lib/Yudhishthira/Types.hs`:

1. Constructor list (after `| PICKUP_ETA_CALCULATION`):
```haskell
  | PICKUP_ETA_CALCULATION
  | TIP_MODULE_CONFIG
```
2. `instance Enumerable LogicDomain` `allValues` list (after `PICKUP_ETA_CALCULATION,`):
```haskell
      PICKUP_ETA_CALCULATION,
      TIP_MODULE_CONFIG,
```
3. `generateLogicDomainShowInstances` (after the `PICKUP_ETA_CALCULATION` line):
```haskell
    ++ [show PICKUP_ETA_CALCULATION]
    ++ [show TIP_MODULE_CONFIG]
```
4. `instance Show LogicDomain` (after the `PICKUP_ETA_CALCULATION` line):
```haskell
  show PICKUP_ETA_CALCULATION = "PICKUP-ETA-CALCULATION"
  show TIP_MODULE_CONFIG = "TIP-MODULE-CONFIG"
```
5. `instance Read LogicDomain` `readsPrec` case (after the `"PICKUP-ETA-CALCULATION"` arm):
```haskell
          "PICKUP-ETA-CALCULATION" ->
            [(PICKUP_ETA_CALCULATION, drop 1 rest)]
          "TIP-MODULE-CONFIG" ->
            [(TIP_MODULE_CONFIG, drop 1 rest)]
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal run yudhishthira-tests 2>&1 | tail -20`
Expected: `LogicDomain TIP_MODULE_CONFIG` group with 3 `OK`, overall `All N tests passed`.

- [ ] **Step 5: Confirm every consumer still compiles under -Werror**

Run: `cabal build all 2>&1 | tail -30`
Expected: success. (Only `rider-app/.../Dashboard/NammaTag.hs` matches on individual `LogicDomain` constructors and it uses a catch-all, so no incomplete-pattern errors are expected; if any appear, add an arm identical to its `PICKUP_ETA_CALCULATION` neighbour.)

- [ ] **Step 6: Commit**

```bash
git add Backend/lib/yudhishthira/src/Lib/Yudhishthira/Types.hs Backend/lib/yudhishthira/test/src/LogicDomainRoundTrip.hs Backend/lib/yudhishthira/test/app/Main.hs
git commit -m "yudhishthira/feat: add TIP_MODULE_CONFIG logic domain"
```

---

### Task 2: `TipModuleConfig` type and `RiderConfig.tipModuleConfig` column

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Types/Extra/RiderConfig.hs` (append type after `SyncSearchDispatchConfig`, ~line 126)
- Modify: `Backend/app/rider-platform/rider-app/Main/spec/Storage/RiderConfig.yaml` — `imports:` (line 1-20), `fields:` (~line 97-295), `beamType:` (~296), `fromTType:` (~367), `toTType:` (~430), `sqlType:` (~561)
- Generated (do not hand-edit): `src-read-only/Domain/Types/RiderConfig.hs`, `src-read-only/Storage/Beam/RiderConfig.hs`, `src-read-only/Storage/Queries/RiderConfig.hs`, `Backend/dev/migrations-read-only/rider-app/rider_config.sql`

**Interfaces:**
- Produces: `Domain.Types.Extra.RiderConfig.TipModuleConfig { showAfterSec :: Int, repeatIntervalSec :: Int, maxPrompts :: Int }` deriving `Show, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema`; `Domain.Types.RiderConfig.RiderConfig.tipModuleConfig :: Maybe TipModuleConfig`.

- [ ] **Step 1: Add the type**

Append to `Backend/app/rider-platform/rider-app/Main/src/Domain/Types/Extra/RiderConfig.hs` (after the `SyncSearchDispatchConfig` block that ends with `deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)`):

```haskell
-- | Cadence for the rider-app "add tip" module shown during an active search.
-- Produced per estimate by the TIP_MODULE_CONFIG JSON-logic domain from the
-- estimate's QAR (quote acceptance rate); this record on RiderConfig is the
-- per-city fallback when the rules yield nothing.
data TipModuleConfig = TipModuleConfig
  { showAfterSec :: Int, -- first prompt after N seconds of searching (from select2)
    repeatIntervalSec :: Int, -- re-prompt cadence; 0 = never repeat
    maxPrompts :: Int -- hard cap per search
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)
```

- [ ] **Step 2: Add the RiderConfig field in the YAML**

In `Backend/app/rider-platform/rider-app/Main/spec/Storage/RiderConfig.yaml`:

`imports:` — add one line next to `SyncSearchDispatchConfig: Domain.Types.Extra.RiderConfig`:
```yaml
  TipModuleConfig: Domain.Types.Extra.RiderConfig
```
`fields:` — add next to `busTrackingConfig: "Maybe BusTrackingConfig"`:
```yaml
    tipModuleConfig: "Maybe TipModuleConfig"
```
`beamType:` — add next to `busTrackingConfig: Maybe Value`:
```yaml
    tipModuleConfig: Maybe Value
```
`fromTType:` — add next to the `busTrackingConfig` line:
```yaml
    tipModuleConfig: tipModuleConfig >>= Kernel.Utils.JSON.valueToMaybe|E
```
`toTType:` — add next to the `busTrackingConfig` line:
```yaml
    tipModuleConfig: tipModuleConfig >>= Just . Data.Aeson.toJSON|E
```
`sqlType:` — add next to `busTrackingConfig: "json"`:
```yaml
    tipModuleConfig: "json"
```

- [ ] **Step 3: Regenerate and inspect**

Run (from `Backend/`, nix shell): `, run-generator 2>&1 | tail -20`
Then: `git status --short app/rider-platform/rider-app/Main/src-read-only dev/migrations-read-only/rider-app | head`
Expected: `src-read-only/Domain/Types/RiderConfig.hs`, `Storage/Beam/RiderConfig.hs`, `Storage/Queries/RiderConfig.hs` modified; `dev/migrations-read-only/rider-app/rider_config.sql` gains `ALTER TABLE atlas_app.rider_config ADD COLUMN tip_module_config json ;`.
Run: `grep -n "tipModuleConfig" app/rider-platform/rider-app/Main/src-read-only/Domain/Types/RiderConfig.hs`
Expected: `tipModuleConfig :: Kernel.Prelude.Maybe Domain.Types.Extra.RiderConfig.TipModuleConfig,`

- [ ] **Step 4: Build rider-app**

Run: `cabal build rider-app 2>&1 | tail -30`
Expected: success (the field is `Maybe`, so existing constructors of `RiderConfig` in `src/` still compile via record wildcards; if any site constructs `RiderConfig` positionally and fails, add `tipModuleConfig = Nothing`).

- [ ] **Step 5: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/Domain/Types/Extra/RiderConfig.hs Backend/app/rider-platform/rider-app/Main/spec/Storage/RiderConfig.yaml Backend/app/rider-platform/rider-app/Main/src-read-only Backend/dev/migrations-read-only/rider-app/rider_config.sql
git commit -m "rider-app/feat: TipModuleConfig type and RiderConfig.tipModuleConfig default"
```

---

### Task 3: `SharedLogic/TipModuleConfig.hs` — the rule-domain module (with pure tests)

**Files:**
- Create: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/TipModuleConfig.hs`
- Create: `Backend/app/rider-platform/rider-app/Main/test/TipModuleConfigRules.hs`
- Modify: `Backend/app/rider-platform/rider-app/Main/test/Main.hs`
- Modify: `Backend/app/rider-platform/rider-app/Main/package.yaml:233-252` (test dependencies)
- Reference (read only): `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/PickupETA.hs`

**Interfaces:**
- Consumes: `Domain.Types.Extra.RiderConfig.TipModuleConfig` (Task 2); `Lib.Yudhishthira.Types.TIP_MODULE_CONFIG` (Task 1); `Tools.DynamicLogic.getAppDynamicLogic`; `Lib.Yudhishthira.Tools.DebugLog.runLogicsWithDebugLog`.
- Produces:
  ```haskell
  data TipModuleConfigInput = TipModuleConfigInput
    { qar :: Maybe Double, serviceTier :: ServiceTierType
    , estimatedDistanceInKm :: Maybe Double, isValueAddNP :: Bool }
  mkTipModuleConfigInput :: Bool -> Estimate -> TipModuleConfigInput
  tipModuleConfigToss :: Id SearchRequest -> Int                      -- 1..100, deterministic
  getTipModuleConfigFromModel :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, ClickhouseFlow m r)
    => Seconds -> Id SearchRequest -> Id MerchantOperatingCity -> TipModuleConfigInput -> m (Maybe TipModuleConfig)
  resolveTipModuleConfig :: (same constraints)
    => RiderConfig -> Id SearchRequest -> Id MerchantOperatingCity -> TipModuleConfigInput -> m (Maybe TipModuleConfig)
  ```
  plus the canonical seed rules `seedRulesV1 :: [A.Value]` exported for the test and the migration.

- [ ] **Step 1: Write the failing tests**

Create `Backend/app/rider-platform/rider-app/Main/test/TipModuleConfigRules.hs`:

```haskell
module TipModuleConfigRules (tipModuleConfigRulesTests) where

import qualified Data.Aeson as A
import Domain.Types.Extra.RiderConfig (TipModuleConfig (..))
import qualified Domain.Types.ServiceTierType as DVST
import JsonLogic (jsonLogicEither)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import SharedLogic.TipModuleConfig (TipModuleConfigInput (..), seedRulesV1, tipModuleConfigToss)
import Test.Tasty
import Test.Tasty.HUnit

-- Pure re-implementation of Lib.Yudhishthira.Tools.Utils.runLogics: fold the
-- rules left-to-right, each step's output is the next step's input, errors
-- carry the previous object forward. Kept local so the test needs no Flow.
foldRules :: [A.Value] -> A.Value -> A.Value
foldRules rules input = foldl' step input rules
  where
    step acc rule = either (const acc) identity (jsonLogicEither rule acc)

runSeed :: Maybe Double -> A.Result TipModuleConfig
runSeed mbQar =
  A.fromJSON $
    foldRules seedRulesV1 $
      A.toJSON
        TipModuleConfigInput
          { qar = mbQar,
            serviceTier = DVST.AUTO_RICKSHAW,
            estimatedDistanceInKm = Just 3.2,
            isValueAddNP = True
          }

tipModuleConfigRulesTests :: TestTree
tipModuleConfigRulesTests =
  testGroup
    "TipModuleConfig"
    [ testCase "all four seed rules parse" $
        length seedRulesV1 @?= 4,
      testCase "low QAR (0.2) -> early and frequent" $
        runSeed (Just 0.2) @?= A.Success (TipModuleConfig {showAfterSec = 15, repeatIntervalSec = 30, maxPrompts = 3}),
      testCase "mid QAR (0.45) -> moderate" $
        runSeed (Just 0.45) @?= A.Success (TipModuleConfig {showAfterSec = 30, repeatIntervalSec = 45, maxPrompts = 2}),
      testCase "high QAR (0.7) -> late, once" $
        runSeed (Just 0.7) @?= A.Success (TipModuleConfig {showAfterSec = 60, repeatIntervalSec = 0, maxPrompts = 1}),
      testCase "absent QAR -> conservative default branch" $
        runSeed Nothing @?= A.Success (TipModuleConfig {showAfterSec = 45, repeatIntervalSec = 60, maxPrompts = 1}),
      testCase "toss is within 1..100" $
        let tosses = [tipModuleConfigToss (Id (show n)) | n <- [1 .. 500 :: Int]]
         in assertBool "toss out of range" (all (\t -> t >= 1 && t <= 100) tosses),
      testCase "toss is deterministic per search id" $
        tipModuleConfigToss (Id "search-abc") @?= tipModuleConfigToss (Id "search-abc")
    ]
```

`A.Result` needs `Eq` for `@?=`: aeson provides `instance Eq a => Eq (Result a)`, and `TipModuleConfig` derives `Eq` (Task 2).

Replace `Backend/app/rider-platform/rider-app/Main/test/Main.hs` with:

```haskell
import Kernel.Prelude
import Test.Tasty
import TipModuleConfigRules (tipModuleConfigRulesTests)

main :: IO ()
main = defaultMain $ testGroup "rider-app" [tipModuleConfigRulesTests]
```

(The previous body was a fully commented-out DirectQR harness ending in `pure ()`; `test/FRFS/DirectQR.hs` stays in the tree and still compiles as part of the suite.)

In `Backend/app/rider-platform/rider-app/Main/package.yaml`, `tests: rider-app-test: dependencies:` add:
```yaml
      - aeson
      - json-logic-hs
```

- [ ] **Step 2: Regenerate cabal file and verify the test fails to compile**

Run: `, hpack 2>&1 | tail -3` then `cabal build rider-app-test 2>&1 | tail -20`
Expected: `Could not find module 'SharedLogic.TipModuleConfig'`.

- [ ] **Step 3: Write the module**

Create `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/TipModuleConfig.hs`:

```haskell
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE QuasiQuotes #-}

-- | Rider-side JSON-logic domain that turns an estimate's QAR (quote acceptance
-- rate, shipped by the BPP in on_search) into the cadence for the "add tip"
-- module shown during search. Modelled on "SharedLogic.PickupETA".
module SharedLogic.TipModuleConfig
  ( TipModuleConfigInput (..),
    TipModuleConfig (..),
    mkTipModuleConfigInput,
    tipModuleConfigToss,
    getTipModuleConfigFromModel,
    resolveTipModuleConfig,
    seedRulesV1Raw,
    seedRulesV1,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Default.Class
import qualified Data.Hashable as DH
import Domain.Types.Estimate (Estimate (..))
import Domain.Types.Extra.RiderConfig (TipModuleConfig (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.RiderConfig (RiderConfig)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Text.RawString.QQ as RS
import Tools.DynamicLogic

-- | Everything the rules may branch on. Kept small on purpose; extend here
-- (and in the dashboard sample via 'Default') when ops need a new dimension.
data TipModuleConfigInput = TipModuleConfigInput
  { qar :: Maybe Double, -- estimate.qar; Nothing when the BPP sent no QAR tag
    serviceTier :: DVST.ServiceTierType,
    estimatedDistanceInKm :: Maybe Double,
    isValueAddNP :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Default TipModuleConfigInput where
  def =
    TipModuleConfigInput
      { qar = Just 0.5,
        serviceTier = DVST.AUTO_RICKSHAW,
        estimatedDistanceInKm = Just 3.0,
        isValueAddNP = True
      }

mkTipModuleConfigInput :: Bool -> Estimate -> TipModuleConfigInput
mkTipModuleConfigInput isValueAddNP Estimate {..} =
  TipModuleConfigInput
    { qar,
      serviceTier = vehicleServiceTierType,
      estimatedDistanceInKm = (\d -> realToFrac (distanceToHighPrecMeters d) / 1000) <$> estimatedDistance,
      isValueAddNP
    }

-- | Deterministic 1..100 toss per search so every /results poll within one
-- search resolves the same rollout version (same idea as the BPP's
-- poolingLogicVersionToss).
tipModuleConfigToss :: Id DSR.SearchRequest -> Int
tipModuleConfigToss searchReqId = (DH.hash searchReqId.getId `mod` 100) + 1

getTipModuleConfigFromModel ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r
  ) =>
  Seconds ->
  Id DSR.SearchRequest ->
  Id DMOC.MerchantOperatingCity ->
  TipModuleConfigInput ->
  m (Maybe TipModuleConfig)
getTipModuleConfigFromModel timeDiffFromUtc searchReqId merchantOperatingCityId input = do
  localTime <- getLocalCurrentTime timeDiffFromUtc
  (allLogics, _mbVersion) <-
    getAppDynamicLogic
      (cast merchantOperatingCityId)
      LYT.TIP_MODULE_CONFIG
      localTime
      Nothing
      (Just $ tipModuleConfigToss searchReqId)
  if null allLogics
    then do
      logDebug $ "No TipModuleConfig logics for merchantOperatingCityId: " <> show merchantOperatingCityId
      return Nothing
    else do
      response <-
        withTryCatch "runLogics:getTipModuleConfigFromModel" $
          LYDL.runLogicsWithDebugLog LYDL.Rider (cast merchantOperatingCityId) LYT.TIP_MODULE_CONFIG (Just searchReqId.getId) allLogics input
      case response of
        Left e -> do
          logError $ "Error running TipModuleConfig logics - " <> show e <> " - input: " <> show input
          return Nothing
        Right resp ->
          case (A.fromJSON resp.result :: A.Result TipModuleConfig) of
            A.Success result -> return (Just result)
            A.Error err -> do
              logWarning $ "Error parsing TipModuleConfig - " <> show err <> " - " <> show resp <> " - input: " <> show input
              return Nothing

-- | Rules first, city default second. Never throws.
resolveTipModuleConfig ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r
  ) =>
  RiderConfig ->
  Id DSR.SearchRequest ->
  Id DMOC.MerchantOperatingCity ->
  TipModuleConfigInput ->
  m (Maybe TipModuleConfig)
resolveTipModuleConfig riderConfig searchReqId merchantOperatingCityId input = do
  fromRules <- getTipModuleConfigFromModel riderConfig.timeDiffFromUtc searchReqId merchantOperatingCityId input
  return $ fromRules <|> riderConfig.tipModuleConfig

-- | Canonical version-1 program: qar% -> band -> cadence. Also the fixture for
-- the unit test and the source of dev/feature-migrations/0049-tip-module-config.sql
-- (keep the raw strings byte-identical to the SQL).
-- Bands: qar absent -> {45,60,1}; <30% -> {15,30,3}; <60% -> {30,45,2}; else {60,0,1}.
seedRulesV1Raw :: [ByteString]
seedRulesV1Raw =
  [ [RS.r|{"cat":[{"var":""},{"qarPct":{"if":[{"==":[{"var":"qar"},null]},null,{"*":[100,{"var":"qar"}]}]}}]}|],
    [RS.r|{"cat":[{"var":""},{"showAfterSec":{"if":[{"==":[{"var":"qarPct"},null]},45,{"if":[{"<":[{"var":"qarPct"},30]},15,{"if":[{"<":[{"var":"qarPct"},60]},30,60]}]}]}}]}|],
    [RS.r|{"cat":[{"var":""},{"repeatIntervalSec":{"if":[{"==":[{"var":"qarPct"},null]},60,{"if":[{"<":[{"var":"qarPct"},30]},30,{"if":[{"<":[{"var":"qarPct"},60]},45,0]}]}]}}]}|],
    [RS.r|{"cat":[{"var":""},{"maxPrompts":{"if":[{"==":[{"var":"qarPct"},null]},1,{"if":[{"<":[{"var":"qarPct"},30]},3,{"if":[{"<":[{"var":"qarPct"},60]},2,1]}]}]}}]}|]
  ]

-- | Decoded form. A rule that fails to parse is dropped; the unit test asserts
-- the length is 4 so a typo cannot go unnoticed.
seedRulesV1 :: [A.Value]
seedRulesV1 = mapMaybe A.decodeStrict seedRulesV1Raw
```

Notes for the implementer:
- `[r|…|]` is the raw-string quasi-quoter from `raw-strings-qq`, already a rider-app dependency (`package.yaml:120`); with `OverloadedStrings` (on by default in rider-app) the literal types as `ByteString`. Hence the `{-# LANGUAGE QuasiQuotes #-}` pragma at the top of the module.
- `distanceToHighPrecMeters` is exported by `Kernel.Types.Common` (re-export of `Kernel.Types.Distance`); `HighPrecMeters` has a `Real` instance so `realToFrac` to `Double` works.
- `Storage.Beam.Yudhishthira ()` is the same orphan-instance import `PickupETA.hs` uses; keep it.
- `Kernel.Prelude` does not export `ByteString`, so the explicit `Data.ByteString (ByteString)` import is required (same as `Domain/Types/Extra/RiderConfig.hs`).
- `json-logic-hs`'s `if` is strictly ternary (`[cond, then, else]`); multi-branch bands MUST be written as nested `if`s (as above). A flat `[c1,v1,c2,v2,else]` form throws inside the engine and the step is silently skipped.
- `ToSchema` on `TipModuleConfigInput` is needed by Task 5's dashboard schema arm; `Kernel.Prelude` exports `ToSchema`.

- [ ] **Step 4: Build library and run the tests**

Run: `cabal build rider-app 2>&1 | tail -20` — Expected: success (no unused imports; every import above is used).
Run: `cabal run rider-app-test 2>&1 | tail -20`
Expected: 7 tests `OK`, `All 7 tests passed`.

If a band test fails, print the folded object to see which key is off:
`cabal repl rider-app-test` → `import qualified Data.Aeson as A` → `A.encode (foldRules seedRulesV1 (A.toJSON (def :: TipModuleConfigInput)))` (repl is for diagnosis only; the build above is the correctness gate).

- [ ] **Step 5: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/SharedLogic/TipModuleConfig.hs Backend/app/rider-platform/rider-app/Main/test/TipModuleConfigRules.hs Backend/app/rider-platform/rider-app/Main/test/Main.hs Backend/app/rider-platform/rider-app/Main/package.yaml Backend/app/rider-platform/rider-app/Main/rider-app.cabal
git commit -m "rider-app/feat: TIP_MODULE_CONFIG rule domain module with seed rules and unit tests"
```

---

### Task 4: Surface `tipModuleConfig` and `qar` on `EstimateAPIEntity`

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Estimate.hs:16-31` (imports), `:32-86` (record), `:107-141` (`mkEstimateAPIEntity`)
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Quote.hs:456-503` (`getEstimates`)

**Interfaces:**
- Consumes: `SharedLogic.TipModuleConfig.{mkTipModuleConfigInput, resolveTipModuleConfig}` (Task 3); `Domain.Types.Extra.RiderConfig.TipModuleConfig` (Task 2).
- Produces: `mkEstimateAPIEntity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Bool -> Maybe SOffer.CumulativeOfferResp -> BppDetails -> Bool -> Maybe TipModuleConfig -> Estimate -> m EstimateAPIEntity` (one new argument, position 5); `EstimateAPIEntity.tipModuleConfig :: Maybe TipModuleConfig`, `EstimateAPIEntity.qar :: Maybe Double`.

- [ ] **Step 1: Add fields and the new argument to the API entity**

In `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Estimate.hs`:

Add import (alphabetically after `import Domain.Types.EstimateStatus`):
```haskell
import Domain.Types.Extra.RiderConfig (TipModuleConfig)
```

In `data EstimateAPIEntity`, after `navigationInstruction :: Maybe Text` (last field), add:
```haskell
    navigationInstruction :: Maybe Text,
    tipModuleConfig :: Maybe TipModuleConfig,
    qar :: Maybe Double
```
(the field before gains a trailing comma).

Change the builder signature and set the field explicitly (`qar` is filled by the existing `Estimate {..}` wildcard because the domain `Estimate` already has `qar :: Maybe Double`):
```haskell
mkEstimateAPIEntity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Bool -> Maybe SOffer.CumulativeOfferResp -> BppDetails -> Bool -> Maybe TipModuleConfig -> Estimate -> m EstimateAPIEntity
mkEstimateAPIEntity isReferredRide offer bppDetails valueAddNPRes mbTipModuleConfig (Estimate {..}) = do
```
and inside the `EstimateAPIEntity { … }` literal add, next to `isInsured = Just isInsured,`:
```haskell
        isInsured = Just isInsured,
        tipModuleConfig = mbTipModuleConfig,
        ..
```

- [ ] **Step 2: Wire the caller in `getEstimates`**

In `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Quote.hs` add the import (alphabetically among `SharedLogic.*` imports):
```haskell
import qualified SharedLogic.TipModuleConfig as STMC
```
Inside `getEstimates`, replace the single line
```haskell
    apiEntity <- UEstimate.mkEstimateAPIEntity isReferredRide mbOffer bppDetails valueAddNP estimate
```
with
```haskell
    mbTipModuleConfig <-
      join
        <$> forM
          riderConfig
          ( \rc ->
              STMC.resolveTipModuleConfig
                rc
                searchRequest.id
                searchRequest.merchantOperatingCityId
                (STMC.mkTipModuleConfigInput valueAddNP estimate)
          )
    apiEntity <- UEstimate.mkEstimateAPIEntity isReferredRide mbOffer bppDetails valueAddNP mbTipModuleConfig estimate
```
`riderConfig` (a `Maybe RiderConfig`) is already fetched once at the top of `getEstimates` via `getConfig (RiderConfigDimensions …)`, so this adds no per-estimate config lookups. `getEstimates` runs in `Flow`, which satisfies the `ClickhouseFlow`/`EsqDBReplicaFlow` constraints of `resolveTipModuleConfig`.

- [ ] **Step 3: Find and fix any other call sites**

Run: `grep -rn "mkEstimateAPIEntity" Backend/app/rider-platform/rider-app/Main/src Backend/app/rider-platform/rider-app/Main/src-read-only`
Expected: only the definition and the `Quote.hs` call. If another call exists, pass `Nothing` for the new argument there (those paths have no search context for rules).

- [ ] **Step 4: Build**

Run: `cabal build rider-app 2>&1 | tail -30`
Expected: success. Then `cabal build all 2>&1 | tail -10` — Expected: success (dashboards re-export the API types only via OpenAPI/`ToSchema`, no code change).

- [ ] **Step 5: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Estimate.hs Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Quote.hs
git commit -m "rider-app/feat: return tipModuleConfig and qar on EstimateAPIEntity"
```

---

### Task 5: Dashboard registration (verify + schema arms)

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/NammaTag.hs:117` (imports), `:371-373` (verify arm), `:595-600` (schema arm)

**Interfaces:**
- Consumes: `SharedLogic.TipModuleConfig.{TipModuleConfigInput, TipModuleConfig}` and its `Default` instance (Task 3); `Lib.Yudhishthira.Types.TIP_MODULE_CONFIG` (Task 1).
- Produces: dashboard `postNammaTagAppDynamicLogicVerify` and `getNammaTagAppDynamicLogicGetDomainSchema` handle `TIP-MODULE-CONFIG`.

- [ ] **Step 1: Add the import**

Next to `import qualified SharedLogic.PickupETA as PickupETA` add:
```haskell
import qualified SharedLogic.TipModuleConfig as TipModuleConfig
```

- [ ] **Step 2: Add the verify arm**

Directly after the existing arm
```haskell
    LYTU.PICKUP_ETA_CALCULATION -> do
      logicData :: PickupETA.PickupETAInput <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (cast merchantOpCityId) (Proxy :: Proxy PickupETA.PickupETAResult) _riderConfig.dynamicLogicUpdatePassword req logicData
```
add
```haskell
    LYTU.TIP_MODULE_CONFIG -> do
      logicData :: TipModuleConfig.TipModuleConfigInput <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantid (cast merchantOpCityId) (Proxy :: Proxy TipModuleConfig.TipModuleConfig) _riderConfig.dynamicLogicUpdatePassword req logicData
```

- [ ] **Step 3: Add the schema arm**

Directly after
```haskell
    LYTU.PICKUP_ETA_CALCULATION ->
      return $
        LYTU.DomainSchemaResp
          { LYTU.defaultValue = A.toJSON (def :: PickupETA.PickupETAInput),
            LYTU.schema = toInlinedSchemaValue (Proxy @PickupETA.PickupETAInput)
          }
```
add
```haskell
    LYTU.TIP_MODULE_CONFIG ->
      return $
        LYTU.DomainSchemaResp
          { LYTU.defaultValue = A.toJSON (def :: TipModuleConfig.TipModuleConfigInput),
            LYTU.schema = toInlinedSchemaValue (Proxy @TipModuleConfig.TipModuleConfigInput)
          }
```
`toInlinedSchemaValue` needs `ToSchema TipModuleConfigInput`, which Task 3 already derives.

- [ ] **Step 4: Build**

Run: `cabal build rider-app 2>&1 | tail -30` — Expected: success.

- [ ] **Step 5: Verify through the dashboard API shape (compile-level)**

Run: `grep -n "TIP_MODULE_CONFIG" Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/NammaTag.hs`
Expected: two hits (verify arm and schema arm).

- [ ] **Step 6: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/NammaTag.hs
git commit -m "rider-app/feat: register TIP_MODULE_CONFIG domain in dashboard verify and schema"
```

---

### Task 6: `select2` estimate-expiry guard

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs:226-241`

**Interfaces:**
- Produces: `select2` throws `InvalidRequest "Estimate expired <id>"` for a non-multimodal select on an estimate whose `validTill < now`; `select` becomes a thin alias.

- [ ] **Step 1: Move the guard from `select` into `select2`**

Replace
```haskell
select :: SelectFlow m r c => Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> m DSelectRes
select personId estimateId req = do
  now <- getCurrentTime
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  when (estimate.validTill < now) $ throwError (InvalidRequest $ "Estimate expired " <> show estimate.id) -- select validation check
  select2 personId estimateId req Nothing
```
with
```haskell
select :: SelectFlow m r c => Id DPerson.Person -> Id DEstimate.Estimate -> DSelectReq -> m DSelectRes
select personId estimateId req = select2 personId estimateId req Nothing
```
and in `select2`, immediately after
```haskell
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
```
insert
```haskell
  -- Expiry guard (previously only on v1 /select). Skipped for multimodal journey
  -- legs, which select taxi estimates on their own schedule.
  when (isNothing mbJourneyLegData) $ do
    now <- getCurrentTime
    when (estimate.validTill < now) $ throwError (InvalidRequest $ "Estimate expired " <> show estimate.id)
```

- [ ] **Step 2: Build**

Run: `cabal build rider-app 2>&1 | tail -20` — Expected: success (`isNothing` is in `Kernel.Prelude`; `getCurrentTime` was already imported for `select`).

- [ ] **Step 3: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Select.hs
git commit -m "rider-app/fix: apply estimate expiry check on select2 as well as select"
```

---

### Task 7: Seed migration for local/dev

**Files:**
- Create: `Backend/dev/feature-migrations/0049-tip-module-config.sql`
- Reference: `Backend/dev/feature-migrations/0002-delhi-offline-payment-cashback-offers.sql` (style), `SharedLogic/TipModuleConfig.hs` `seedRulesV1` (Task 3, must match byte-for-byte)

**Interfaces:**
- Produces: `TIP-MODULE-CONFIG` version 1 (4 elements) + 100% `Unbounded` rollout for every `NAMMA_YATRI` operating city, and a `RiderConfig.tipModuleConfig` default for those cities.

- [ ] **Step 1: Write the migration**

```sql
-- TIP-MODULE-CONFIG v1: QAR-driven cadence for the rider "add tip" module.
-- Mirrors SharedLogic.TipModuleConfig.seedRulesV1 (keep in sync; unit test covers the rules).
-- Bands: qar absent -> {45,60,1}; <30% -> {15,30,3}; <60% -> {30,45,2}; else {60,0,1}.

DO $$
DECLARE
  v_merchant_id TEXT;
  v_city RECORD;
BEGIN
  SELECT m.id INTO v_merchant_id FROM atlas_app.merchant m WHERE m.short_id = 'NAMMA_YATRI' LIMIT 1;
  IF v_merchant_id IS NULL THEN
    RAISE NOTICE 'NAMMA_YATRI merchant not found, skipping TIP-MODULE-CONFIG seed';
    RETURN;
  END IF;

  INSERT INTO atlas_app.app_dynamic_logic_element (domain, merchant_id, version, logic, description, created_at, updated_at, "order") VALUES
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"qarPct":{"if":[{"==":[{"var":"qar"},null]},null,{"*":[100,{"var":"qar"}]}]}}]}',
     'qar (0..1) -> qarPct, null when absent', now(), now(), 0),
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"showAfterSec":{"if":[{"==":[{"var":"qarPct"},null]},45,{"if":[{"<":[{"var":"qarPct"},30]},15,{"if":[{"<":[{"var":"qarPct"},60]},30,60]}]}]}}]}',
     'showAfterSec by QAR band', now(), now(), 1),
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"repeatIntervalSec":{"if":[{"==":[{"var":"qarPct"},null]},60,{"if":[{"<":[{"var":"qarPct"},30]},30,{"if":[{"<":[{"var":"qarPct"},60]},45,0]}]}]}}]}',
     'repeatIntervalSec by QAR band', now(), now(), 2),
    ('TIP-MODULE-CONFIG', v_merchant_id, 1,
     '{"cat":[{"var":""},{"maxPrompts":{"if":[{"==":[{"var":"qarPct"},null]},1,{"if":[{"<":[{"var":"qarPct"},30]},3,{"if":[{"<":[{"var":"qarPct"},60]},2,1]}]}]}}]}',
     'maxPrompts by QAR band', now(), now(), 3)
  ON CONFLICT (domain, "order", version) DO UPDATE SET
    logic = EXCLUDED.logic, description = EXCLUDED.description, updated_at = now();

  FOR v_city IN SELECT id FROM atlas_app.merchant_operating_city WHERE merchant_id = v_merchant_id LOOP
    INSERT INTO atlas_app.app_dynamic_logic_rollout
      (domain, merchant_operating_city_id, percentage_rollout, time_bounds, version, version_description, merchant_id, created_at, updated_at)
    VALUES ('TIP-MODULE-CONFIG', v_city.id, 100, 'Unbounded', 1, 'QAR bands v1', v_merchant_id, now(), now())
    ON CONFLICT DO NOTHING;

    -- Per-city fallback used when rules yield nothing (e.g. rollout 0%).
    UPDATE atlas_app.rider_config
    SET tip_module_config = '{"showAfterSec":45,"repeatIntervalSec":60,"maxPrompts":1}'::json,
        updated_at = now()
    WHERE merchant_operating_city_id = v_city.id AND tip_module_config IS NULL;
  END LOOP;

  RAISE NOTICE 'TIP-MODULE-CONFIG v1 seeded and rolled out at 100%% for NAMMA_YATRI cities';
END $$;
```

- [ ] **Step 2: Sanity-check the JSON strings match the Haskell fixture**

Run:
```bash
grep -o '{"cat":\[{"var":""}[^'"'"']*' Backend/dev/feature-migrations/0049-tip-module-config.sql | sort > /tmp/sql_rules.txt
grep -o '\[RS\.r|{"cat":\[{"var":""}[^|]*' Backend/app/rider-platform/rider-app/Main/src/SharedLogic/TipModuleConfig.hs | sed 's/^\[RS\.r|//' | sort > /tmp/hs_rules.txt
diff /tmp/sql_rules.txt /tmp/hs_rules.txt && echo IDENTICAL
```
Expected: `IDENTICAL`. (Use the scratchpad directory instead of `/tmp` if the session prescribes one.)

- [ ] **Step 3: Commit**

```bash
git add Backend/dev/feature-migrations/0049-tip-module-config.sql
git commit -m "rider-app/chore: seed TIP-MODULE-CONFIG v1 rules, rollout and RiderConfig default for local"
```

---

### Task 8: Integration assertions (Postman) and end-to-end check

**Files:**
- Modify: `Backend/dev/integration-tests/collections/RideBookingFlow/01-AutoRideFlow.json` — the `Get Search Results` request's `test` script and the `Select Estimate (Auto Assign)` request
- Reference: `Backend/dev/integration-tests/Rules.md`

**Interfaces:**
- Consumes: running local stack with the migrations of Task 2 (`rider_config.tip_module_config` column) and Task 7 (rules + rollout + default) applied.

- [ ] **Step 1: Confirm the stack and DB state (do not start the stack yourself)**

Ask the user to have `, run-mobility-stack-dev` running (probe: `curl -s -o /dev/null -w '%{http_code}\n' http://localhost:8013/healthCheck` → `200`), then, only if they confirm migrations should be applied locally, apply Task 2's generated `rider_config.sql` and Task 7's seed with the nix `psql` against `atlas_dev` (per the user's local infra ports). Otherwise ask them to run them.

- [ ] **Step 2: Extend the `Get Search Results` test script**

In `01-AutoRideFlow.json`, `Get Search Results` → `event[listen=test].script.exec`, append these lines (keep the existing ones):

```js
pm.test('Every estimate carries tipModuleConfig from TIP-MODULE-CONFIG rules (QAR absent locally -> {45,60,1})', function () {
    d.estimates.forEach(function (e) {
        pm.expect(e, 'estimate').to.have.property('tipModuleConfig');
        pm.expect(e.tipModuleConfig, 'tipModuleConfig').to.be.an('object');
        pm.expect(e.tipModuleConfig.showAfterSec).to.eql(45);
        pm.expect(e.tipModuleConfig.repeatIntervalSec).to.eql(60);
        pm.expect(e.tipModuleConfig.maxPrompts).to.eql(1);
        pm.expect(e, 'estimate').to.have.property('qar');
    });
});
pm.collectionVariables.set('_test_tip_cfg_first_poll', JSON.stringify(d.estimates.map(function (e) { return [e.id, e.tipModuleConfig]; })));
```

Duplicate the `Get Search Results` request as `Get Search Results (Again — deterministic tip config)` immediately after it, with this test script:

```js
var d = pm.response.json();
pm.test('Status code is 200', function () { pm.response.to.have.status(200); });
pm.test('tipModuleConfig is identical across polls of the same search', function () {
    var first = JSON.parse(pm.collectionVariables.get('_test_tip_cfg_first_poll') || '[]');
    var second = d.estimates.map(function (e) { return [e.id, e.tipModuleConfig]; });
    pm.expect(second).to.eql(first);
});
```

(Locally the BPP does not emit the `QAR` tag unless `isDynamicPricingQARCalEnabled` is on and the 15-minute bucket has > 4 demand events, so `qar` is `null` and the rules take the null branch — exactly what the assertion checks. In `Master` env this assertion is not meaningful; guard it with `if (pm.environment.get('envType') === 'Local')` per `Rules.md`.)

- [ ] **Step 3: Add the expired-estimate select2 check**

Add a request `Select2 on expired estimate is rejected` after `Get Search Results (Again …)`, `POST {{baseUrl_app}}/estimate/00000000-0000-0000-0000-000000000000/select2` with the same body as `Select Estimate (Auto Assign)`, test script:

```js
pm.test('unknown estimate -> 400 (EstimateDoesNotExist)', function () { pm.response.to.have.status(400); });
```
and, for the actual expiry path, a prerequest that sets `validTill` in the past is not possible from the API; instead assert on the real selected estimate after the flow completes: add at the very end of the collection a request `Select2 after search expiry` → `POST {{baseUrl_app}}/estimate/{{estimateId}}/select2` with test:

```js
pm.test('re-select of an expired/consumed estimate is rejected', function () {
    pm.expect(pm.response.code).to.be.oneOf([400]);
    var body = pm.response.json();
    pm.expect(JSON.stringify(body)).to.match(/Estimate expired|ActiveBookingPresent|EstimateDoesNotExist|INVALID_REQUEST/);
});
```
(After ride completion the estimate is past `validTill` and/or a booking exists; either guard is acceptable — the point is that `select2` no longer silently re-fires.)

- [ ] **Step 4: Run the collection**

Run (stack up, from `Backend/`): `newman run dev/integration-tests/collections/RideBookingFlow/01-AutoRideFlow.json -e dev/integration-tests/collections/RideBookingFlow/Local/Local_NY_Bangalore.postman_environment.json 2>&1 | tail -40`
(or via the test dashboard at `http://localhost:7070` if the user prefers).
Expected: all assertions pass, including the three new tests.

- [ ] **Step 5: Manual fallback + rollout checks (documented, run if the user wants)**

1. `UPDATE atlas_app.app_dynamic_logic_rollout SET percentage_rollout = 0 WHERE domain = 'TIP-MODULE-CONFIG';` → clear the rider-app dynamic-logic cache (config-pilot two-layer cache: flush Redis key prefix `CacheHash:TIP-MODULE-CONFIG*` **and** restart rider-app) → `/results` returns the RiderConfig default `{45,60,1}` (same numbers as the null band, so also set the RiderConfig default to `{50,0,1}` temporarily to see the difference).
2. `UPDATE atlas_app.rider_config SET tip_module_config = NULL …` with rollout still 0 → `tipModuleConfig: null`.
3. Restore rollout to 100 and the default; flush + restart.
4. Dashboard: `POST …/nammaTag/appDynamicLogic/verify` with `{"domain":"TIP-MODULE-CONFIG","rules":[…seedRulesV1…],"inputData":[{"qar":0.2}]}` → `result` decodes to `{15,30,3}`; repeat with `0.7` and `null`.
5. Observability: enable the JSON-logic debug flag for the local city + `TIP-MODULE-CONFIG` (dashboard `setJsonLogicDebugFlags`, or Redis keys `JSONLogicDebug:<mocId>:TIP-MODULE-CONFIG:{enabled,startTime,endTime}`), run one search, and confirm rows in ClickHouse `app_monitor.json_logic_transactions` with `domain = 'TIP-MODULE-CONFIG'` showing the input object and the decoded output.

- [ ] **Step 6: Commit**

```bash
git add Backend/dev/integration-tests/collections/RideBookingFlow/01-AutoRideFlow.json
git commit -m "rider-app/test: assert tipModuleConfig/qar on search results and select2 expiry guard"
```

---

### Task 9: Final verification and wrap-up

- [ ] **Step 1: Full build and both unit suites**

Run: `cabal build all 2>&1 | tail -10 && cabal run yudhishthira-tests 2>&1 | tail -5 && cabal run rider-app-test 2>&1 | tail -5`
Expected: build success; both suites `All … tests passed`.

- [ ] **Step 2: Diff review against the spec**

Run: `git log --oneline main..HEAD` — Expected 8 commits (spec + Tasks 1–8). Run `git diff main --stat` and confirm the touched files match the spec's "Files touched" table; anything outside it needs a reason in the commit or PR body.

- [ ] **Step 3: Hand off**

Do not push or open a PR unless the user asks. Report: the new API fields, the domain string ops will use (`TIP-MODULE-CONFIG`), the seed migration number, and the frontend contract (start timer at `select2`; show at `showAfterSec`; repeat every `repeatIntervalSec` while shown `< maxPrompts`; `null` → hide).
