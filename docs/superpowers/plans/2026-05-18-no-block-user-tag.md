# NO_BLOCK_USER Tag Implementation Plan

> **STATUS — Implemented, then narrowed after review.** Branch `rider/feat/no-block-user-tag`. The originally drafted Tasks 2 and 3 sink-refactors were discarded in favour of a single storage-helper gate. After review feedback (gate where blocks are *created*; don't react to blocked-state on every read path), the design was narrowed to: **keep** the storage-level write guard in `PersonExtra.hs` + the fraud-entry early-exits (`Search.fraudCheck`, the auth-fraud `fork` in `Registration`, `CustomerCancellationRate.nudgeOrBlockCustomer`); **drop** the read-side guards (`AadhaarVerification`, `Registration` login-deny/auth-reject), the dashboard `assertCanBlock` + `PersonHasNoBlockUserTag` (HTTP 400) error, and the auto-unblock-on-tag-add hook. Consequence: the tag prevents *future* blocks but does not unblock an already-blocked rider. Task 10's integration tests landed as the Postman collection `Backend/dev/integration-tests/collections/CustomerBlockingFlow/01-NoBlockUserTag.json` (runnable via `./run-tests.sh customer-blocking`); the suite now verifies that a dashboard block on a tagged rider is a silent no-op (rider stays unblocked) and that blocking works once the tag is removed. Task bodies below describe the original, wider design and are retained for history.

## Implementation Notes (read before consulting the task bodies below)

1. **Task 2 was rewritten** mid-flight from "Refactor `SMC.blockCustomer` to take `Person` and gate" to "Gate the three block-state helpers (`updatingEnabledAndBlockedState`, `updatingBlockedStateWithUntil`, `updatingAuthEnabledAndBlockedState`) inside `Storage/Queries/PersonExtra.hs`". Reason: the first attempt missed two `SMC.blockCustomer` callers in `Domain/Action/Beckn/Common.hs:1330,1524` — the storage-level guard catches every caller (existing and future) for free and reuses the row that those helpers already fetch.
2. **Task 3 was scope-reduced** to just the `nudgeOrBlockCustomer` pre-check. The originally planned `blockCustomerTemporarily` signature refactor is no longer needed because the storage guard covers that path.
3. **Tasks 4–9 shipped as planned** with no notable deviations.

Shipped commits (in order):
- `4ec4082aa3` — Task 1: helper module + `PersonHasNoBlockUserTag` error
- `438eb8f002` — Task 2 (revised): storage-level guard in `PersonExtra.hs`
- `ea2237b269` — Task 3: cancellation pre-check
- `2f675bd0d2` — Task 4: auth/IP fraud fork bypass
- `adf48bea44` — Task 5: search fraudCheck pre-check
- `a78148e65f` — Task 6: Aadhaar read-side guard
- `a7c360fcab` — Task 7: login-deny guard
- `fc36071fe4` — Task 8: dashboard explicit error
- `a246b457b6` — Task 9: auto-unblock on tag add

---

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `NO_BLOCK_USER` tag on `Person.customerNammaTags` that bypasses every rider-side block path (cancellation rate, search/booking fraud, auth/IP fraud, device-token fraud, dashboard manual block) and auto-unblocks tagged users.

**Architecture (as shipped):** A single helper module `SharedLogic.PersonBlock` exposes `isNoBlockUser :: Person -> Bool` (presence check on tag name, via existing `Lib.Yudhishthira.Tools.Utils.elemTagName`) and `assertCanBlock :: Person -> m ()` (throws a typed error). The three Person block-state helpers in `Storage/Queries/PersonExtra.hs` gate the actual DB write when `isNoBlockUser` is True and the call sets `blocked=true`. The auth-fraud `fork` in `Registration.auth` is gated at the caller (Redis IP-block path, not a Person write). The dashboard handler calls `assertCanBlock` for explicit operator error. Pre-checks in `nudgeOrBlockCustomer` and `fraudCheck` skip redundant Redis/Clickhouse work. Two read-side guards (`AadhaarVerification.hs`, `Registration.hs:419`) handle the case where a `blocked=true` row exists when the tag is added. Tag-add handler clears the three block fields and logs once. No DB migration, no `src-read-only/` edits.

**Tech Stack:** Haskell, Servant, EulerHS prelude, Beam ORM, `cabal build all` for verification, project-specific integration test harness (via `, run-tests`).

**Conventions referenced:** `CLAUDE.md` Critical Rules — never edit `src-read-only/`; `-Werror` is in effect; use `logInfo` from `Kernel.Utils.Logging`; commit prefix `<sub-project>/<type>: <summary>` (here: `rider/feat: ...`).

---

## Pre-flight

- [ ] **Step 0: Confirm on a feature branch (do NOT work on `main`)**

Run:
```bash
git -C /home/shailesh/Desktop/Github/nammayatri rev-parse --abbrev-ref HEAD
```

If output is `main`, create a branch:
```bash
git -C /home/shailesh/Desktop/Github/nammayatri checkout -b rider/feat/no-block-user-tag
```

Expected: a non-`main` branch name on subsequent re-runs.

---

## Task 1: Add `SharedLogic.PersonBlock` helper module + extend `CustomerError`

**Files:**
- Create: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/PersonBlock.hs`
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Tools/Error.hs:24-41`

This task is foundational — every later task depends on `isNoBlockUser`, `assertCanBlock`, and `PersonHasNoBlockUserTag`.

- [ ] **Step 1.1: Extend `CustomerError` with `PersonHasNoBlockUserTag Text`**

Edit `Tools/Error.hs`. The current block (lines 24-41) is:

```haskell
data CustomerError = PersonMobileAlreadyExists Text | DeviceTokenNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CustomerError

instance IsBaseError CustomerError where
  toMessage (PersonMobileAlreadyExists phoneNo) = Just $ "Mobile number " <> phoneNo <> " already exists with another user."
  toMessage DeviceTokenNotFound = Just "Device Token does not exist."

instance IsHTTPError CustomerError where
  toErrorCode = \case
    PersonMobileAlreadyExists _ -> "PERSON_MOBILE_ALREADY_EXISTS"
    DeviceTokenNotFound -> "DEVICE_TOKEN_NOT_FOUND"
  toHttpCode = \case
    PersonMobileAlreadyExists _ -> E400
    DeviceTokenNotFound -> E400

instance IsAPIError CustomerError
```

Replace with:

```haskell
data CustomerError
  = PersonMobileAlreadyExists Text
  | DeviceTokenNotFound
  | PersonHasNoBlockUserTag Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CustomerError

instance IsBaseError CustomerError where
  toMessage (PersonMobileAlreadyExists phoneNo) = Just $ "Mobile number " <> phoneNo <> " already exists with another user."
  toMessage DeviceTokenNotFound = Just "Device Token does not exist."
  toMessage (PersonHasNoBlockUserTag personId) = Just $ "Cannot block person " <> personId <> ": NO_BLOCK_USER tag is set. Remove the tag first."

instance IsHTTPError CustomerError where
  toErrorCode = \case
    PersonMobileAlreadyExists _ -> "PERSON_MOBILE_ALREADY_EXISTS"
    DeviceTokenNotFound -> "DEVICE_TOKEN_NOT_FOUND"
    PersonHasNoBlockUserTag _ -> "PERSON_HAS_NO_BLOCK_USER_TAG"
  toHttpCode = \case
    PersonMobileAlreadyExists _ -> E400
    DeviceTokenNotFound -> E400
    PersonHasNoBlockUserTag _ -> E400

instance IsAPIError CustomerError
```

- [ ] **Step 1.2: Create `SharedLogic/PersonBlock.hs`**

Write the new file with exactly this content:

```haskell
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PersonBlock
  ( isNoBlockUser,
    assertCanBlock,
    noBlockTagName,
  )
where

import qualified Domain.Types.Person as DPerson
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (throwError)
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Tools.Error (CustomerError (..))

noBlockTagName :: LYT.TagName
noBlockTagName = LYT.TagName "NO_BLOCK_USER"

-- | True iff `customerNammaTags` contains a tag whose name is "NO_BLOCK_USER".
-- Value and expiry are intentionally ignored (presence-only contract).
isNoBlockUser :: DPerson.Person -> Bool
isNoBlockUser person =
  case person.customerNammaTags of
    Nothing -> False
    Just tags -> LYTU.elemTagName noBlockTagName tags

-- | Used by the dashboard manual-block handler. Throws PersonHasNoBlockUserTag
-- (HTTP 400) if the person carries the tag. Auto-block sinks should NOT use
-- this helper — they should silently no-op via `isNoBlockUser`.
assertCanBlock :: (MonadThrow m, Log m) => DPerson.Person -> m ()
assertCanBlock person =
  when (isNoBlockUser person) $
    throwError (PersonHasNoBlockUserTag person.id.getId)
```

- [ ] **Step 1.3: Build to verify**

Run from `Backend/`:
```bash
cabal build rider-app
```

Expected: builds successfully. If `Lib.Yudhishthira.Tools.Utils` or `Lib.Yudhishthira.Types` import paths fail, check that `rider-app.cabal` includes `yudhishthira` as a dependency (it already does — `customerNammaTags` is wired up).

- [ ] **Step 1.4: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/SharedLogic/PersonBlock.hs \
  Backend/app/rider-platform/rider-app/Main/src/Tools/Error.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: add SharedLogic.PersonBlock helper and PersonHasNoBlockUserTag error"
```

---

## Task 2: Gate `SMC.blockCustomer` and update its 3 callers

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/MerchantConfig.hs:196-203`
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/Customer.hs:101`
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs:660-661`
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs:949`

The current signature is `blockCustomer :: Id Person.Person -> Maybe (Id DMC.MerchantConfig) -> m ()`. Refactor to take `Person.Person` so the sink itself can read tags. All three callers already have the `Person` record in scope.

- [ ] **Step 2.1: Refactor `SMC.blockCustomer` to take `Person`**

In `SharedLogic/MerchantConfig.hs`, find:

```haskell
blockCustomer :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id Person.Person -> Maybe (Id DMC.MerchantConfig) -> m ()
blockCustomer riderId mcId = do
  regTokens <- RT.findAllByPersonId riderId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key
  _ <- RT.deleteByPersonId riderId
  void $ QP.updatingEnabledAndBlockedState riderId mcId True
```

Replace with:

```haskell
blockCustomer :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Person.Person -> Maybe (Id DMC.MerchantConfig) -> m ()
blockCustomer person mcId
  | SharedLogic.PersonBlock.isNoBlockUser person = pure ()
  | otherwise = do
      let riderId = person.id
      regTokens <- RT.findAllByPersonId riderId
      for_ regTokens $ \regToken -> do
        let key = authTokenCacheKey regToken.token
        void $ Redis.del key
      _ <- RT.deleteByPersonId riderId
      void $ QP.updatingEnabledAndBlockedState riderId mcId True
```

Add the import at the top of the file (after the existing `Storage.Queries.Person` import):

```haskell
import qualified SharedLogic.PersonBlock
```

No log line on the bypass branch — per spec logging policy.

- [ ] **Step 2.2: Update caller in `Domain/Action/UI/Search.hs:660-661`**

Find:
```haskell
      mFraudDetected <- SMC.anyFraudDetected person.id merchantOperatingCity.id merchantConfigs (Just searchRequest)
      whenJust mFraudDetected $ \mc -> SMC.blockCustomer person.id (Just mc.id)
```

Replace with:
```haskell
      mFraudDetected <- SMC.anyFraudDetected person.id merchantOperatingCity.id merchantConfigs (Just searchRequest)
      whenJust mFraudDetected $ \mc -> SMC.blockCustomer person (Just mc.id)
```

(Single-character change at end — pass `person` instead of `person.id`.)

- [ ] **Step 2.3: Update caller in `Domain/Action/UI/Registration.hs:949`**

Find:
```haskell
    when merchantConfig.useFraudDetection $ SMC.blockCustomer person.id ((.blockedByRuleId) =<< personWithSameDeviceToken)
```

Replace with:
```haskell
    when merchantConfig.useFraudDetection $ SMC.blockCustomer person ((.blockedByRuleId) =<< personWithSameDeviceToken)
```

- [ ] **Step 2.4: Update caller in `Domain/Action/Dashboard/Customer.hs:101`**

Find:
```haskell
  SMC.blockCustomer personId Nothing
  logTagInfo "dashboard -> blockCustomer : " (show personId)
```

Replace with:
```haskell
  SMC.blockCustomer customer Nothing
  logTagInfo "dashboard -> blockCustomer : " (show personId)
```

(The `customer` Person record is already in scope from line 92-95.)

- [ ] **Step 2.5: Build to verify all callers compile**

Run from `Backend/`:
```bash
cabal build rider-app
```

Expected: builds cleanly. If a missed caller fails compilation, fix it the same way (`person.id` → `person`).

- [ ] **Step 2.6: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/SharedLogic/MerchantConfig.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/Customer.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: silent NO_BLOCK_USER bypass at SMC.blockCustomer sink"
```

---

## Task 3: Gate `blockCustomerTemporarily` and pre-check `nudgeOrBlockCustomer`

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/BehaviourManagement/CustomerCancellationRate.hs:173-283`

- [ ] **Step 3.1: Refactor `blockCustomerTemporarily` to take `Person` and gate**

Current signature/body (lines 267-283):

```haskell
blockCustomerTemporarily ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, JobCreator r m, HasShortDurationRetryCfg r c) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Int ->
  m ()
blockCustomerTemporarily merchantId merchantOperatingCityId customerId suspensionTimeHours = do
  now <- getCurrentTime
  let blockedUntil = addUTCTime (fromIntegral suspensionTimeHours * 60 * 60) now
  void $ QPExtra.updatingBlockedStateWithUntil customerId Nothing True (Just blockedUntil)
  let unblockCustomerJobTs = secondsToNominalDiffTime (fromIntegral suspensionTimeHours) * 60 * 60
  JC.createJobIn @_ @'RJS.UnblockCustomer (Just merchantId) (Just merchantOperatingCityId) unblockCustomerJobTs $
    RJS.UnblockCustomerJobData
      { customerId = customerId
      }
  logInfo $ "Temporarily blocking customer, customerId: " <> customerId.getId <> " until: " <> show blockedUntil <> ". Unblock job scheduled."
```

Replace with:

```haskell
blockCustomerTemporarily ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, JobCreator r m, HasShortDurationRetryCfg r c) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DP.Person ->
  Int ->
  m ()
blockCustomerTemporarily merchantId merchantOperatingCityId customer suspensionTimeHours
  | SharedLogic.PersonBlock.isNoBlockUser customer = pure ()
  | otherwise = do
      let customerId = customer.id
      now <- getCurrentTime
      let blockedUntil = addUTCTime (fromIntegral suspensionTimeHours * 60 * 60) now
      void $ QPExtra.updatingBlockedStateWithUntil customerId Nothing True (Just blockedUntil)
      let unblockCustomerJobTs = secondsToNominalDiffTime (fromIntegral suspensionTimeHours) * 60 * 60
      JC.createJobIn @_ @'RJS.UnblockCustomer (Just merchantId) (Just merchantOperatingCityId) unblockCustomerJobTs $
        RJS.UnblockCustomerJobData
          { customerId = customerId
          }
      logInfo $ "Temporarily blocking customer, customerId: " <> customerId.getId <> " until: " <> show blockedUntil <> ". Unblock job scheduled."
```

Add import near the top of the file (after `import qualified Storage.Queries.PersonExtra as QPExtra`):

```haskell
import qualified SharedLogic.PersonBlock
```

- [ ] **Step 3.2: Update the three call sites of `blockCustomerTemporarily` inside `nudgeOrBlockCustomer`**

Find (lines 217-225):
```haskell
        (True, _, _) -> do
          let suspensionTimeHours = weeklyOffenceSuspensionTimeHours
          blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer.id suspensionTimeHours
        (_, True, _) -> do
          let suspensionTimeHours = dailyOffenceSuspensionTimeHours
          blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer.id suspensionTimeHours
        (_, _, True) -> do
          let suspensionTimeHours = fromMaybe 0 monthlyOffenceSuspensionTimeHours
          when (suspensionTimeHours > 0) $ do
            blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer.id suspensionTimeHours
```

Replace each `customer.id` argument with `customer`:

```haskell
        (True, _, _) -> do
          let suspensionTimeHours = weeklyOffenceSuspensionTimeHours
          blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer suspensionTimeHours
        (_, True, _) -> do
          let suspensionTimeHours = dailyOffenceSuspensionTimeHours
          blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer suspensionTimeHours
        (_, _, True) -> do
          let suspensionTimeHours = fromMaybe 0 monthlyOffenceSuspensionTimeHours
          when (suspensionTimeHours > 0) $ do
            blockCustomerTemporarily customer.merchantId customer.merchantOperatingCityId customer suspensionTimeHours
```

- [ ] **Step 3.3: Add pre-check at the top of `nudgeOrBlockCustomer`**

Current opening (line 178):

```haskell
nudgeOrBlockCustomer riderConfig customer = do
  unless (riderConfig.enableCustomerCancellationRateBlocking == Just True) $ do
    logDebug $ "Customer cancellation rate blocking is disabled for city: " <> customer.merchantOperatingCityId.getId
    return ()
```

Insert a new guard immediately after the `do` of the function body, before the `unless ... enableCustomerCancellationRateBlocking` line:

```haskell
nudgeOrBlockCustomer riderConfig customer = do
  if SharedLogic.PersonBlock.isNoBlockUser customer
    then pure ()
    else do
      unless (riderConfig.enableCustomerCancellationRateBlocking == Just True) $ do
        logDebug $ "Customer cancellation rate blocking is disabled for city: " <> customer.merchantOperatingCityId.getId
        return ()
      -- ... rest of existing body indented one level deeper
```

Note: the entire existing body must be re-indented inside the `else do` branch. If preferred for diff hygiene, an equivalent guard using early `when` won't work here because the existing function uses `unless ... return ()` pattern (which does **not** early-return — it logs and falls through). So the `if/else` re-indent is correct.

Alternative cleaner form using `Kernel.Prelude.when`:

```haskell
nudgeOrBlockCustomer riderConfig customer
  | SharedLogic.PersonBlock.isNoBlockUser customer = pure ()
  | otherwise = do
      unless (riderConfig.enableCustomerCancellationRateBlocking == Just True) $ do
        logDebug $ "Customer cancellation rate blocking is disabled for city: " <> customer.merchantOperatingCityId.getId
        return ()
      -- ... rest of existing body unchanged
```

Use the guard form — no re-indent required for the existing body.

- [ ] **Step 3.4: Build to verify**

```bash
cabal build rider-app
```

Expected: builds cleanly.

- [ ] **Step 3.5: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/SharedLogic/BehaviourManagement/CustomerCancellationRate.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: NO_BLOCK_USER bypass for cancellation-rate block path"
```

---

## Task 4: Gate auth-fraud `fork` block in `Registration.auth`

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs:361-368`

The auth-fraud branch lives inside a `fork "Fraud Auth Check Processing"` block. The `person` value is already bound from lines 335-346. Wrap the body in an `unless isNoBlockUser` guard.

- [ ] **Step 4.1: Add import**

At the imports section, add:

```haskell
import qualified SharedLogic.PersonBlock
```

- [ ] **Step 4.2: Gate the `fork` body**

Find (lines 361-368):

```haskell
  fork "Fraud Auth Check Processing" $ do
    when (fromMaybe False person.authBlocked || maybe False (now >) person.blockedUntil) $ Person.updatingAuthEnabledAndBlockedState person.id Nothing (Just False) Nothing
    whenJust mbClientIP $ \clientIP -> do
      SMC.updateCustomerAuthCountersByIP clientIP merchantConfigs
      (isFraudDetected, mbMerchantConfigId) <- SMC.checkAuthFraudByIP merchantConfigs clientIP
      when isFraudDetected $ do
        whenJust mbMerchantConfigId $ \mcId ->
          SMC.blockCustomerByIP clientIP (Just mcId) riderConfig.blockedUntilInMins
```

Replace with:

```haskell
  fork "Fraud Auth Check Processing" $
    unless (SharedLogic.PersonBlock.isNoBlockUser person) $ do
      when (fromMaybe False person.authBlocked || maybe False (now >) person.blockedUntil) $ Person.updatingAuthEnabledAndBlockedState person.id Nothing (Just False) Nothing
      whenJust mbClientIP $ \clientIP -> do
        SMC.updateCustomerAuthCountersByIP clientIP merchantConfigs
        (isFraudDetected, mbMerchantConfigId) <- SMC.checkAuthFraudByIP merchantConfigs clientIP
        when isFraudDetected $ do
          whenJust mbMerchantConfigId $ \mcId ->
            SMC.blockCustomerByIP clientIP (Just mcId) riderConfig.blockedUntilInMins
```

Note: this also skips the *counter increment* (`updateCustomerAuthCountersByIP`) for tagged users. This is intentional — a whitelisted user's failed-OTP attempts should not push their IP toward block status either.

**Known limitation (do not fix here):** the `isIPBlocked` check at lines 320-321 fires *before* `person` is resolved. If a tagged user's IP gets blocked by someone else sharing the IP, the tagged user will still hit `IpHitsLimitExceeded` until the Redis TTL expires. Documented in spec; out of scope.

**Note on sink-level gating for `blockCustomerByIP`:** The spec lists `SMC.blockCustomerByIP` as a sink to gate. That function's signature is `Text -> Maybe (Id DMC.MerchantConfig) -> Maybe Minutes -> m ()` — IP only, no Person — so a tag check inside the sink would require adding a `Maybe Person.Person` parameter just for this check. Since `blockCustomerByIP` has exactly one caller (the `fork` body wrapped in Step 4.2), the caller-level gate is functionally equivalent and avoids a signature change with no current value. If a second caller is added later, gate it the same way at its call site (or refactor `blockCustomerByIP` then).

- [ ] **Step 4.3: Build to verify**

```bash
cabal build rider-app
```

- [ ] **Step 4.4: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: NO_BLOCK_USER bypass for auth/IP fraud path"
```

---

## Task 5: Pre-check in Search.hs `fraudCheck`

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs:656-661`

This is the optimisation pre-check from spec §5(a). Correctness is already held by Task 2's sink gate; this just skips the redis counter increment and Clickhouse fraud query when the user is whitelisted.

- [ ] **Step 5.1: Add import**

At the imports section:

```haskell
import qualified SharedLogic.PersonBlock
```

- [ ] **Step 5.2: Wrap `fraudCheck` body**

Find:
```haskell
    fraudCheck :: SearchRequestFlow m r => DPerson.Person -> DMOC.MerchantOperatingCity -> SearchRequest.SearchRequest -> m ()
    fraudCheck person merchantOperatingCity searchRequest = do
      SMC.updateSearchFraudCounters person.id merchantConfigs
      mFraudDetected <- SMC.anyFraudDetected person.id merchantOperatingCity.id merchantConfigs (Just searchRequest)
      whenJust mFraudDetected $ \mc -> SMC.blockCustomer person (Just mc.id)
```

(Note: line 661 was already updated to `person` instead of `person.id` in Task 2.)

Replace with:
```haskell
    fraudCheck :: SearchRequestFlow m r => DPerson.Person -> DMOC.MerchantOperatingCity -> SearchRequest.SearchRequest -> m ()
    fraudCheck person merchantOperatingCity searchRequest =
      unless (SharedLogic.PersonBlock.isNoBlockUser person) $ do
        SMC.updateSearchFraudCounters person.id merchantConfigs
        mFraudDetected <- SMC.anyFraudDetected person.id merchantOperatingCity.id merchantConfigs (Just searchRequest)
        whenJust mFraudDetected $ \mc -> SMC.blockCustomer person (Just mc.id)
```

- [ ] **Step 5.3: Build**

```bash
cabal build rider-app
```

- [ ] **Step 5.4: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: skip search fraud check for NO_BLOCK_USER tagged riders"
```

---

## Task 6: Read-side guard in `AadhaarVerification`

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/AadhaarVerification.hs:58,92`

These two sites read `person.blocked` and throw `InternalError "Person Account is Blocked"`. They don't go through any sink, so we belt-and-suspenders the read.

- [ ] **Step 6.1: Add import**

```haskell
import qualified SharedLogic.PersonBlock
```

- [ ] **Step 6.2: Update both blocked-checks**

Find at line 58:
```haskell
  when person.blocked $ throwError (InternalError "Person Account is Blocked")
```

Replace with:
```haskell
  when (person.blocked && not (SharedLogic.PersonBlock.isNoBlockUser person)) $ throwError (InternalError "Person Account is Blocked")
```

Find at line 92:
```haskell
  when (person.blocked) $ throwError (InternalError "Person Account Blocked")
```

Replace with:
```haskell
  when (person.blocked && not (SharedLogic.PersonBlock.isNoBlockUser person)) $ throwError (InternalError "Person Account Blocked")
```

- [ ] **Step 6.3: Build**

```bash
cabal build rider-app
```

- [ ] **Step 6.4: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/AadhaarVerification.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: NO_BLOCK_USER bypass for Aadhaar verification blocked check"
```

---

## Task 7: Read-side guard at Registration login-deny

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs:419`

The auth flow denies login at line 419 via `if (fromMaybe False req.allowBlockedUserLogin) || not person.blocked`. We need the tagged-user case to be allowed through.

- [ ] **Step 7.1: Update line 419 conditional**

Find:
```haskell
  if (fromMaybe False req.allowBlockedUserLogin) || not person.blocked
```

Replace with:
```haskell
  if (fromMaybe False req.allowBlockedUserLogin) || not person.blocked || SharedLogic.PersonBlock.isNoBlockUser person
```

(The `SharedLogic.PersonBlock` import was already added in Task 4.)

- [ ] **Step 7.2: Build**

```bash
cabal build rider-app
```

- [ ] **Step 7.3: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: allow login for NO_BLOCK_USER tagged riders despite blocked flag"
```

---

## Task 8: Dashboard handler — explicit error

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/Customer.hs:101`

The operator sees a clear error when trying to block a tagged user. The underlying `SMC.blockCustomer` already silently no-ops (Task 2), but we want the dashboard call to fail loudly.

- [ ] **Step 8.1: Add import**

At the imports section in `Domain/Action/Dashboard/Customer.hs`:

```haskell
import qualified SharedLogic.PersonBlock
```

- [ ] **Step 8.2: Call `assertCanBlock` before `SMC.blockCustomer`**

Find (around line 100-103):
```haskell
  unless (merchant.id == merchantId && customer.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist personId.getId)

  SMC.blockCustomer customer Nothing
  logTagInfo "dashboard -> blockCustomer : " (show personId)
```

Replace with:
```haskell
  unless (merchant.id == merchantId && customer.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist personId.getId)

  SharedLogic.PersonBlock.assertCanBlock customer
  SMC.blockCustomer customer Nothing
  logTagInfo "dashboard -> blockCustomer : " (show personId)
```

- [ ] **Step 8.3: Build**

```bash
cabal build rider-app
```

- [ ] **Step 8.4: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/Customer.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: dashboard block returns PersonHasNoBlockUserTag for tagged riders"
```

---

## Task 9: Auto-unblock on `NO_BLOCK_USER` tag-add

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/NammaTag.hs:525-545`

The handler `postNammaTagUpdateCustomerTag` already updates `customerNammaTags`. After a successful add of `NO_BLOCK_USER`, clear the three Person block-state fields.

- [ ] **Step 9.1: Add imports**

At the imports section in `Domain/Action/Dashboard/NammaTag.hs`, add:

```haskell
import qualified SharedLogic.PersonBlock
import qualified Storage.Queries.PersonExtra as QPExtra
```

(Check whether `QPExtra` is already imported under another alias — many dashboard files import it as `QPersonExtra` or directly via `Storage.Queries.Person`. Use whatever alias is already established in the file to avoid clashes; the function names referenced below are `updatingEnabledAndBlockedState`, `updatingBlockedStateWithUntil`, `updatingAuthEnabledAndBlockedState`.)

- [ ] **Step 9.2: Add auto-unblock hook**

Find (lines 537-545):

```haskell
  let tag =
        if req.isAddingTag
          then do
            let reqCustomerTagWithExpiry = LYTU.addTagExpiry req.tag (mbNammTag >>= (.validity)) now
            LYTU.replaceTagNameValue person.customerNammaTags reqCustomerTagWithExpiry
          else LYTU.removeTagNameValue person.customerNammaTags req.tag
  unless (Just (LYTU.showRawTags tag) == (LYTU.showRawTags <$> person.customerNammaTags)) $
    CQPerson.updateCustomerTags (Just tag) personId
  pure Success
```

Replace with:

```haskell
  let tag =
        if req.isAddingTag
          then do
            let reqCustomerTagWithExpiry = LYTU.addTagExpiry req.tag (mbNammTag >>= (.validity)) now
            LYTU.replaceTagNameValue person.customerNammaTags reqCustomerTagWithExpiry
          else LYTU.removeTagNameValue person.customerNammaTags req.tag
  unless (Just (LYTU.showRawTags tag) == (LYTU.showRawTags <$> person.customerNammaTags)) $
    CQPerson.updateCustomerTags (Just tag) personId
  when (req.isAddingTag && tagNameMatchesNoBlock req.tag) $ do
    void $ QPExtra.updatingEnabledAndBlockedState personId Nothing False
    void $ QPExtra.updatingBlockedStateWithUntil personId Nothing False Nothing
    void $ QPExtra.updatingAuthEnabledAndBlockedState personId Nothing (Just False) Nothing
    logInfo $ "NO_BLOCK_USER tag added for personId=" <> personId.getId <> "; auto-unblocked"
  pure Success
  where
    tagNameMatchesNoBlock :: LYT.TagNameValue -> Bool
    tagNameMatchesNoBlock t =
      case T.splitOn "#" (LYT.getTagNameValue t) of
        (name : _) -> LYT.TagName name == SharedLogic.PersonBlock.noBlockTagName
        _ -> False
```

Make sure `import qualified Data.Text as T` and `import qualified Lib.Yudhishthira.Types as LYT` are present (they typically already are in this file — confirm by reading the imports list).

**On the three storage calls:** they correspond to the three blocked-state fields touched across the sinks. Each is idempotent and the row-update style used throughout `PersonExtra.hs`. Argument order is the existing convention — verify against the actual signatures in `Storage/Queries/PersonExtra.hs` before running. If a signature differs (e.g., requires `Maybe (Id MerchantOperatingCity)` rather than `Nothing`), pass `(Just person.merchantOperatingCityId)`.

- [ ] **Step 9.3: Build**

```bash
cabal build rider-app
```

Expected: builds cleanly. If type errors point at signature mismatches on `updatingEnabledAndBlockedState` etc., open `Backend/app/rider-platform/rider-app/Main/src/Storage/Queries/PersonExtra.hs` (note: not `src-read-only/`) or `Backend/app/rider-platform/rider-app/Main/src-read-only/Storage/Queries/PersonExtra.hs`, and align argument types.

- [ ] **Step 9.4: Commit**

```bash
git -C /home/shailesh/Desktop/Github/nammayatri add \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/NammaTag.hs
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/feat: auto-unblock rider when NO_BLOCK_USER tag is added"
```

---

## Task 10: Integration tests

**Files:**
- Create: integration test scenarios (location depends on test harness — see Step 10.1)

This task uses the project's integration test framework. Refer to `Backend/.cursor/docs/17-testing-framework.md` and the `extend-testing` / `run-tests` skills if needed.

If the existing harness covers rider-app, add cases under whatever directory holds rider-app integration tests. If no rider-app integration tests exist on this branch, document the manual verification matrix below in a markdown file at `Backend/app/rider-platform/rider-app/test/manual/no-block-user.md` and verify by hand.

- [ ] **Step 10.1: Locate the integration test directory**

Run:
```bash
find /home/shailesh/Desktop/Github/nammayatri/Backend -type d -name "test" -path "*rider*" 2>/dev/null
find /home/shailesh/Desktop/Github/nammayatri/Backend -type d -name "Spec" -path "*rider*" 2>/dev/null
find /home/shailesh/Desktop/Github/nammayatri -type d -name "integration-tests" 2>/dev/null
```

If a target exists, follow the existing structure (HSpec or a custom collection-runner). Otherwise proceed with the manual matrix.

- [ ] **Step 10.2: Write the test matrix**

For each row below, set up the precondition, run the action, and assert the expected result.

| # | Precondition | Action | Expected |
|---|---|---|---|
| 1 | Rider with cancellation rate above daily threshold; no tag | `nudgeOrBlockCustomer` runs | `Person.blocked == true`, `blockedUntil` set |
| 2 | Same rider, plus `NO_BLOCK_USER#Yes` tag in `customerNammaTags` | `nudgeOrBlockCustomer` runs | `Person.blocked == false` |
| 3 | Rider with fraud-search count above threshold; no tag | `fraudCheck` runs (via search) | `Person.blocked == true` |
| 4 | Same rider, plus tag | `fraudCheck` runs | `Person.blocked == false`, no counter increment |
| 5 | Rider attempting failed OTPs over auth-fraud window; no tag | Subsequent auth | `IpHitsLimitExceeded` thrown; Redis key `Customer:IPBlocked:<IP>` set |
| 6 | Same rider, plus tag | Subsequent auth | no `IpHitsLimitExceeded`; no Redis key set |
| 7 | Untagged rider; dashboard `postCustomerBlock` called | — | Returns Success; `Person.blocked == true` |
| 8 | Tagged rider; dashboard `postCustomerBlock` called | — | HTTP 400 with `PERSON_HAS_NO_BLOCK_USER_TAG` |
| 9 | Already-blocked rider (`blocked=true`, `blockedUntil=future`); dashboard adds `NO_BLOCK_USER` tag via `postNammaTagUpdateCustomerTag` | — | `Person.blocked == false`, `blockedUntil == null`, `authBlocked == null` (or false) |
| 10 | Tagged rider with `blocked=true` in DB; Aadhaar OTP generate request | `generateAadhaarOtp` | Succeeds (does not throw `InternalError "Person Account is Blocked"`) |
| 11 | Tagged rider with `blocked=true` in DB; login auth | `auth` | Returns `AuthRes` (does not deny via line 419) |
| 12 | Untagged rider; verify all 11 scenarios with tag absent | — | Existing behaviour preserved |

- [ ] **Step 10.3: Run the integration suite if applicable**

If the harness invocation is via the project's nix shell:
```bash
cd /home/shailesh/Desktop/Github/nammayatri/Backend && , run-tests
```

(Or use the `run-tests` skill.)

Expected: all rider-app integration tests pass.

- [ ] **Step 10.4: Commit**

If test files were added:
```bash
git -C /home/shailesh/Desktop/Github/nammayatri add <added test paths>
git -C /home/shailesh/Desktop/Github/nammayatri commit -m "rider/test: NO_BLOCK_USER bypass coverage across block paths"
```

---

## Task 11: Final verification

- [ ] **Step 11.1: Full build**

```bash
cd /home/shailesh/Desktop/Github/nammayatri/Backend && cabal build all
```

Expected: builds cleanly with `-Werror`. Pay attention to "Defined but not used" warnings on `noBlockTagName` if Task 9's `tagNameMatchesNoBlock` was written to bypass it.

- [ ] **Step 11.2: Pre-commit hooks**

Hooks run automatically on each commit, but if any commits skipped (network blip), run:

```bash
cd /home/shailesh/Desktop/Github/nammayatri && pre-commit run --files \
  Backend/app/rider-platform/rider-app/Main/src/SharedLogic/PersonBlock.hs \
  Backend/app/rider-platform/rider-app/Main/src/Tools/Error.hs \
  Backend/app/rider-platform/rider-app/Main/src/SharedLogic/MerchantConfig.hs \
  Backend/app/rider-platform/rider-app/Main/src/SharedLogic/BehaviourManagement/CustomerCancellationRate.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Registration.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/AadhaarVerification.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/Customer.hs \
  Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/NammaTag.hs
```

Expected: all hooks pass.

- [ ] **Step 11.3: Self-check against spec**

Open `docs/superpowers/specs/2026-05-18-no-block-user-tag-design.md` and confirm:

- File-change inventory matches the commits in this plan (`git log --oneline` since the branch base).
- Tests cover all six "Testing" scenarios from the spec.
- Logging policy: only one INFO line in the codebase (Task 9 auto-unblock); zero log lines added at any sink-bypass site.

---

## Out of scope (do not implement)

- Gating `customerAuthBlock` (currently dead code; no callers in `src/` or `src-read-only/`). Re-evaluate when it gets a caller.
- Tag-removal hook on `NO_BLOCK_USER` removal — natural fallthrough (future blocks become possible again).
- Driver-app equivalent block bypass.
- Migrating existing blocked users in bulk (auto-unblock fires only at tag-add time, by spec design).
