# NO_BLOCK_USER Tag — Whitelist Riders From All Block Paths

**Date:** 2026-05-18
**Owner:** Shailesh Gahlawat
**Scope:** rider-app (BAP) only
**Status:** Implemented on branch `rider/feat/no-block-user-tag`. See "Implemented Architecture" note below.

## Revised after review (supersedes everything below)

Following review feedback (gate where blocks are *created*, don't react to blocked-state on every read path), the final shipped design is **narrower** than the rest of this document describes:

- **Kept:** the storage-level write guard in `Storage/Queries/PersonExtra.hs` (the three `updating*BlockedState*` helpers short-circuit every block write for tagged riders — this is the single source of truth and catches every caller, including `Beckn/Common.hs`), plus the fraud-entry early-exits in `Search.fraudCheck`, the auth-fraud `fork` in `Registration.auth`, and `CustomerCancellationRate.nudgeOrBlockCustomer`.
- **Removed:** the read-side guards in `AadhaarVerification.hs` and `Registration.hs` (login-deny and per-person auth-reject) — redundant once a tagged rider can never be written to a blocked state; the dashboard `assertCanBlock` + the `PersonHasNoBlockUserTag` (HTTP 400) error — a dashboard block on a tagged rider now silently no-ops via the storage guard (operator action is not special-cased); and the auto-unblock-on-tag-add hook in `NammaTag`.
- **Behavioural consequence:** the tag prevents *future* blocks but does **not** unblock a rider who was already blocked before tagging (clear an existing block via the dashboard unblock endpoint).

The sections below reflect the original, wider design and are retained for history only.

## Implemented Architecture (supersedes the originally drafted §Architecture)

The shipped design **gates at the lowest-level write helpers** in `Storage/Queries/PersonExtra.hs` (`updatingEnabledAndBlockedState`, `updatingBlockedStateWithUntil`, `updatingAuthEnabledAndBlockedState`) rather than at the SMC sink layer. The storage helpers already fetch the `Person` row at the top to compute `blockedCount`, so the `isNoBlockUser` check has zero extra DB cost. Every existing caller (and any future caller) of those three helpers inherits the bypass automatically — no signature changes propagate.

This replaced the originally drafted sink-level refactor of `SMC.blockCustomer` and `blockCustomerTemporarily` after the first implementation attempt found two missed callers in `Domain/Action/Beckn/Common.hs` (line 1330, 1524, inside `cancellationTransaction`). The deeper guard subsumes them and any future ones.

Retained alongside the storage guard (see "Revised after review" above for what was dropped):
- Auth-fraud `fork` in `Registration.auth` is gated at the caller (the IP-keyed Redis block isn't a Person row write, so the storage guard can't catch it).
- Pre-check optimizations at `nudgeOrBlockCustomer` and `fraudCheck` (skips Redis counters + Clickhouse work for tagged users).

## Problem

The rider-app has several independent paths that flip `Person.blocked = true`:

1. Cancellation-rate auto-block (`SharedLogic/BehaviourManagement/CustomerCancellationRate.hs`)
2. Search/booking fraud auto-block (`SharedLogic/MerchantConfig.hs` ← `Domain/Action/UI/Search.hs`)
3. Auth/IP fraud login block (`Domain/Action/UI/Registration.hs`)
4. Dashboard operator manual block (`Domain/Action/Dashboard/Customer.hs`)
5. Device-token fraud block at registration (`Domain/Action/UI/Registration.hs`)

Operations needs a way to mark certain riders (e.g., VIPs, internal QA accounts, partner test users) as exempt from *all* of these so that no automatic or manual block can take effect against them.

## Goal

Introduce a single tag, `NO_BLOCK_USER`, that when present in `Person.customerNammaTags`:

- Prevents any of the above paths from flipping `blocked`, `authBlocked`, or writing the IP-block Redis key for that rider.
- Causes the dashboard manual-block API to return an explicit, operator-facing error.
- Auto-unblocks an already-blocked rider when the tag is added via the dashboard.

## Non-goals

- Driver-side blocking. Out of scope.
- Route-level `isBlockedRoute` (this is BPP-returned route metadata, not a rider block).
- Introducing a new fare-amount-threshold block. (User confirmed this does not exist today; not part of this change.)
- Migrating existing blocked users in bulk. Auto-unblock fires only on tag-add events going forward.

## Tag semantics

- Name: `NO_BLOCK_USER`.
- **Presence-only check**: any tag whose parsed `tagName == "NO_BLOCK_USER"` whitelists the rider. The tag *value* and *expiry* are ignored. Rationale: simplest contract for operators; no implicit re-blocking when an expiry passes.
- Reuses the existing `customerNammaTags :: Maybe [TagNameValueExpiry]` field on `Person`. No schema change.

## Architecture

The design separates two concerns:

1. **Silent bypass at the lowest write sinks** — every function that flips `blocked = true` checks the tag and no-ops if set. This is the scalable safety net: any future block path that goes through these sinks inherits the protection automatically.
2. **Explicit error at the dashboard handler** — the operator-facing API performs an up-front check and throws a typed error so the dashboard UI can surface a clear message.

Auto-block call sites (cancellation evaluator, fraud check) additionally short-circuit at the top to avoid pointless work, but they are not load-bearing for correctness.

## Components

### 1. New helper module

File: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/PersonBlock.hs`

```haskell
module SharedLogic.PersonBlock
  ( isNoBlockUser
  , assertCanBlock
  , noBlockTagName
  ) where

import qualified Domain.Types.Person as DPerson
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Kernel.Prelude
import Kernel.Utils.Common  -- throwError, Log

noBlockTagName :: LYT.TagName
noBlockTagName = LYT.TagName "NO_BLOCK_USER"

-- Pure: name-only match. Value and expiry are ignored.
isNoBlockUser :: DPerson.Person -> Bool
isNoBlockUser person =
  case person.customerNammaTags of
    Nothing -> False
    Just tags -> any (matchesNoBlockName) tags

-- Throws a typed error if the person carries the tag.
-- Used only by the dashboard manual-block handler.
assertCanBlock :: (MonadThrow m, Log m) => DPerson.Person -> m ()
assertCanBlock person =
  when (isNoBlockUser person) $
    throwError (PersonHasNoBlockUserTag person.id.getId)
```

`matchesNoBlockName` extracts the name segment of `TagNameValueExpiry` (using the existing `LYTU.parseTag`/`LYTU.removeTagExpiry` utilities) and compares against `noBlockTagName`. Detailed implementation is left to the implementation plan; the contract is: returns True iff some tag's name equals `"NO_BLOCK_USER"`.

### 2. New error type

Extend the existing `CustomerError` sum in `Main/src/Tools/Error.hs` with a new constructor `PersonHasNoBlockUserTag Text` (the `Text` carries the offending `personId`).

- HTTP code: **400 Bad Request**
- Message: `"Cannot block person <personId>: NO_BLOCK_USER tag is set. Remove the tag first."`
- Extend the existing `IsBaseError` / `IsHTTPError` / `IsAPIError` instances for `CustomerError` with the new constructor — match the existing pattern used for `PersonMobileAlreadyExists` and `DeviceTokenNotFound` in the same file.

### 3. Sink-level silent bypass

Each of the four write sinks below adds, near the top:

```haskell
if isNoBlockUser person
  then pure ()  -- whitelisted; silently skip
  else <existing block logic>
```

No log line on this path (per the volume constraint — see "Logging policy").

| Sink | File | Edit |
|---|---|---|
| `SMC.blockCustomer` | `Main/src/SharedLogic/MerchantConfig.hs` | Fetch `person` (already available in callers; if not, pass it in), gate body on `not (isNoBlockUser person)` |
| `SMC.blockCustomerByIP` | `Main/src/SharedLogic/MerchantConfig.hs` | Same gate. **Also skips the `Customer:IPBlocked:<IP>` Redis write** — a tagged user shouldn't cause their IP to be blocked for everyone else. |
| `blockCustomerTemporarily` | `Main/src/SharedLogic/BehaviourManagement/CustomerCancellationRate.hs` | Same gate |
| Device-token block at registration | `Main/src/Domain/Action/UI/Registration.hs:~949` | Same gate. In practice a brand-new account has no tags so this is dormant, but it's added for symmetry and future-proofing. |

Signature change: where a sink today only receives `personId`, refactor to also receive (or fetch) the `Person` record so the tag check is possible. Choose the option that minimises caller changes per sink.

### 4. Dashboard handler — explicit error

File: `Main/src/Domain/Action/Dashboard/Customer.hs:~101-127`

`blockCustomer` calls `assertCanBlock person` before invoking `SMC.blockCustomer`. The operator receives `PersonHasNoBlockUserTag` (HTTP 400) instead of an opaque success.

The underlying `SMC.blockCustomer` would have silently no-op'd anyway, but the explicit pre-check turns the operator-visible behaviour into a clear failure rather than a silent success.

### 5. Auto-block call-site pre-checks

Two flavours of pre-check, depending on whether the call site relies on the sink to provide coverage:

**(a) Optimisation pre-checks** — at the top of:

- `nudgeOrBlockCustomer` in `CustomerCancellationRate.hs`
- The fraud-check entry point invoked from `Domain/Action/UI/Search.hs:~660-661` (`MerchantConfig.fraudCheck` / `anyFraudDetected`)

```haskell
if isNoBlockUser person
  then pure ()  -- skip the entire evaluation
  else <existing evaluator>
```

Pure optimisation; correctness is held by the sink-level gate.

**(b) Required pre-check at `checkAuthFraudByIP`** — `Domain/Action/UI/Registration.hs:~365-368`. The auth-fraud flow performs *two* writes: `SMC.blockCustomerByIP` (covered by the sink gate) **and** a direct call to `QPExtra.updatingAuthEnabledAndBlockedState` (not behind any of the four sinks). To keep `authBlocked` from flipping, wrap the whole `checkAuthFraudByIP` body in `unless (isNoBlockUser person)`. Treat this as part of the contract, not an optimisation.

### 6. Auto-unblock when tag is added

File: `Main/src/Domain/Action/Dashboard/NammaTag.hs:~531-543` (the existing `handleCustomerTagAddition` flow that mutates `customerNammaTags`).

After the tag write succeeds, if the tag just added matches `noBlockTagName` *and* `req.isAddingTag == True`:

1. Call `QPExtra.updatingEnabledAndBlockedState person.id person.merchantOperatingCityId True False` (enable, unblock).
2. Call `QPExtra.updatingBlockedStateWithUntil person.id False Nothing` (clear `blockedUntil`).
3. Call `QPExtra.updatingAuthEnabledAndBlockedState person.id person.merchantOperatingCityId True False Nothing` (clear auth-block).
4. Log a single INFO line: `"NO_BLOCK_USER tag added for <personId>; auto-unblocked"`. This is one of the few permitted log lines (see "Logging policy").

Scheduled `UnblockCustomer` jobs are left intact — they will fire and no-op because `blocked == false` already.

### 7. Read-side belt-and-suspenders

Two sites read `person.blocked` directly to deny service and do not pass through the sinks:

- `Main/src/Domain/Action/UI/Registration.hs:419` (login deny)
- `Main/src/Domain/Action/UI/AadhaarVerification.hs:58, 92`

Add `&& not (isNoBlockUser person)` to each `blocked` check. Covers any case where the DB still has `blocked = true` but the tag has been added (e.g., out-of-band data fix, race with auto-unblock).

## Logging policy

Log volume must not increase materially. Rules:

- **Sink-level bypass**: no log. Bypass is the common path for whitelisted users and would dwarf normal traffic on every search.
- **Auto-block pre-check skip**: no log.
- **Auto-unblock on tag add**: log INFO **once** per tag-add event. Low frequency.
- **Dashboard error path**: no extra log; the thrown error is itself observable via standard error telemetry.
- **Read-side guard hits**: no log.

Net: at most one log line per tag-add and zero per normal request.

## Backward compatibility

| Aspect | Impact |
|---|---|
| Untagged users | Zero behavioural change. |
| Tagged users — rider flows (search/login/cancel) | Silent bypass. No new error surfaces. |
| Tagged users — already blocked when tagged | Auto-unblocked on next tag-add event. No DB migration. |
| Dashboard operators | New error `PersonHasNoBlockUserTag` (HTTP 400) when blocking a tagged user. Existing operators not touching tagged users see no change. |
| Existing API contracts | Additive only; new error code in the union. Dashboard UI may want to display the message text. |
| Schema / generated code | None. `customerNammaTags` already exists; no `src-read-only/` edits. |
| New block paths in the future | Calling any of the four sinks inherits protection automatically. A brand-new sink would have to add the `isNoBlockUser` check explicitly — same hygiene as any new block code. |

## Risks

1. **A future sink author bypasses `isNoBlockUser`**. Mitigation: the four canonical sinks are the conventional path; a code review on PRs that introduce a new write to `Person.blocked` catches it. Optionally a `grep`-able hpc lint comment can be added near the `blocked` field, but not in scope.
2. **IP-shared with a real attacker**. We skip the Redis IP write when a tagged user trips the auth-fraud counter, which could allow a co-located attacker to evade rate limiting. Accepted trade-off: per user direction, the trusted account's traffic must not block their IP.
3. **Race between tag-add auto-unblock and a concurrent auto-block**. The sink-level gate ensures the post-tag block can't take effect even if it fires after the tag is added; the tag-add unblock is the catch-up step.

## Testing

Unit tests (`isNoBlockUser`):

- Empty tags → False.
- Tag list with unrelated names → False.
- Tag list containing `NO_BLOCK_USER#Yes` → True.
- Tag list containing `NO_BLOCK_USER#No` → True (value ignored).
- Tag list containing an *expired* `NO_BLOCK_USER` entry → True (expiry ignored).

Integration tests:

1. **Cancellation block bypass** — set a rider's cancellation rate above the daily threshold, add the tag, run `nudgeOrBlockCustomer`; assert `blocked == false` after.
2. **Fraud-search block bypass** — drive `fraudSearchCountWindow` over threshold for a tagged rider; assert search still succeeds and `blocked == false`.
3. **Auth/IP block bypass** — exceed the auth-fraud counter for a tagged rider; assert `authBlocked == false`, no `Customer:IPBlocked:<IP>` Redis key written.
4. **Dashboard block returns error** — call dashboard `blockCustomer` for a tagged rider; assert response is `PersonHasNoBlockUserTag` (HTTP 400).
5. **Tag-add auto-unblock** — start with a blocked rider, add the tag; assert `blocked = false`, `blockedUntil = null`, `authBlocked = false`.
6. **Untagged baseline** — repeat each scenario without the tag; assert existing block behaviour is preserved.

## File-change inventory

| File | Change |
|---|---|
| `Main/src/SharedLogic/PersonBlock.hs` | **New.** Helper module. |
| `Main/src/SharedLogic/MerchantConfig.hs` | Gate `blockCustomer` and `blockCustomerByIP`; skip IP-Redis write for tagged users. |
| `Main/src/SharedLogic/BehaviourManagement/CustomerCancellationRate.hs` | Gate `blockCustomerTemporarily`; add pre-check in `nudgeOrBlockCustomer`. |
| `Main/src/Domain/Action/UI/Registration.hs` | Gate device-token block at `~949`; wrap `checkAuthFraudByIP` body at `~365-368` in `unless isNoBlockUser`; add `not (isNoBlockUser person)` to login-deny check at `419`. |
| `Main/src/Domain/Action/UI/Search.hs` | Pre-check in fraud entry point. |
| `Main/src/Domain/Action/UI/AadhaarVerification.hs` | Add `not (isNoBlockUser person)` to both blocked checks. |
| `Main/src/Domain/Action/Dashboard/Customer.hs` | Call `assertCanBlock` before `SMC.blockCustomer`. |
| `Main/src/Domain/Action/Dashboard/NammaTag.hs` | On `NO_BLOCK_USER` tag-add, call the three Person-state clear queries; log once. |
| `Main/src/Tools/Error.hs` | Extend `CustomerError` with `PersonHasNoBlockUserTag Text` + instance arms. |
| Tests directory | Unit + integration tests above. |

No edits to `src-read-only/`, no YAML spec changes, no DB migration.

## Out of scope (deferred)

- Tag-removal hook: when the `NO_BLOCK_USER` tag is removed, no special action is taken (future blocks become possible naturally). If operations later want a re-evaluation pass at removal time, that's a separate change.
- Per-block-category tag granularity (e.g., `NO_CANCEL_BLOCK_USER`). Single coarse switch chosen for simplicity.
- Driver-side equivalent. Separate audit and design.
