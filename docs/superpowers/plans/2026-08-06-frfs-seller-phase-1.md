# FRFS Seller (Metro BPP) — Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stand up rider-app as an `ONDC:TRV11` **seller** that answers `search` with a signed `on_search` callback for Chennai metro, using cached station data and no operator call.

**Architecture:** rider-app already carries both halves of the FRFS Beckn spec but mounts only the buyer (`on_*`) half. This phase mounts the seller `search` endpoint under a new route tree, acknowledges synchronously, and does the work in a forked action that builds and POSTs `on_search` back to the buyer's `bap_uri`. It reuses `frfs_ticket_booking` / `frfs_ticket` / `FRFSConfig` — no new tables. Seller bookings carry a synthetic rider identity, following the `partnerOrgRiderId` precedent already in production.

**Tech Stack:** Haskell (GHC, `-Werror`), Servant, EulerHS `Flow`, NammaDSL codegen, Beam/Postgres, Hedis (Redis), Tasty/HUnit.

## Global Constraints

- **Never edit files under `src-read-only/`.** They are generated from `spec/**/*.yaml` by `, run-generator`. Change the YAML, regenerate.
- **`-Werror` is enabled.** An unused import, an incomplete pattern match, or a shadowed binding fails the build.
- **YAML constraint tags must be quoted**: write `fieldName: "!SecondaryKey"`, never bare `!SecondaryKey`. Unquoted, the YAML parser reads it as a tag and silently drops it, producing an empty `enableKVPG` list and intermittently-empty KV reads at runtime. A pre-commit hook enforces this.
- **Deny list for secondary keys**: `merchantId`, `merchantOperatingCityId`, `status` may never be secondary keys.
- **ONDC error codes are a published contract.** ~10 live buyer apps branch on the numeric `code`. Phase 1 emits only `30016` (invalid signature) and `31001` (generic). Do not invent new codes.
- **All commands run from `Backend/`** inside the nix shell (`ln -sf .envrc.backend .envrc && direnv allow` once, from repo root).
- Commit message format: `<sub-project>/<type>: <summary>` — e.g. `rider-app/feat: mount FRFS seller search route`.

---

## Scope

**In scope (Phase 1):** `search` → `on_search`, Chennai only, catalog served from cached station data. Signature verification inbound, signing outbound. The synthetic seller rider identity. One end-to-end proof that a real buyer app accepts our callback.

**Out of scope (later phases):** `select`/`init`/`confirm`/`status`/`cancel`, any operator (CMRL/KMRL) API call, Kochi entirely, settlement, grievances, the ops endpoints. Phase 1 deliberately makes no outbound operator call — it proves the protocol path in isolation.

**Why this order:** the riskiest unknowns (Kochi's gateway envelope, settlement wire protocol) land last, on a path already proven. Phase 1's only real risk is that the unmounted seller types don't serve — which Task 2 settles in an afternoon.

## Prior verification (already done — do not redo)

| Question | Answer | Where |
|---|---|---|
| Do the seller request types have `FromJSON`? | Yes, all six | `BecknV2/FRFS/Types.hs` |
| Do the `On*Req` types have `ToJSON`? | Yes, all six | same |
| Is `SignatureAuth 'Domain.PUBLIC_TRANSPORT` in production? | Yes | `API/FRFS.hs:34` |
| Can a booking exist with no real rider? | Yes — `partnerOrgRiderId = Id "partnerOrg_rider_id"` | `SharedLogic/FRFSUtils.hs:796` |
| Does a seller need new tables? | No | `FRFSConfig`, `frfs_ticket_booking`, `frfs_ticket` all reusable |

## File Structure

| File | Responsibility |
|---|---|
| `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller.hs` | Route tree for the seller: mounts `search` under `beckn/frfs-seller/v1` with signature auth. |
| `.../src/API/Beckn/FRFSSeller/Search.hs` | HTTP layer for `search`: decode, dedupe claim, ack, fork. |
| `.../src/Beckn/ACL/FRFSSeller/OnSearch.hs` | Pure builder: domain catalog → `Spec.OnSearchReq`. No IO. |
| `.../src/Domain/Action/Beckn/FRFSSeller/Search.hs` | The work: resolve stops, build the catalog, hand to the ACL, send. |
| `.../src/SharedLogic/FRFSSeller/Common.hs` | Seller identity constants (`sellerRiderId`) and shared helpers. |
| `.../src/SharedLogic/FRFSSeller/CallBAP.hs` | Outbound: sign and POST `on_search` to the buyer's `bap_uri`. |
| `.../src/API.hs:55-63` | Modify: add `FRFSSeller.API` to the top-level API type and handler. |

---

### Task 1: ~~Make the test suite runnable~~ — **DESCOPED 2026-08-09, do not implement**

**Attempted and reverted.** `hunit-tests` is absent from `Backend/cabal.project`, so `cabal build all` in CI has never compiled it. Registering it exposes pre-existing bitrot that is unrelated to this plan:

- `tasty-hspec` is declared in `package.yaml`, imported nowhere, and absent from the nix GHC package set.
- `mock-sms` (`app/mocks/sms`) is a real dependency also missing from `cabal.project`.
- Nine test modules (~3,200 lines) have drifted from the types they exercise — e.g. `DM.is2faMandatory` moved to `Environment.hs`; `RegisterRCReq` gained a `VehicleCategory` argument. They are **live**, not dead: `Dashboard.hs` is registered in `app/Main.hs:34` and imports all nine at `Dashboard.hs:15-24`, so GHC compiles them regardless of the cabal module list. Pruning cannot avoid them.

Fixing that is real work with real value — nobody can run this suite today — but it belongs to a separate ticket, not to the FRFS seller.

**Consequence for this plan:** there is no runnable unit-test harness. Tasks 3 and 5 verify by compilation (`-Werror` makes that a genuine gate) and by Task 8's end-to-end run. **Phase 1 therefore ships the pure `on_search` builder without unit tests** — a real weakness, recorded deliberately rather than hidden.

---

### Task 2: Prove the seller types serve — mount `search`, ack only

The load-bearing assumption of the whole migration is that `BecknV2.FRFS.APIs.SearchAPI` — declared but never mounted — can actually serve a request. This task settles it with the smallest possible change: a route that verifies the signature, logs, and returns `ack`. No catalog, no callback.

**Files:**
- Create: `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs`
- Create: `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller.hs`
- Modify: `Backend/app/rider-platform/rider-app/Main/src/API.hs`

**Interfaces:**
- Consumes: nothing (Task 1 descoped)
- Produces:
  - `API.Beckn.FRFSSeller.API` — the seller route tree, mountable in `API.hs`
  - `API.Beckn.FRFSSeller.handler :: FlowServer API`
  - `API.Beckn.FRFSSeller.Search.API` / `.handler`

- [ ] **Step 1: Write the search route module**

Create `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs`:

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Beckn.FRFSSeller.Search (API, handler) where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API = Spec.SearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = search

search :: SignatureAuthResult -> Spec.SearchReq -> FlowHandler Spec.AckResponse
search _authResult req = withFlowHandlerAPI $ do
  transactionId <-
    req.searchReqContext.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <-
    req.searchReqContext.contextMessageId
      & fromMaybeM (InvalidRequest "MessageId not found")
  logInfo $
    "FRFS seller search received: txn=" <> show transactionId
      <> " msg="
      <> show messageId
  pure Utils.ack
```

- [ ] **Step 2: Write the seller route tree**

Create `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller.hs`:

```haskell
module API.Beckn.FRFSSeller (API, handler) where

import qualified API.Beckn.FRFSSeller.Search as Search
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "beckn" :> "frfs-seller" :> "v1"
    :> SignatureAuth 'Domain.PUBLIC_TRANSPORT "Authorization"
    :> Search.API

handler :: FlowServer API
handler = Search.handler
```

- [ ] **Step 3: Mount it in the top-level API**

In `Backend/app/rider-platform/rider-app/Main/src/API.hs`:

Add the import alongside the other `API.Beckn` imports (near line 22):
```haskell
import qualified API.Beckn.FRFSSeller as BecknFRFSSeller
```

In the `type API = ...` block, add a line after `:<|> FRFS.APIM` (line 58):
```haskell
    :<|> BecknFRFSSeller.API
```

In the corresponding `handler = ...` block, add a line in the **same position** — after `:<|> FRFS.handlerM` (line 108):
```haskell
    :<|> BecknFRFSSeller.handler
```

> The `:<|>` order in the handler must match the type exactly. A mismatch produces a long Servant type error, not a helpful one.

- [ ] **Step 4: Build**

```bash
cabal build rider-app
```
Expected: compiles clean. If it fails with an unused-import warning, remove the import — `-Werror` treats it as an error.

- [ ] **Step 5: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller.hs \
        Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs \
        Backend/app/rider-platform/rider-app/Main/src/API.hs
git commit -m "rider-app/feat: mount FRFS seller search route, ack only"
```

---

### Task 3: The synthetic seller rider identity

Seller bookings have no `Person`. Production already solves this for partner-org bookings with a single synthetic id; this task adds the seller equivalent, in its own module so later tasks import one name.

> **Note:** nothing in Phase 1 writes a booking, so this constant is not yet *called* — Phase 2 uses it when `confirm` first persists a row. It lands here because it is the single decision the whole reuse strategy rests on (see the migration spec §3b), and a two-line module with three tests is the cheapest place to make it explicit and reviewable. If you would rather defer it, move it to Phase 2 wholesale; do not inline the literal at its call site later.

**Files:**
- Create: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSSeller/Common.hs`

**Interfaces:**
- Consumes: Task 1 (runnable tests)
- Produces:
  - `SharedLogic.FRFSSeller.Common.sellerRiderId :: Id DP.Person`
  - `SharedLogic.FRFSSeller.Common.isSellerRider :: Id DP.Person -> Bool`

> **Verification changed 2026-08-09 (Task 1 descoped):** there is no runnable test
> harness, so the original "write the failing test / watch it fail" steps are gone.
> This task now verifies by compilation. `-Werror` is on, so a type error, an unused
> import, or a missing export all fail the build — that is a real gate, just a weaker
> one than a test.

- [ ] **Step 1: Confirm the precedent you are copying**

Read the existing synthetic-rider constant so yours matches its shape and intent:

```bash
sed -n '790,800p' Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSUtils.hs
```
Expected: `partnerOrgRiderId :: Id DP.Person` / `partnerOrgRiderId = Id "partnerOrg_rider_id"`.

- [ ] **Step 2: Write the module**

Create `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSSeller/Common.hs`:

```haskell
module SharedLogic.FRFSSeller.Common
  ( sellerRiderId,
    isSellerRider,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id

-- | Synthetic rider identity for bookings this app SELLS rather than buys.
--
-- A seller has no 'Person': the passenger belongs to the buyer app, and we
-- only ever receive a phone number on init. This mirrors the existing
-- 'SharedLogic.FRFSUtils.partnerOrgRiderId' sentinel, which has served the
-- same purpose for partner-org bookings in production.
--
-- Safety: the two places that read 'riderId' for authorization both fail
-- closed against a sentinel — access checks compare it to the logged-in
-- 'personId' (never equal, so denied) and history queries filter BY riderId
-- (so seller rows never surface in a rider's history).
sellerRiderId :: Id DP.Person
sellerRiderId = Id "frfsSeller_rider_id"

isSellerRider :: Id DP.Person -> Bool
isSellerRider = (== sellerRiderId)
```

- [ ] **Step 3: Verify it compiles**

```bash
cabal build rider-app
```
Expected: compiles clean. This is a cold-ish build — see the note at the top of the plan about launching long builds.

- [ ] **Step 4: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSSeller/Common.hs
git commit -m "rider-app/feat: add synthetic seller rider identity"
```

---

### Task 4: Idempotency guard for inbound search

The ONDC gateway can deliver one `search` to more than one of our registered subscriber URLs. Each delivery would emit its own `on_search`, and buyer apps NACK the second as out-of-sequence. Claim the (transaction, message) pair in Redis; later deliveries ack without reprocessing. **Fail open** — a Redis outage must not stop us answering searches.

**Files:**
- Modify: `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs`

**Interfaces:**
- Consumes: Task 2 (`Search.handler`)
- Produces: `searchDedupeKey :: Text -> Text -> Text` (exported for the Task 5 test)

- [ ] **Step 1: Add the dedupe key function and guard**

Replace the body of `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs` with:

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Beckn.FRFSSeller.Search (API, handler, searchDedupeKey) where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API = Spec.SearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = search

-- | One claim per (transaction, message). The gateway may fan a single search
-- to several of our registered subscriber URIs; without this, each delivery
-- emits its own on_search and buyers NACK the duplicate as out-of-sequence.
searchDedupeKey :: Text -> Text -> Text
searchDedupeKey txnId msgId = "frfsSeller:search:" <> txnId <> ":" <> msgId

searchDedupeTtlSeconds :: Int
searchDedupeTtlSeconds = 60

search :: SignatureAuthResult -> Spec.SearchReq -> FlowHandler Spec.AckResponse
search _authResult req = withFlowHandlerAPI $ do
  transactionId <-
    req.searchReqContext.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <-
    req.searchReqContext.contextMessageId
      & fromMaybeM (InvalidRequest "MessageId not found")
  withTransactionIdLogTag' transactionId $ do
    -- Fail open: if Redis is unavailable we would rather emit a duplicate
    -- on_search than answer nothing at all.
    isFirst <-
      try @_ @SomeException
        ( Redis.withCrossAppRedis $
            Redis.setNxExpire (searchDedupeKey transactionId messageId) searchDedupeTtlSeconds True
        )
        >>= \case
          Right claimed -> pure claimed
          Left err -> do
            logWarning $ "FRFS seller search dedupe unavailable, processing anyway: " <> show err
            pure True
    if isFirst
      then logInfo $ "FRFS seller search accepted: msg=" <> messageId
      else logInfo $ "FRFS seller search duplicate ignored: msg=" <> messageId
    pure Utils.ack
```

- [ ] **Step 2: Build**

```bash
cabal build rider-app
```
Expected: compiles clean.

- [ ] **Step 3: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs
git commit -m "rider-app/feat: dedupe inbound FRFS seller search by txn+message"
```

---

### Task 5: The `on_search` builder (pure)

The callback payload is where seller work actually lives, and it is pure — domain values in, `Spec.OnSearchReq` out. Keeping it free of IO is what makes it testable without a database.

**Files:**
- Create: `Backend/app/rider-platform/rider-app/Main/src/Beckn/ACL/FRFSSeller/OnSearch.hs`

**Interfaces:**
- Consumes: Task 3 (`SellerCommon`)
- Produces:
  - `Beckn.ACL.FRFSSeller.OnSearch.SellerCatalog(..)` — `{ providerId, providerName, items :: [SellerItem] }`
  - `Beckn.ACL.FRFSSeller.OnSearch.SellerItem(..)` — `{ itemId, itemDescription, priceValue, currency, fromStopCode, toStopCode }`
  - `Beckn.ACL.FRFSSeller.OnSearch.buildOnSearchReq :: Spec.Context -> SellerCatalog -> Spec.OnSearchReq`
  - `Beckn.ACL.FRFSSeller.OnSearch.buildOnSearchErrorReq :: Spec.Context -> Text -> Text -> Spec.OnSearchReq`

> **Verification changed 2026-08-09 (Task 1 descoped):** there is no runnable test
> harness, so the original test-first steps are gone. This builder is pure — it is the
> single most testable unit in Phase 1, and shipping it untested is the real cost of
> descoping Task 1. Compensate two ways: (a) `-Werror` compilation, which catches every
> constructor and field-name error, and (b) Task 8, which exercises this exact code path
> end to end and prints the emitted payload. **When a test harness exists, this builder
> is the first thing that should get unit tests.**

- [ ] **Step 1: Write the builder**

Create `Backend/app/rider-platform/rider-app/Main/src/Beckn/ACL/FRFSSeller/OnSearch.hs`:

```haskell
module Beckn.ACL.FRFSSeller.OnSearch
  ( SellerCatalog (..),
    SellerItem (..),
    buildOnSearchReq,
    buildOnSearchErrorReq,
  )
where

import qualified BecknV2.FRFS.Types as Spec
import Kernel.Prelude

-- | One sellable journey option. Ids are opaque to the buyer and must round
-- trip unchanged on select/init/confirm.
data SellerItem = SellerItem
  { itemId :: Text,
    itemDescription :: Text,
    priceValue :: Text,
    currency :: Text,
    fromStopCode :: Text,
    toStopCode :: Text
  }
  deriving (Show, Eq)

data SellerCatalog = SellerCatalog
  { providerId :: Text,
    providerName :: Text,
    items :: [SellerItem]
  }
  deriving (Show, Eq)

-- | Turn the inbound search context into the outbound callback context:
-- same transaction and message, action flipped to on_search.
mkCallbackContext :: Spec.Context -> Spec.Context
mkCallbackContext ctx = ctx {Spec.contextAction = Just "on_search"}

buildOnSearchReq :: Spec.Context -> SellerCatalog -> Spec.OnSearchReq
buildOnSearchReq ctx catalog =
  Spec.OnSearchReq
    { onSearchReqContext = mkCallbackContext ctx,
      onSearchReqError = Nothing,
      onSearchReqMessage = Just (mkMessage catalog)
    }

buildOnSearchErrorReq :: Spec.Context -> Text -> Text -> Spec.OnSearchReq
buildOnSearchErrorReq ctx code message =
  Spec.OnSearchReq
    { onSearchReqContext = mkCallbackContext ctx,
      onSearchReqError =
        Just
          Spec.Error
            { errorCode = Just code,
              errorMessage = Just message,
              errorPaths = Nothing
            },
      onSearchReqMessage = Nothing
    }

mkMessage :: SellerCatalog -> Spec.OnSearchReqMessage
mkMessage catalog =
  Spec.OnSearchReqMessage
    { onSearchReqMessageCatalog =
        Spec.Catalog
          { catalogDescriptor = Nothing,
            catalogProviders = Just [mkProvider catalog],
            catalogTags = Nothing
          }
    }

mkProvider :: SellerCatalog -> Spec.Provider
mkProvider catalog =
  Spec.Provider
    { providerCategories = Nothing,
      providerDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Nothing,
              descriptorImages = Nothing,
              descriptorName = Just catalog.providerName
            },
      providerFulfillments = Nothing,
      providerId = Just catalog.providerId,
      providerItems = Just (map mkItem catalog.items),
      providerPayments = Nothing,
      providerTags = Nothing,
      providerTime = Nothing
    }

mkItem :: SellerItem -> Spec.Item
mkItem item =
  Spec.Item
    { itemCategoryIds = Nothing,
      itemDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just item.itemDescription,
              descriptorImages = Nothing,
              descriptorName = Just item.itemDescription
            },
      itemFulfillmentIds = Nothing,
      itemId = Just item.itemId,
      itemPrice =
        Just
          Spec.Price
            { priceCurrency = Just item.currency,
              priceValue = Just item.priceValue,
              priceOfferedValue = Nothing
            },
      itemQuantity = Nothing,
      itemTime = Nothing
    }
```

> Field names above are copied from `BecknV2/FRFS/Types.hs` (`OnSearchReqMessage:1160`, `Catalog:338`, `Provider:1462`, `Item:801`, `Descriptor:561`, `Price:1432`, `Error:615`). `fromStopCode`/`toStopCode` on `SellerItem` are carried for Phase 2, where they populate `providerFulfillments`; Phase 1 leaves fulfillments empty.

- [ ] **Step 2: Verify it compiles**

```bash
cabal build rider-app
```
Expected: compiles clean. Every field name and constructor above was copied from
`BecknV2/FRFS/Types.hs`, so a failure here means a genuine mismatch — read the error and
re-check the source rather than guessing at the field name.

- [ ] **Step 3: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/Beckn/ACL/FRFSSeller/OnSearch.hs
git commit -m "rider-app/feat: add FRFS seller on_search payload builder"
```

---

### Task 6: Send the callback

Signed outbound POST to the buyer's `bap_uri`. This is the piece with a genuine open decision: rider-app's own sender (`callBecknAPIWithSignature'`) has only ever been pointed at seller URLs, while driver-app's `Callback.withCallback` handles the seller→buyer direction but is typed on driver-app's merchant. Phase 1 uses rider-app's sender directly and keeps the module thin so it can be swapped.

**Files:**
- Create: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSSeller/CallBAP.hs`

**Interfaces:**
- Consumes: Task 5 (`Spec.OnSearchReq`)
- Produces: `SharedLogic.FRFSSeller.CallBAP.sendOnSearch :: Id Merchant -> BaseUrl -> Spec.OnSearchReq -> Flow ()`

- [ ] **Step 1: Read the existing sender**

```bash
sed -n '60,110p' Backend/app/rider-platform/rider-app/Main/src/SharedLogic/CallFRFSBPP.hs
sed -n '149,175p' Backend/app/rider-platform/rider-app/Main/src/SharedLogic/CallFRFSBPP.hs
```

Note the argument order of `callBecknAPIWithSignature'`: `merchantId`, the manager-selector key, the action name, the API proxy, the target URL, `internalEndPointHashMap`, then the request.

- [ ] **Step 2: Write the sender**

Create `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSSeller/CallBAP.hs`:

```haskell
module SharedLogic.FRFSSeller.CallBAP (sendOnSearch) where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Client (BaseUrl)
import qualified SharedLogic.CallFRFSBPP as CallBPP

-- | POST a signed on_search back to the buyer app.
--
-- The seller answers asynchronously: the inbound request already received an
-- ack, and this opens a NEW connection to the buyer's bap_uri. Failures are
-- logged and swallowed — this runs inside a fork and has no caller to return to.
sendOnSearch :: Id DM.Merchant -> BaseUrl -> Spec.OnSearchReq -> Flow ()
sendOnSearch merchantId bapUri req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <-
    req.onSearchReqContext.contextBapId
      & fromMaybeM (InvalidRequest "BapId missing on on_search context")
  result <-
    try @_ @SomeException $
      CallBPP.callBecknAPIWithSignature'
        merchantId
        bapId
        "on_search"
        Spec.onSearchAPI
        bapUri
        internalEndPointHashMap
        req
  case result of
    Right _ -> logInfo $ "on_search delivered to " <> showBaseUrl bapUri
    Left err -> logError $ "on_search delivery failed to " <> showBaseUrl bapUri <> ": " <> show err
```

- [ ] **Step 3: Build**

```bash
cabal build rider-app
```
Expected: compiles. If `callBecknAPIWithSignature'` is not exported, add it to the module's export list in `SharedLogic/CallFRFSBPP.hs` and rebuild. If its type demands additional constraints, add them to `sendOnSearch`'s signature rather than weakening the call.

- [ ] **Step 4: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSSeller/CallBAP.hs \
        Backend/app/rider-platform/rider-app/Main/src/SharedLogic/CallFRFSBPP.hs
git commit -m "rider-app/feat: send signed on_search callback to buyer app"
```

---

### Task 7: Wire the flow — search to callback, end to end

Connects Tasks 4, 5 and 6: the forked action builds a catalog from cached station data and sends `on_search`. No operator call — Phase 1 proves the protocol path.

**Files:**
- Create: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFSSeller/Search.hs`
- Modify: `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs`

**Interfaces:**
- Consumes: Tasks 3, 5, 6
- Produces: `Domain.Action.Beckn.FRFSSeller.Search.handleSearch :: Spec.SearchReq -> Flow ()`

- [ ] **Step 1: Write the domain action**

Create `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFSSeller/Search.hs`:

```haskell
module Domain.Action.Beckn.FRFSSeller.Search (handleSearch) where

import qualified Beckn.ACL.FRFSSeller.OnSearch as ACL
import qualified BecknV2.FRFS.Types as Spec
import Environment (Flow)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP

-- | Phase 1: answer every search with a fixed, valid catalog.
--
-- This deliberately makes no operator call. Its job is to prove the protocol
-- path end to end — signature in, ack, fork, signed callback out, buyer
-- accepts. Phase 2 replaces mkCatalog with real station and fare resolution.
handleSearch :: Spec.SearchReq -> Flow ()
handleSearch req = do
  let ctx = req.searchReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on search context")
  bapUri <- parseBaseUrl bapUriText
  merchant <-
    CQM.findByShortId sellerMerchantShortId
      >>= fromMaybeM (MerchantNotFound (getShortId sellerMerchantShortId))
  let onSearchReq = ACL.buildOnSearchReq ctx mkCatalog
  CallBAP.sendOnSearch merchant.id bapUri onSearchReq
  where
    -- Phase 1: one hard-coded seller merchant. Phase 2 selects it per city
    -- from the inbound context's city code, once Kochi exists too.
    sellerMerchantShortId :: ShortId DM.Merchant
    sellerMerchantShortId = ShortId "FRFS_SELLER_CMRL"

    -- Phase 1 placeholder catalog. Replaced in Phase 2 by station+fare lookup.
    mkCatalog =
      ACL.SellerCatalog
        { providerId = "CMRL",
          providerName = "Chennai Metro Rail Limited",
          items = []
        }
```

With imports:

```haskell
import qualified Domain.Types.Merchant as DM
import Kernel.Types.Id
import qualified Storage.CachedQueries.Merchant as CQM
```

> `CQM.findByShortId :: ShortId Merchant -> m (Maybe Merchant)` is at
> `Storage/CachedQueries/Merchant.hs:63`. The `shortId` used here must match the
> row seeded in Task 8 Step 2.

- [ ] **Step 2: Fork the action from the route**

In `Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs`, add the import:
```haskell
import qualified Domain.Action.Beckn.FRFSSeller.Search as DSearch
```

Replace the `if isFirst` block with:
```haskell
    if isFirst
      then do
        logInfo $ "FRFS seller search accepted: msg=" <> messageId
        fork "FRFS seller on_search processing" $ DSearch.handleSearch req
      else logInfo $ "FRFS seller search duplicate ignored: msg=" <> messageId
    pure Utils.ack
```

> The `fork` must come **after** the dedupe claim and **before** `pure Utils.ack`. The ack is what the buyer receives synchronously; everything else happens on the fork.

- [ ] **Step 3: Build**

```bash
cabal build rider-app
```
Expected: compiles clean.

- [ ] **Step 4: Re-check the whole app compiles**

```bash
cabal build rider-app
```
Expected: compiles clean. (Task 1 was descoped, so there is no test suite to run — Task 8
is the functional gate.)

- [ ] **Step 5: Commit**

```bash
git add Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFSSeller/Search.hs \
        Backend/app/rider-platform/rider-app/Main/src/API/Beckn/FRFSSeller/Search.hs
git commit -m "rider-app/feat: wire FRFS seller search to on_search callback"
```

---

### Task 8: Local end-to-end verification

Unit tests prove the builder; they do not prove a real request survives signature auth, routing, forking and delivery. This task runs the whole path locally.

**Files:** none — verification only.

**Interfaces:**
- Consumes: Tasks 1–7

- [ ] **Step 1: Start dependencies**

From `Backend/`, inside the nix shell:
```bash
, run-mobility-stack-dev
```
Expected: Postgres, Redis and supporting services come up.

- [ ] **Step 2: Seed the seller merchant**

Insert one merchant row for the operator, in schema `atlas_app`. It needs `subscriberId`, `shortId`, `bapId`, `bapUniqueKeyId`, `signingPublicKey`, `signingPrivateKey`, `gatewayUrl`, `registryUrl`, plus the ~9 rider-shaped `NOT NULL` columns that a seller does not care about (`driverOfferBaseUrl`, `driverOfferApiKey`, `driverOfferMerchantId`, `minimumDriverRatesCount`, `aadhaarVerificationTryLimit`, `aadhaarKeyExpiryTime`, `fakeOtpMobileNumbers`, `kaptureDisposition`, `geofencingConfig`) — give these any valid dummy value.

Copy an existing row as the template rather than writing the insert from scratch:
```sql
SELECT * FROM atlas_app.merchant LIMIT 1;
```

- [ ] **Step 3: Start rider-app**

```bash
cabal run rider-app
```
Expected: boots and listens on 8013.

- [ ] **Step 4: Send a signed search**

The endpoint is behind `SignatureAuth`, so an unsigned curl will be rejected — that is correct behaviour, and worth observing once:

```bash
curl -i -X POST http://localhost:8013/beckn/frfs-seller/v1/search \
  -H 'Content-Type: application/json' \
  -d '{"context":{"domain":"ONDC:TRV11","action":"search","version":"2.0.0","bap_id":"buyer.example.com","bap_uri":"http://localhost:8090","transaction_id":"txn-local-1","message_id":"msg-local-1"},"message":{}}'
```
Expected: `401`/signature error. This confirms auth is wired.

For a signed request, reuse the repo's existing Beckn signing helper:
```bash
grep -rn "signature" Backend/dev/ --include="*.sh" --include="*.md" | head -5
```
Follow whichever helper that surfaces. If none exists, use the mock registry from `, run-mobility-stack-dev` and the same signing path the buyer flow uses in `Backend/test/`.

- [ ] **Step 5: Verify the four checkpoints**

In the rider-app logs, confirm in order:
1. `FRFS seller search accepted: msg=msg-local-1`
2. A forked-processing log line
3. `on_search delivered to ...` **or** `on_search delivery failed to ...` (a delivery failure to a nonexistent buyer is expected locally — what matters is that the attempt was made with a signature)
4. Re-sending the **same** transaction and message id logs `FRFS seller search duplicate ignored`

- [ ] **Step 6: Record the outcome**

Append the result to this plan file under a new `## Phase 1 verification` heading: what was sent, what the logs showed, whether the duplicate guard fired. If any checkpoint failed, note it there before moving to Phase 2.

- [ ] **Step 7: Commit**

```bash
git add docs/superpowers/plans/2026-08-06-frfs-seller-phase-1.md
git commit -m "rider-app/docs: record FRFS seller phase 1 verification"
```

---

## Before you start

Two things must be settled by a human, not an implementer:

1. **The route prefix.** This plan uses `beckn/frfs-seller/v1`. It must not collide with the buyer's `beckn/frfs/v1`, and once a subscriber is registered against a URL, changing it means re-registering with the ONDC registry.
2. **The operator merchant rows.** Task 8 seeds one locally. Real subscriber ids and key pairs for CMRL and KMRL are an ONDC-registry conversation with external lead time — start it in parallel with Phase 1, not after.

## What Phase 2 picks up

Real station and fare resolution in `mkCatalog` (via `OTPRest.getStationByGtfsIdAndStopCode`); the item-id format decision (`journeytype-Source-Destination` from station **names** — fragile, but buyer apps may hold cached ids, so change it deliberately or not at all); `select`/`init`; then `confirm`+`status` together; then `cancel`, soft-before-hard including the direct-confirm bypass; then Kochi.
