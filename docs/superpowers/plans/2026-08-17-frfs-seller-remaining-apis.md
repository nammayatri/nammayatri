# FRFS seller — the remaining 10 APIs

Working plan for finishing the `public-transport-bpp` (Go) → `rider-app` (Haskell)
migration of the ONDC:TRV11 metro seller. Written to be executable by someone who has not
seen the first four messages get built, so it leads with the things that are not visible
in the code.

**Status: 10 of Go's 14 seller routes done; the RSF recon loop is complete.**

| | route | state |
|---|---|---|
| ✅ | `/search` | `a1d3ae9287`, `d28f629c9e`, earlier |
| ✅ | `/select` | done |
| ✅ | `/init` | done |
| ✅ | `/confirm` | `bab2d90a9d` |
| ✅ | `/status` | `05ae098db9` — §4.1, with two corrections noted there |
| ✅ | `/cancel` | `1bf99e8468` — §4.2, and smaller than the section implies |
| ✅ | `/issue`, `/issue_status` | `41de8469e0` — §4.3 Phase A |
| ⏸ | `/on_issue_status` | deliberately unmounted, §4.3 |
| ✅ | `/info` | §4.4 — **DONE** (`85b367a4e4`); `/on_info` stays unmounted (answers a message nothing sends) |
| ✅ | `/receiver_recon` (+ `on_receiver_recon`) | §4.5 — **recon loop complete** (`92c5cf7982`, `04d055e615`) |
| ⏸ | `/on_settle`, `/on_report` | §4.5 — answer loops we never start; `settle` is an internal ops route outside seller scope |

RSF (§4.5) is what remains of substance: a different domain entirely (`ONDC:NTS10`), a
net-new wire layer, its own auth combinator and a migration. §4.4 has no plan text at all —
whoever takes `info` writes it first.

**Two environment findings from the IGM work, neither of them code:**

1. `merchant.subscriber_id` is `character(36)` — Postgres BLANK-PADS it — while
   `beckn_config.subscriber_id` is `text`. The seller's id is 35 chars, so any read of the
   merchant column carries a trailing space, which breaks signing-manager lookup and
   corrupts the published `bpp_id`. Nothing hit it before because every other subscriber id
   is a 36-char UUID. **Take seller identity from `beckn_config`, never from `merchant`.**
2. **The drainer halts on the first bad row and stays halted** — it sets
   `RIDER_DRAINER_STOP=true` in Redis and every later row, however correct, stops draining
   behind it. Ours had been stopped since 2026-08-14 on a `varchar(36)` overflow from the
   old `sellerSearchId` bug. Recovering it (done, 2026-08-18):

   ```sh
   redis-cli -c -p 30001 get RIDER_DRAINER_STOP        # "true" = halted
   tail /tmp/rider-app-drainer-eul.log                 # names the column that rejected it
   # delete the offending stream entry, then:
   redis-cli -c -p 30001 del RIDER_DRAINER_STOP
   cd Backend && direnv exec . cabal run rider-app-drainer:exe:rider-app-drainer-exe
   ```

   Two things that cost time: the drainer must be launched **from `Backend/`** (it reads
   `./dhall-configs/dev/rider-drainer.dhall` relative to CWD and dies instantly otherwise),
   and its real log is `/tmp/rider-app-drainer-eul.log`, not the stdout capture.

Route list is `internal/app/integration/integrationRoutes/metro_seller_routes.go`. Non-seller
routes (`cacheSearch`, `signpayload`, onboarding) are §5.

---

## 1. The loop, and why it is slow

Per-change cost, measured: `cabal build rider-app` is minutes, app restart ~90s, and the
config caches mean a restart is often mandatory rather than optional. **The build/verify
cycle dominates everything.** Two consequences for planning:

- **Batch changes per build.** Implementing `status` and `cancel` in one cycle is much
  cheaper than two. Do not build after every file.
- **Parallel agents editing this package do not help** and actively hurt: they serialise on
  the one build and collide on the shared modules every message touches —
  `API/Beckn/FRFSSeller.hs`, `SharedLogic/FRFSSeller/{Common,CallBAP}.hs`,
  `Beckn/ACL/FRFSSeller/OnInit.hs` (which now exports `mkStop`/`mkPrice`/`ticketCategoryId`).
  Split by *reading*, not by writing.

```sh
cd Backend
ulimit -s 50000                      # without this the linker segfaults with exit 139
direnv exec . cabal build rider-app  # NOT bare cabal
, hpack                              # after adding or deleting a module (-Wmissing-home-modules)
```

`-Werror` is on, including unused imports. A comment-only edit still needs a build if it
touches Haddock in an export list.

### Restarting the app

`process-compose` supervises it, but has twice stopped restarting it after a kill. If
`:8013` is dead, start it manually from `Backend`:

```sh
ulimit -s 50000 && direnv exec . cabal run rider-app:exe:rider-app-exe > /tmp/rider-app-manual.log 2>&1 &
```

**Application logs go to `/tmp/rider-app.log`** (from the dhall `logFilePath`), *not* to
that stdout capture. The stdout file only has direnv/nix noise. Time lost to this: real.

---

## 2. Traps that cost time in this session

Every one of these produced a green-looking result that was wrong.

### 2.1 The stale binary, and two apps at once

`pgrep -f "rider-app-exe"` matches the `cabal run` wrapper too, so `kill $(pgrep ... | head -1)`
can kill the wrapper and leave the exe serving, or kill one of *two* running instances. Twice
a test ran against a binary built before the change.

**Always confirm the process start time is later than the binary mtime:**

```sh
ps -o pid=,lstart= -p $(pgrep -f "rider-app-exe/rider-app-exe$")
stat -c "%y" Backend/dist-newstyle/build/aarch64-osx/ghc-9.2.7/rider-app-0.1.0.0/x/rider-app-exe/build/rider-app-exe/rider-app-exe
lsof -nP -iTCP:8013 -sTCP:LISTEN     # exactly one owner
```

### 2.2 `IntegratedBPPConfig` is cached twice

In Redis under `app-backend:ConfigPilot:IntegratedBPPConfig`, **and** in the app process on
top. So a bare `UPDATE` is invisible, busting Redis alone is *also* invisible, and the app
must be restarted. Cost two wrong runs: one that produced a flawless catalog while still
talking to the mock, and one that read the stale host back out of Redis after a restart.

Anything that changes a config row must: `UPDATE` → bust `*ConfigPilot*` → **restart**.
`hurl/frfs-seller/test_against_real_cmrl.sh` does this correctly; copy it.

Redis `--scan --pattern` is **case-sensitive**. `*beckn_config*` matches nothing; the key is
`*BecknConfig*`. A test once "passed" purely because of this.

### 2.3 Anything ≤36 chars is a silent data-loss bug

`frfs_ticket_booking.id`, `.search_id`, `.quote_id` are `varchar(36)`. An over-long value
**does not fail on write**: the KV layer accepts it into Redis, the request succeeds, and the
drainer drops the row later when Postgres rejects it. The booking simply ceases to exist
seconds after the buyer was told its tickets were confirmed.

It surfaced only because a subsequent `updateByPrimaryKey` happened to hit Postgres directly.
Deterministic ids that must fit go through UUIDv5 — see `sellerSearchId` in
`Domain/Action/Beckn/FRFSSeller/Confirm.hs`.

### 2.4 CMRL v2 (CDAC) surprises

- **Auth rejection arrives as HTTP 200** with `{"errMsg":"Invalid username or password or
  merchantdetails!!! "}`. Not 401. Handled in `Auth.hs` (`AuthResult`) as of `75d6d5ae7b` —
  do not undo it, and expect the same shape on other endpoints (unverified; check before
  assuming).
- **CDAC issues the AES key at auth** and versions it with `key_index`. Use
  `Auth.getEncryptionKey`, never `config.encryptionKey` (`d1583e22de`).
- **Go does not encrypt at all** — plain JSON, no envelope, no `X-ENC-*` headers. So Go is
  not a reference for our encrypted path; the only evidence it works is
  `hurl/frfs-seller/cmrl_v2_probe.py` against the live box, 2026-08-14.
- The pre-prod CDAC endpoint (`CHENNAI_V2_BASE_URL`) is **not** the prod one
  (`https://merchantcta.chennaimetrorail.org`). The `quickticketapi.chennaimetrorail.org/api/ONDC/*`
  URLs in `app.env` are **v1 config and unused** — `CHENNAI_API_VERSION=v2`. Reading the v1
  block and concluding "pre-prod talks to production" is a mistake that was made once.
- CMRL credentials live in the pre-prod pod at `/app/app.env` (`CHENNAI_V2_*`). Run auth and
  fetch in a **single** `kubectl exec` so they stay in the pod, unless deliberately doing
  otherwise. The box is reachable from a laptop (it is *not* allowlisted to the cluster) and
  takes credentials over plain HTTP.

### 2.4b Two rider-apps on one port, and a `set -e` that kills a suite silently

Two traps that between them cost most of a session, both of which present as "the test is
flaky" rather than as themselves.

**Two rider-app instances were bound to 8013 at once** — one on IPv4, one on IPv6. Both
`LISTEN`, both serving, one running a stale binary. Every result was a coin flip, and a
falsification that "did not reproduce" was simply answered by the other process. Check with
`lsof -nP -iTCP:8013 -sTCP:LISTEN` and expect exactly ONE row; the IPv4/IPv6 pair is the
tell. This is §2.1's stale-binary trap wearing a second face.

**`set -e` plus `x=$(psql ...)` kills a bash suite mid-run, silently.** A command
substitution in an assignment is a simple command, so a non-zero psql exit terminates the
script — no message, no non-zero suite result, just a log that stops in the middle. It
reads exactly like a hang. Every psql call in the seller suites is now `|| true` and every
assertion a full `if/fi`; `[ cond ] && action` is safe under `set -e` but sets a trap for
the next person who adds an `else`.

### 2.5 `acceptOnce` is not idempotency

`API/Beckn/FRFSSeller/Handler.hs` dedupes on (operator, action, txn, msg) for **60s** and
**fails open** when Redis is unavailable. Correct for search/select/init. **Not sufficient for
anything that spends, refunds or settles.** `confirm` therefore carries a durable claim in
`frfs_ticket_booking.search_id` (a KV secondary key, so the read sees a row written moments
ago). `cancel` and the settlement messages need the same treatment — decide it *before*
writing the handler, not after.

### 2.6 Ids are contract, and derive from CMRL's station names

`item.id` = `<journeyType>-<from>-<to>` where the halves are CMRL `stationName` stripped to
ASCII alphanumerics (`Common.journeyIdFromStationNames`). **Not GTFS names** — GTFS says
"Washermenpet", CMRL says "Washermanpet Metro", and only 2 of 51 GTFS names strip to an id Go
would mint. Buyers echo these back, and Go parses them apart with `strings.Split(id, "-")`.
`local.env`'s `expected_item_id` asserts this; it was wrong for weeks and looked reasonable.

---

## 3. Verification discipline

The rule earned the hard way: **before trusting a passing test, establish that it could
fail.** Three tests in this session passed for the wrong reason.

1. **`*beckn_config*` vs `*BecknConfig*`** — the cache was never busted, so the "missing
   config" test read the populated config it thought it had removed.
2. **`NO-CALLBACK == NO-CALLBACK`** — the confirm-retry test compared two failures and
   reported "identical tickets, operator not called again". Now guarded explicitly.
3. **The mock's AES key equalled the config key** — so the encryption-key fix passed
   identically before and after. The mock now issues a key config cannot hold, which is what
   makes the test mean something.

Practical checks:

- **The mock's fares match real CMRL by design** (40 SJT / 80 RJT, hardcoded to match). So
  output equality proves nothing about which upstream was called. Verify the **auth host** in
  `/tmp/rider-app.log`.
- When a test asserts a *refusal*, confirm the request reached the code being tested. A
  request that dies earlier (e.g. `MerchantDoesNotExist`) throws inside the fork and publishes
  **no callback at all** — distinguishable from an error callback, so assert on the error
  *code*, not on failure.
- Prefer reproducing a real failure over building a fixture. The HTTP-200 auth fix was
  verified against a genuine CDAC rejection, because the seeded local password is a dev
  placeholder and so produces one on demand.

### Signatures are now VERIFIED, not just present

Every callback assertion in the harness used to check that an `Authorization` header
existed. That is a much weaker claim than it reads as: a callback signed with the wrong key,
over the wrong bytes, or with a stale digest carries a perfectly good-looking header, and
`bap_listener.py` does not verify — so nothing here would have caught it before a real buyer
refused it. The suites were making the weak claim while the commit messages made the strong
one.

`_verify_sig.py` closes it, checking what mobility-core checks
(`Kernel/Utils/SignatureAuth.hs`): the digest is `BLAKE-512=` over the EXACT body bytes, the
signing string is `(created)/(expires)/digest`, the Ed25519 signature verifies under the
subscriber's public key, and the keyId names us with `ed25519`. It is wired into
`_show_capture.py` (so every `--show` path covers `on_search` through `on_cancel`) and into
the `on_receiver_recon` and `on_info` assertions.

The public key is derived from the dev signing seed in
`dhall-configs/dev/secrets/rider-app.dhall` — every dev subscriber shares it, which is what
lets one key verify both sides locally.

**Falsified four ways** (altered body, altered signature, altered `created`, wrong key — all
rejected), and then proven LIVE rather than merely present: a forged `on_receiver_recon`
satisfying every structural assertion is still refused on the signature alone. That last
step matters, because a verifier that always returns "ok" is worse than no verifier, and
looks identical from a green run.

⚠️ **This does not verify OUTBOUND against a real counterparty.** It proves our signature is
internally valid under the key we hold. Whether the real registry resolves that key for our
subscriber — and whether `PUBLIC_TRANSPORT` is the right domain for NTS10 — is still
unanswered and needs cluster access.

### The harness

```
hurl/frfs-seller/
  sign.sh                     # signs (Ed25519 over Blake2b-512) and runs hurl
    --chain / --chain-init / --chain-confirm   # search then select/init/confirm, same txn
    --chain-status                             # ...then status, on the order confirm minted
    --replay                                  # same (txn,msg) twice: dedupe must suppress #2
    OPERATOR= / CHAIN_OPERATOR= / QTY= / ITEM_CODE= / FEE= / TXN= / MSG=
  test_confirm.sh             # issue / retry-idempotency / 30005 / 30004
  test_status.sh              # refresh really happened / 31002 / full order republished
  test_cancel.sh              # 50001 refusal / soft-vs-confirm / idempotency / 31002
  test_cross_operator.sh      # operator-scoped keys; seeds a KMRL merchant
  test_missing_settlement.sh  # init refuses when settlement config is absent
  test_against_real_cmrl.sh   # points local app at real CDAC, read-only
  cmrl_v2_probe.py            # our cipher, driven against real CDAC
  reference/                  # golden Go on_search + live CMRL station roster
```

Needs `bap_listener.py` on :9911 to catch callbacks, and the mock servers on :8080.
`reference/README.md` is the record of what was captured from where, and when.

**The hurl directory is not version-controlled.** It lives on disk only.

---

## 4. Per-API plans

### 4.0 Read this first — the reference implementation is in-house, not Go

`dynamic-offer-driver-app` **is already a BPP** (TRV10) and serves inbound `status`, `cancel`,
`issue` and `issue_status`. For the *structure* of a seller message, mirror it rather than
translating Go: same idioms, same libraries, already reviewed and shipped.

- `src/API/Beckn/Status.hs:40`, `src/API/Beckn/Cancel.hs:59`, `src/API/Beckn/IGM/Issue.hs:38`
- The callback half worth stealing: `src/Beckn/OnDemand/Utils/Callback.hs:33-119` —
  `withCallback` → `withBecknCallback` acks synchronously (`:87`) and runs the work in
  `forkBecknCallback` (`:102`). Note `:114-119`: a BAP NACK on an expected state mismatch is
  logged at info, not raised as a 5xx. Take that verbatim.
- Failure model there is **exceptions plus a per-route `errHandler :: Spec.Context ->
  BecknAPIError -> Spec.On*Req`** (`Status.hs:74-87`, `Cancel.hs:163-176`) applied by
  `forkBecknCallback` on `Left`.
- Those helpers are **not** in `Backend/lib/` — they live only in driver-app. Copying means
  copying, or promoting to the lib.

**Do not mirror it wholesale.** Two things here are already better and must stay:
`Handler.acceptOnce` (driver-app's `status` has *no* dedupe at all, `cancel` has an ad-hoc
60s Redis lock) and `sellerAck` (driver-app has no equivalent; the shared FRFS `ack` helper
hardcodes `"200"`, which is not a Beckn ack status and not what Go returns). So: **our
inbound contract, driver-app's callback-send pattern.**

---

### 4.1 `status` → `on_status` — DONE (`05ae098db9`)

**Two things this section got wrong, corrected in the code:**

1. It said to **persist** the refreshed ticket state. Don't. The operator's answer is the
   truth at call time; writing it back lets a transient blip overwrite a ticket the
   passenger still holds. Nothing is persisted.
2. It said to lift `mapFRFSStatusToDTicketStatus` out of the buyer's `where` clause. Better
   home: next to its inverse `castTicketStatus` in `Beckn/ACL/FRFS/Utils.hs`, as
   `wireTicketStatus`. The buyer diff then becomes a pure deletion, and the seller stops
   importing a buyer *domain-action* module.

**And a bug this section did not know about.** `mkIssuedTicket` published
`qrStatus = show ticket.status` — the domain constructor. `castTicketStatus` throws on
anything outside `{UNCLAIMED, CLAIMED, EXPIRED, CANCELLED}`, so a nammayatri buyer **threw
on our own `on_confirm`**. Fixed in the same commit. If you add any other field that
crosses to a buyer, check there is a parser for the exact string you emit.

**What to do.** Resolve the booking, fetch live ticket state from the operator,
publish the stored `on_confirm` order with authorization statuses refreshed and an
`order.status` derived from the ticket set.

Go: route `metro_seller_routes.go:45`; payload built by
`transformer/metro_transformer.go:892-967` from the **latest** stored `on_confirm`
(deliberately latest — a failed confirm stores a placeholder whose `order.id` is the
transaction id, `metro_manager.go:443-449`). Everything is copied verbatim except: per START
stop `authorization.status = upper(ticketStatus)` matched on
`authorization.token == TicketGuid` (`:907`), token **blanked** when `CANCELLED` (`:916`),
`updated_at = now`, and `order.status`.

**Spec needed — all present, nothing to add.** `Spec.StatusReq`/`OnStatusReq`
(`BecknV2/FRFS/Types.hs:1616,1184`), `StatusAPI`/`OnStatusAPI` (`APIs.hs:118,126`),
`STATUS`/`ON_STATUS` (`Enums.hs:53,63`). Both callbacks reuse `ConfirmReqMessage` — there is
no `OnStatusReqMessage`.

**Status mapping already exists in two halves, both reusable:**
- operator wire → domain: `castTicketStatus` (`Beckn/ACL/FRFS/Utils.hs:352-360`), plus the
  expiry override at `:345-350`. Pass `checkInprogress = False` (solicited).
- domain → wire: `mapFRFSStatusToDTicketStatus` (`Domain/Action/Beckn/FRFS/OnStatus.hs:140-148`)
  → `{UNCLAIMED, CLAIMED, EXPIRED, CANCELLED}`, an exact match for Go's vocabulary. **Lift it
  out of the `where` clause** so the seller can call it.

**⚠ Look out for — `status` is broken for CMRLV2 today.**
`Metro/CMRL/V2/TicketStatus.hs:113` **hardcodes `qrStatus = "ACTIVE"`**, and `"ACTIVE"` is not
in `castTicketStatus`'s domain, which `throwError`s on anything unrecognised. It compiles
clean and fails at runtime. That endpoint is the *QR-reissue* one — Go uses it only for order
recovery (`chennai_metro_v2.go:1058`). Real state comes from a different endpoint Haskell does
not have: `GET api/qr/v1/tickets/details-by-ticketId?operatorNameId=&ticketId=`
(`chennai_metro_v2.go:985-1024`), mapped by `chennaiV2TicketStatusMap` (`:93-99`). **So a new
adapter module is required, not optional.** Note `ENTRY_USED` (entered, not exited) collapses
to `UNCLAIMED` in Go — not `INPROGRESS`. Reproduce that.

**What NOT to do.**
- `SharedLogic/FRFSStatus.frfsBookingStatus` — rider UI poll. `:93` denies any seller row on
  `personId /= riderId`, and `:227` actually *triggers a confirm*.
- `Domain/Action/Beckn/FRFS/OnStatus.hs` — inbound, i.e. an `on_status` we received as buyer.
  Pure naming trap.
- `Flow.Common.status` (`ExternalBPP/Flow/Common.hs:514`) wraps results in a buyer-shaped
  `DOrder`. Call `CallAPI.getTicketStatus` directly.
- Don't port Go's `order.status` derivation: it seeds `"COMPLETE"` and only downgrades inside a
  nested loop, so with several tickets the **last match wins** and an unmatched guid silently
  stays `COMPLETE` (`metro_transformer.go:895`). Derive deterministically from the whole set
  and document the divergence.
- `orderStatus` is `Maybe Text`, so the compiler cannot catch a bad string. Use
  `ACLUtils.encodeToText'`: `show SOFT_CANCELLED` is `"SOFT_CANCELLED"` but the wire value is
  `"SOFT_CANCEL"` (`Enums.hs:192`). Also `OnConfirm.hs:165`'s comment says `"COMPLETED"` — the
  valid value is `COMPLETE`; do not copy that string.

**How to verify.** `--chain-confirm` to create a booking, then a signed `status` for that
`order.id`; assert one `authorization.status` per ticket and an `order.status` consistent with
them. Then flip one ticket's status directly in the DB and re-run — the published value must
change. Mock note: the CMRL mock's ticketstatus route returns success-shaped data, so **assert
on the mapped output, not on the call succeeding**.

---

### 4.2 `cancel` → `on_cancel` — DONE (`1bf99e8468`)

**What this section over-planned.** The whole `calculateCancellationCharges` question is
moot: the only cancellation Chennai accepts is `reason_id == "0"`, which Go always refunds in
full with zero charges. So there is no tier arithmetic to get wrong and no config to verify —
the function is never called. It stays the hook for KMRL, where a real fee exists. The
warnings about `FRFSCancel.handleCancelledStatus` and
`checkRefundAndCancellationCharges` were right and both are avoided.

**One decision this section did not make: `SOFT_CANCEL` persists nothing.** It is a
quotation. Writing `CANCEL_INITIATED` through to the tickets would make the next `status`
report a live ticket as cancelled and blank a token the passenger can still travel on — and
the `on_cancel` itself looks identical either way, so only a follow-up `status` catches it.
`test_cancel.sh` step 3 is that check.

**The scope surprise: CMRL has no cancellation API, and neither does Go.**
`chennai_metro_v2.go:1037-1054` (and V1 `chennai_metro.go:441-450`) reject anything with
`cancellation_reason_id != "0"` as `TF_METRO_CANCELLATION_NOT_POSSIBLE` = **`50001`**, and
treat `"0"` (technical cancellation) as a no-op success with **full refund** and
`UpdatedSettlementAmount = 0`. The gate is the *reason id*, not the cancel type.

So Phase-1 `cancel` for Chennai is **local bookkeeping plus a refusal** — no operator call.
That is correct behaviour, not a gap, and it is much smaller than it looks. It also matches
what our own buyer half expects: `API/Beckn/FRFS/OnCancel.hs:62` sets
`isBookingCancellable = False` on exactly code `50001`.

**KMRL is where this stops being acceptable** — it has real soft/hard cancel endpoints and a
real refund (`kochi_metro.go:553-649`). A seller answering `on_cancel { CANCELLED, refund }`
without calling them would leave a live ticket at the operator. Keep the operator call behind
the per-operator rule in `Common.hs`.

**Spec needed — present, but thinner than you'd expect.** `CancelReq`/`OnCancelReq`
(`Types.hs:229,1005`), `CancelAPI`/`OnCancelAPI` (`APIs.hs:142,150`). **But**
`CancellationTerm` is a newtype with one field, `cancellationTermExternalRef :: Maybe MediaFile`
(`Types.hs:314-317`) — it **cannot carry a fee, refund or window** — and `Cancellation` has
only `cancelledBy` + `time` (`:287-292`), no reason. **Refund amounts travel in
`orderQuote.quotationBreakup` titles**, which is what the buyer reads
(`BecknV2/FRFS/Utils.hs:185-196`).

**The breakup must carry all three titles, in order: `BASE_FARE`, `REFUND`,
`CANCELLATION_CHARGES`** (`metro_transformer.go:1046-1082`), with `quote.price.value`
overwritten to the cancellation charges. Go's own fallback path emits only two, omitting
`BASE_FARE` — and our buyer's `getAndValidateCancellationParams` rejects that outright. Do not
copy the fallback.

**Reuse:** `Flow.Common.calculateCancellationCharges` (`ExternalBPP/Flow/Common.hs:576-618`) is
a pure config read plus arithmetic — the best reuse candidate. `DOnCancel`
(`Domain/Types/Beckn/FRFS/OnCancel.hs:7-18`) is already the right shape. **Every field a seller
`on_cancel` needs already exists on `frfs_ticket_booking`** (`cancellationCharges`,
`customerCancelled`, `isBookingCancellable`, `refundAmount`, `status`, `startTime`,
`validTill`, `totalPrice`) — **no schema change.**

**What NOT to do.**
- `SharedLogic/FRFSCancel.handleCancelledStatus` — `:78` does `QPerson.findById booking.riderId
  >>= fromMaybeM PersonNotFound`, which **throws for our sentinel rider**; `:79` needs a payment
  booking; `:97` initiates a Juspay refund; `:99-100` mutate rider counters. Extract the
  booking/ticket status writes (`:86-95`) into a rider-free helper instead. **Do not "fix" the
  person lookup** — that weakens the buyer path.
- `checkRefundAndCancellationCharges` (`FRFSCancel.hs:123-132`) throws unless both money fields
  are already persisted, and seller `confirm` writes both as `Nothing`. A direct
  `CONFIRM_CANCEL` with no prior `SOFT_CANCEL` fails that assert.
- Don't assume `calculateCancellationCharges` matches Go. Only its **empty-config** branch does
  (`:591` → `(0, baseFare)`). If buyer-side `FRFSCancellationConfig` rows exist for the seller's
  `(mocId, METRO)` — plausible, since rider-app buys Chennai metro — the seller silently
  inherits tiers Go never applied, and the outside-all-windows branch throws
  `CancellationNotSupported` (`:614`) where Go refunds in full. **Verify against the deployed
  rows.** Note the tier key is `fromMaybe validTill startTime` and seller bookings have
  `startTime = Nothing`.
- **`acceptOnce` is not enough here.** Mirror `Confirm.hs:146-155`: if the booking is already
  `CANCELLED`, republish from persisted state rather than recomputing.

**How to verify.** `--chain-confirm`, then `cancel` with `cancellation_reason_id != "0"` → must
be `50001`; with `"0"` → `SOFT_CANCEL` then `CONFIRM_CANCEL`, asserting `order.status`
`SOFT_CANCEL` → `CANCELLED`, all three breakup titles present, authorizations cleared, and the
settlement tag rewritten. Re-send the confirm-cancel and assert it republishes rather than
double-refunding.

---

### 4.3 IGM — `issue`, `issue_status` — PHASE A DONE (`41de8469e0`)

**What this section got right:** the three blockers were real, `openBecknIssue` is the one
hard wall, and Phase A genuinely needs no migration.

**What it missed, and what made the work much smaller than planned:**

- `mapStatusAndTypeToStatus` maps **OPEN+GRIEVANCE → `ESCALATED`**, not to a second OPEN.
  Escalation therefore UPDATES the row whose id the buyer reused. Following the shared
  vocabulary is what makes this correct; a hand-rolled gate inserts a duplicate.
- **`DIssueStatus.handler` takes no `ServiceHandle` and touches no ride** — only its
  `validateRequest` does. So `issue_status` is near-total reuse; build the
  `ValidatedDIssueStatus` yourself and call the handler.
- `buildIssueReq` already parses the whole envelope including the complainant chain. Do not
  re-read those fields by hand.
- **IGM's core version is `1.0.0`**, not TRV11's `2.0.0`, and the wrong one is rejected
  before the handler runs (`IGM/Utils.hs:56`). Domain stays `ONDC:TRV11`.
- Sub-categories are a closed enum — `FLM101` is "ticket not working at the gate". An
  invented code is rejected at `ACL/Issue.hs:31`.

**Phase B (resolution) is a real capability regression, not a spec problem.** `issueResolution`
is `Maybe` and Go omits it too when absent, so Phase A is spec-legal. But Go CAN close a
seller-side issue with a resolution — ops posts to `POST /igmResolution`
(`public_transport_router.go:27`) and `SendUpdatedIssueToBap` pushes it. Whether that
matters is an ops question: does anyone actually close metro complaints that way? Unanswered
— the Triffy database is not reachable from Metabase.

### 4.3-orig (superseded) — `issue`, `issue_status`, `on_issue_status`

**Do not port from Go. Extend the shared library.** `Backend/lib/shared-services/src/IssueManagement/`
already implements the *receiving* side end to end — the direction rider-app lacks. State
handling at `Domain/Action/Beckn/Issue.hs:121-125`, persist at `:165`, callback sent from a fork
in `API/Beckn/Issue.hs:61-67`.

Three blockers, all specific:
1. **No TRV11 seller route exists.** `Common/Beckn/Issue.hs:30-36` exposes `PublicTransportAPI`
   as `on_issue`/`on_issue_status` only; the sole seller route is `/beckn/:merchantId/issue`
   under `SignatureAuth 'Domain.MOBILITY`. We need a TRV11 route on our operator-first path
   with `SellerSignatureAuth`.
2. **Persistence is Ride+driverId-shaped** (`Domain/Action/Beckn/Issue.hs:137,170`) while the
   payload builders are **already hardcoded to ONDC:TRV11** (`ACL/Issue.hs:50,128`). An inbound
   FRFS `/issue` dies at `:137`. The hook already exists: `mbFindFRFSTicketBookingById` is in
   the `ServiceHandle` (`Domain/Action/UI/Issue.hs:88`) and rider-app already wires it
   (`API/UI/Issue.hs:132`) — the Beckn seller path just never calls it.
3. Inbound validation is already domain-agnostic (`beckn-spec/src/IGM/Utils.hs:32`), so that
   part is free.

**Tables — split the answer by ambition. I initially wrote that "zero new tables" fails here;
that is only true of the fuller version.**

`atlas_app.igm_issue` **already exists** (`dev/migrations-read-only/rider-app/igm_issue.sql`,
with hand-written Domain/Beam/Queries under `IssueManagement/.../Issue/IGMIssue.hs`) and it can
hold a seller-side issue as-is: `booking_id` takes the `frfs_ticket_booking` id, `domain` takes
`ONDC:TRV11`, `issue_raised_by_merchant` takes the buyer's `bap_id`, and `issue_status` ×
`issue_type` cover Go's `OPEN`/`CLOSED` × `ISSUE`/`GRIEVANCE` exactly. No FK to person, ride or
booking is enforced.

- **Phase A — `issue` + `issue_status` with a single respondent action and no resolution: NO
  migration.** This is the honest MVP and it is genuinely reusable.
- **Phase B — resolution parity: migration unavoidable.** Nothing stores `short_desc`/
  `long_desc`/`gro_remarks`/`odr_remarks`/`action_triggered`/`refund_amount`. (`igm_issue` has a
  `resolution_action` column **in the SQL but in neither the Domain nor the Beam type**, so it is
  unreachable from code — one column against Go's seven.)
- **Phase B — action *history* parity: also a migration.** Go's `igm_actions` is append-only and
  its `on_issue_status` emits **one respondent_action per row**, whereas `igm_issue` has a single
  scalar `respondent_action` and the shared ACL synthesises exactly one action from it
  (`Beckn/ACL/IssueStatus.hs:73-82`). Decide which you are shipping.

Adding a column here is not cheap: these types are **hand-written, not NammaDSL-generated** (no
`spec/Storage` yaml), so a field costs Domain + Beam + `FromTType'`/`ToTType'` + a new
`dev/ddl-migrations` file + a mirror into `migrations-read-only`.

**A deploy prerequisite, not a migration:** `igm_config` rows for `FRFS_SELLER_CMRL` and
`FRFS_SELLER_KMRL`. Both `Domain/Action/Beckn/Issue.hs:105` and `IssueStatus.hs:76` throw
`InternalError "IGMConfig not found"` when `findByMerchantId` misses.

**Verify before writing any insert:** `igm_issue.sql:6,11` declare `merchant_operating_city_id`
and `responding_merchant_id` **NOT NULL**, while the Beam type has both as `Maybe Text` and the
existing shared insert writes `Nothing` to both. Either the deployed schema differs from the
read-only file or the on-demand seller insert is already failing. Check the live schema; don't
pick a side from source.

**The one hard blocker.** `openBecknIssue` (`IssueManagement/Domain/Action/Beckn/Issue.hs:136-171`)
is welded to on-demand: `:137` requires a ride, `:170-171` require a `driverId` and file the
report as `Common.DRIVER`. A metro booking has neither. **Replace it with a seller-specific
domain action; do not adapt it** — and specifically do not repurpose the shared
`Identifier` `DRIVER`/`CUSTOMER` switch (`API/Beckn/Issue.hs:64,139`) to mean "reply to bap_uri
vs bpp_uri", because that same value also selects issue-config and report scoping for driver-app.

Everything *around* that blocker is reusable and already TRV11: `Beckn/ACL/Issue.hs:39-86`
`buildOnIssueReq` hardcodes `Spec.PUBLIC_TRANSPORT` (`:50`), `IssueStatus.hs:29-48` likewise, and
`IssueManagement/SharedLogic/CallAPI.hs:17-57` signs with `merchant.subscriberId` — which is
exactly the seller-signs-as-itself rule our `CallBAP` documents.

**`CallBAP.sendCallback` cannot carry IGM.** It is pinned to
`IsBecknAPI api req BecknV2.FRFS.Types.AckResponse`; IGM returns `IGM.Types.AckResponse`. Use the
shared `IssueManagement.SharedLogic.CallAPI` rather than generalising ours.

**Mount on the seller path, not by extending `PublicTransportAPI`.** That type
(`rider-app/src/API/Beckn/IGM/Issue.hs:38-39`) serves the **live buyer** TRV11
`on_issue`/`on_issue_status`; extending it would hand the buyer path a seller identity.

**Defer `/on_issue_status`.** `getSubscriberType ON_ISSUE_STATUS = BPP`, but in Go that route
only ever services issues *we raised outbound to a BAP* (`metro_manager.go:1018-1021`) — so the
caller is a BAP and a BPP lookup would likely 401. Phase A raises no outbound issues, so mount
it only when outbound `issue` exists rather than inheriting the mismatch. *(That 401 is inference
from the enum mapping, not a traced registry lookup.)*

**Three `on_issue`/`on_issue_status` parity gaps that ship in Go today** and are not
representable: `Issue.selected_odrs_info` (Go emits real per-city ODR blocks from
`prod-metro-bpp-config.json`), `IssueResolution.odr_remarks` and `refund_amount`, and
`RespondentAction.cascaded_level`. All need shared beckn-spec additions — and remember the
options-table rule above when adding them.

**Reproduce Go's escalation gate** (`integration_service.go:276-290`): `issue_type = ISSUE`
always passes; `GRIEVANCE` passes only if a prior issue exists **and** its status is not `OPEN`
**and** its type differs — i.e. a grievance is accepted only after the issue was closed.

**Do not model Go's lifecycle or its idempotency — it has neither.** The only status constants
are `OPEN` and `CLOSED` (`integration_constant.go:97-98`); `issue_status` is unconstrained
varchar and inbound `on_issue_status` writes **whatever string the BAP sends, unvalidated**
(`metro_manager.go:1023-1028`). There are no SLA timers — `expected_time_to_*` are opaque
strings never compared to `now()`. And there is **no dedupe anywhere on the IGM path**: no
unique constraint on `issue_id`, no upsert, so a redelivered `/issue` produces duplicate rows
after which every read is ambiguous and updates hit all of them. Go's one attempted dedupe is
broken — `CheckIfIgmActionExists` queries schema `integration` for a table that lives in `pt`,
so the count always errors and **every respondent action on an inbound `on_issue_status` is
silently dropped**. Our 60s fail-open claim is already strictly better; design a real one.

**Spec needed — all four endpoints already exist and compile with zero new instances**
(`BecknV2/IGM/APIs.hs:21-51`: `issue`, `on_issue`, `issue_status`, `on_issue_status`, each
`ReqBody '[JSON] → AckResponse`; no auth combinator, the consumer applies it).

`ToSchema` is **definitively not needed**, and this was checked rather than assumed: swagger is
generated from `MainAPI` alone (`rider-app/src/API.hs:138`), and `MainAPI`'s `Beckn.API` /
`Beckn.APIV2` lines are commented out in place (`API.hs:67-69`) while the top-level `type API`
serves the Beckn tree *outside* the swagger'd one (`:55-61`). So the Beckn routes are
deliberately excluded from `toOpenApi`. You would only need `ToSchema` by attaching IGM types to
`MainAPI` or the dashboard tree, which the seller flow should not do.

**Calibrate the two failure directions — they are opposites, and only one is dangerous.**
*Reading* a field that does not exist is a **compile error**: loud, immediate, cheap (this is the
`Spec.Time` mistake from §2 — annoying, self-correcting). *Adding* a field to an IGM record
without its options-table entry is **silent**: it compiles, warns nothing, and emits a garbage
JSON key on the wire. So if a phase only **consumes** existing fields, the inventory below is all
the protection needed; the moment it **extends** any IGM record, the table entry is a mandatory
second edit in the same change.

**Six spec traps, each of which costs a build cycle or a silent runtime bug:**

1. **`IGM/Types.hs` does not use prefix-stripping — every type has a hand-written assoc-list
   `fieldLabelModifier`** (e.g. `optionsIssue`, `Types.hs:562-587`). A field added to a record
   but **not** to its table serialises as the literal key
   `"did not find JSON field name for \"fieldName\""`. It compiles clean, emits no warning (the
   file sets `-fno-warn-unused-binds`), and only shows up on the wire. **If you add a field, add
   the table entry in the same edit.** Two stale entries already exist with no backing field —
   `Location`'s table claims `gps`/`address`/`state`/`area_code`/`id`/`updated_at` while the
   record has only `city` and `country`, so "Location has gps" is false.
2. **`IssueReq`'s context field is `context`, not `issueReqContext`** (`Types.hs:828-833`) — the
   only Req type in the file that breaks the convention. Access is `req.context`.
3. **`on_issue_status` reuses `IssueReqMessage`** — there is no `OnIssueStatusReqMessage`. And
   `issue_status`'s entire message is a single `issue_id` (`Types.hs:969-972`): no order ref, no
   context echo.
4. **Three enums where `toJSON` disagrees with both `show` and their own `FromJSON`** — use
   `show`, never `toJSON`: `IssueType TYPE_ISSUE` (`toJSON` gives `"TYPE-ISSUE"`, parser accepts
   only `"ISSUE"`), `ComplaintAction OPEN_ISSUE` (→ `"OPEN-ISSUE"` vs `"OPEN"`), and
   `RespondentInfo RESPONDING_INTERFACING_NP` (→ `"RESPONDING-INTERFACING-NP"` vs
   `"INTERFACING-NP"`). Existing ACL code already uses `show`; match it.
5. **`IGM.Utils.ack` hardcodes `status: "200"`** (`Utils.hs:73`) — the same defect we already
   rejected in the FRFS helper, so **do not use it**; reuse the seller's `sellerAck` shape. There
   is also **no `nack` helper** in the spec lib, so a rejection path needs one written.
6. **Classification fields are `Maybe Text`, not the enums** — `issueCategory`, `issueSubCategory`,
   `issueIssueType`, `issueStatus`, `issueRating` are all untyped while typed enums sit unused in
   `IGM/Enums.hs`. Conversion is a runtime `decode . encode` that throws on an unrecognised
   string. **The compiler will not catch a bad category.**

**Required fields when building outbound** (these *will* fail to compile, so know them up front):
`Issue` needs `issueId`, `issueCreatedAt`, `issueUpdatedAt` — everything else is optional;
`IssueResolution` needs `issueResolutionActionTriggered` **and** `issueResolutionShortDesc`;
`ResolutionProvider` needs a non-`Maybe` `respondentInfo`; `GRO` needs `gROGroType`;
`IssuePost200ResponseError` needs `code` and `type`. Note that error type uses `"path"`
singular, while the `Error` type in `AckResponse` uses `"paths"` plural — easy to grab the wrong
one.

**FRFS is limited to 11 subcategory codes**: `metroSubcategories` (`Enums.hs:295-309`) =
`ORD101`, `FLM101-104`, `PMT101-106`. Anything outside that set needs a new enum constructor
**plus** a `mkBeamInstancesForEnumAndList` regeneration and probably a migration wherever it is
persisted — the highest-blast-radius change in this group. `IssueSubCategory` also has a
zero-field vestigial namesake in `IGM/Types.hs:993`, which is why existing imports read
`import qualified IGM.Types as Spec hiding (IssueSubCategory)`.

**Also note `validateCoreVersion` hardcodes `"1.0.0"`** (`Utils.hs:55-60`) and rejects anything
else — worth checking against what our seller context actually publishes (`2.0.0` for FRFS)
before assuming inbound validation passes.

**How to verify.** Extend the harness with a signed `issue` for a confirmed booking; assert an
`on_issue` callback, a persisted issue linked to the booking, and that a **redelivered** issue
with the same `issue_id` does not create a second row — that last one is the check Go would
fail.

---

### 4.4 `info` → `on_info` — entity disclosure — DONE (`85b367a4e4`)

**What it is, and it is not what the name suggests.** Not catalogue info, not order info. `info`
is ONDC's **entity/KYC disclosure**: the buyer asks who we are legally, and `on_info` answers
with the seller's own registration details — GST (legal entity name, business address, city
codes, GST number), PAN (name as per PAN, PAN number, date of incorporation), the authorised
signatory (name, address, mobile), country and email. `message.info.type` is the literal
`"BPP"`.

**It reads nothing from the database.** `TransformMetroOnInfoRequest`
(`metro_transformer.go:1229-1258`) builds the whole message from
`AppConfig.TriffyDigitalInfoApiDetails` — every field is config. The inbound `info` body is not
consulted beyond its context; Go caches the context, then builds the answer from static values.
That makes this the smallest remaining seller message by logic and the most sensitive by
content.

**Go's flow** (`metro_controller.go:696`, `metro_manager.go:786`): validate signature → validate
structure → ACK → async → build `on_info` from config → POST to the buyer's `bap_uri`. The same
ack-then-fork shape as everything else.

#### The blocker, and it is the RSF one again

**`Kernel.Types.Beckn.Context.Action` has no `INFO` or `ON_INFO` constructor**
(mobility-core, `Context.hs:72-96`). `Tools.SellerSignatureAuth` parses the last path segment
into that type, so `info` mounted there dies with `Could not parse api name` **before the
handler runs** — identical to how `receiver_recon` failed in §4.5. Adding the constructor is a
mobility-core change and a `flake.lock` bump.

**The fix already exists: reuse `Tools.RsfSignatureAuth`.** It is `SellerSignatureAuth` minus
the action parse, with the subscriber type stated (`BAP` — `info` comes from a buyer) and the
domain as a type parameter. `info` needs exactly that.

One thing improves on the RSF case: **the domain is correct here rather than assumed.**
§4.5 flags `PUBLIC_TRANSPORT` as unverified because RSF really travels on `ONDC:NTS10` and
`Domain` models no such constructor. `info` is TRV11, and `PUBLIC_TRANSPORT` is the registry
domain rider-app already uses for FRFS — so that open question does not apply. The module name
is now misleading, though: it serves settlement *and* entity disclosure, and neither is "RSF"
as a category. Rename or re-document it when mounting.

#### Where the entity values live

**Not in code, ever.** These are real GST and PAN numbers. Go keeps them in per-environment
config files; the Haskell equivalent should be a per-merchant DB value seeded per environment,
so a dev box holds obvious placeholders and nothing legal is ever committed.

`beckn_config` is the natural home and has the precedent: `paymentParamsJson :: Maybe Text`
already stores a settlement-config blob on the same row that holds the seller's subscriber
identity. Add one nullable sibling — `sellerEntityInfoJson :: Maybe Text` — parsed into a record
at send time. One column, no new table, and `on_info` is simply not answered when it is unset
(the same "fail loudly rather than publish a placeholder" posture `init` takes on settlement
terms, `test_missing_settlement.sh`).

#### `/on_info` is dead for us, and should not be mounted

Go mounts `POST /on_info` (`metro_seller_routes.go:60`) and the handler stores and caches the
body (`metro_manager.go:831`). That is the SELLER receiving somebody else's entity disclosure —
which only happens if we sent an `info`, and nothing in the seller path ever does. Mounting it
would create a receiver for a message nothing solicits, on a route whose only effect is to write
a row nothing reads. Leave it unmounted and say so, exactly as `on_issue_status` is (§4.3).

#### Build order

1. **Wire types.** beckn-spec has no `Info`/`OnInfo` anywhere — net-new, but small: context +
   `message.info.{type, entity{gst, pan, ...}}`. Inbound `InfoReq` can be almost entirely
   optional (we read only the context); outbound `OnInfoReq` needs `ToJSON`.
2. **Config column.** `sellerEntityInfoJson` on `BecknConfig.yaml`, regen with
   `, run-generator --skip-update`, apply the migration locally, seed a placeholder row.
3. **ACL** `Beckn/ACL/FRFSSeller/OnInfo.hs` — config record → `OnInfoReq`, callback context
   built not echoed (timestamp/ttl/version ours), same as every other seller callback.
4. **Route** under `RsfSignatureAuth 'Domain.PUBLIC_TRANSPORT`, claim-and-fork via
   `H.claimOnce`, then `CallBAP.sendOnInfo`.
5. **Test** `test_info.sh` — assert the callback is signed, carries `type: "BPP"`, echoes the
   buyer's `transaction_id`/`message_id`, and that an unset config yields no callback rather
   than one full of nulls. Falsify each.

**Verdict on priority: not a cutover blocker.** Nothing in the booking, status, cancel, IGM or
settlement paths depends on it, and a buyer that never asks never notices.

**Built as planned, with one addition worth carrying forward.** The build order above held; the
only surprises were in the harness rather than the code, and both are general:

- **`WHERE domain='FRFS' AND vehicle_category='METRO' LIMIT 1` returns the wrong row.** The dev
  database holds a dozen FRFS/METRO `beckn_config` rows across unrelated merchants
  (BRIDGE_CABS, ANNA_APP, BHARAT_TAXI...), so `LIMIT 1` silently hands back somebody else's cab
  subscriber. Pin seller config by joining `merchant` on the operator short id.
- **Redis is a CLUSTER, so `--scan --pattern` misses keys.** `SCAN` iterates one node; the
  `beckn_config` cache key routinely lives on another. A sweep reports success, leaves the
  cache warm, and the next assertion accuses correct code of a bug — which is exactly what
  happened here. Build the exact key
  (`app-backend:CachedQueries:BecknConfig:MerchantId:<id>:Domain:FRFS:Vehicle:METRO`) and
  `DEL` it. This is the case-sensitivity lesson from §2 in a second disguise.

Also seeding a `beckn_config` column now changes the generated `beckn_config.sql`, which the
dev DB records a checksum for — startup prints `Fail: beckn_config.sql expected ... hash was
...`. Update `atlas_app.schema_migrations.checksum` for that filename, or the app refuses to
come up cleanly.

### 4.5 Settlement / RSF — `receiver_recon`, `on_settle`, `on_report`

**Stage 0 is DONE (`a782b86552`): seller `confirm` now writes the `frfs_recon` row.**
Findings that change the rest of this section:

- **No migration was needed.** `entity_type` and `recon_status` are `text` in the deployed
  schema, and every one of the 13 NOT NULL columns is available at confirm. The section's
  worry about a seller `entity_type` constructor was moot — `FRFS_TICKET_BOOKING` is honest
  for a seller row, and direction is already recoverable via `Common.isSellerRider`.
- **One row per ORDER, not per ticket.** The buyer half writes one per ticket
  (`Beckn/FRFS/OnConfirm.hs:332`); copying that would leave `receiver_recon` with N balances
  for one order. Go writes one. Asserted in `test_confirm.sh` step 5.
- `frfs_recon.id` is `varchar(36)` like everything else — `Common.sellerReconId` UUIDv5s the
  operator order id, which also makes one-row-per-order a property of the key.

**Stage 1 DONE (`1a4be03280`)** — `BecknV2/NTS10/{Types,APIs}.hs`, 17 test cases. Two
corrections to this section:

- The shape `receiver_recon` actually binds is `ReceiverReconPostRequest` →
  `orderbook.orders[]` of `RSFOrder`, i.e. the validation DTO's shape — **not** the
  `Rsfv2ReconRequest` discrepancy shape that also exists in Go's DTO package. Building the
  latter would have been a plausible dead end.
- `ReconContext.city` accepts **either** `"std:0484"` or `{"code":"std:0484"}`
  (`flex_recon.go`), a second tolerance beyond `FlexAmount`. A context that fails to parse
  kills the request before the orderbook is read.

**Stage 2 DONE (`fd206d5510`)** — `Tools/RsfSignatureAuth.hs`, mounted as a sibling tree.
Verified that a signed `receiver_recon` reaches the handler, and that swapping back to
`SellerSignatureAuth` reproduces the predicted `Could not parse api name`. Servant falls
through from the seller tree to the RSF tree when the leaf does not match — path matching
precedes the auth check, which was not obvious.

**⚠ Two things Stage 3 must resolve before this can ship:**

1. **The registry domain is unverified.** `authCheck` feeds it into the lookup that finds
   the sender's key. We pass `PUBLIC_TRANSPORT` because `Domain` has no NTS10 constructor.
   If a settlement agency does not resolve under it, every inbound settlement 401s.
2. **Go does not verify these routes at all** — registered bare, no middleware, no check in
   the handler. Mounting under any auth is therefore STRICTER than the service being
   replaced. Weigh that deliberately rather than discovering it at cutover.

**Stage 3 DONE (`92c5cf7982`)** — `receiver_recon` applies settlements. Four things it
settled that the rest of this section was written without:

- **An order is settled by exactly ONE payment.** Confirmed with the product side, and it
  is the fact the whole design rests on. `frfs_recon` has a single
  `settlement_reference_number`, so it remembers only the LAST reference applied — enough
  for one payment and nothing else. **This retires the `frfs_seller_settlement_event` table
  proposed above.** It was built and backed out once the rule was confirmed; the table is
  only needed if instalments are ever allowed.
- **The status guard protects the ledger; the reference check does not.** Only `PENDING` is
  settleable, so a redelivery is refused before its reference is ever compared. The
  reference check decides the ANSWER — "already applied" vs "refused" — which is a real
  distinction, becomes the Stage 3b callback payload, and is worth keeping, but it is not
  what stops a double-subtract. Getting this backwards is easy: the original code and its
  test both claimed the opposite.
- **The suite's replay assertion was vacuous, and only falsification found it.** Deleting
  the reference check left every step passing, because the status guard refused the replay
  anyway. `test_recon.sh` step 2 now also asserts the log line that separates the two cases,
  and goes red when the check is removed. This is the fifth vacuous pass in this work — the
  pattern is always the same: two guards where one is enough for the observable, so the
  test cannot see which one fired.
- **Widening `settleable` to `PARTIALLY_SETTLED` is a trap with a measurement.** It looks
  like the way to let a part-paid order be completed. Settle under A then B, redeliver A:
  it no longer matches the stored B, passes the status check, and is applied again — a 120
  order driven to **-10**. Both `Recon.hs` and `test_recon.sh` say so at the point of
  temptation. Do not do it without the event table.

**Stage 3b DONE (`04d055e615`)** — `on_receiver_recon` goes back, signed, one verdict per
order. `receiver_recon` also moved to claim-and-fork (ack first, settle after) like the other
seller messages. Three wire details, each taken from Go and each falsified here:

- **`counterparty_diff_amount` is ABSOLUTE**; the sign lives in the status code. Only an
  OVER-payment exercises this — our balance is positive when the collector still owes us, so
  a shortfall renders identically with or without the `abs`. The suite grew an overpaid case
  for exactly that reason; before it, the `abs` was asserted by nothing.
- **`core_version` is `1.0.0`**, not the `2.0.0` the inbound body carries and not the `2.0.0`
  the order messages use.
- **`collector_app_id` is theirs, `receiver_app_id` is ours** — adjacent `Text` fields, so a
  transposition compiles and reads as though the collector were the seller.

`sendCallback` is now polymorphic in its response type; it had been pinned to the FRFS
`AckResponse` while the signing path underneath always was polymorphic.

**Stage 4 — `on_settle` — is deliberately NOT applied, and this is the finding that closes
§4.5.** `on_settle` is the callback half of a loop we never start:

- Go's outbound `settle` comes from `InternalOutSettle` (`metro_manager.go:1057`), driven by
  **`PUT /sendSettlementData`** (`public_transport_router.go:46`) — an internal ops route,
  **not one of the 14 seller routes**, reading a `pt.recon_log` table this migration does not
  have, and posting to NPCI's own base URL under a dedicated user agent.
- So nothing we do produces an `on_settle` addressed to us. Applying one would mean writing
  NPCI settlement state — Go keeps **seven `npci_*` columns** on `pt.settlement`, and
  `frfs_recon` has none of them — for a message that cannot arrive.
- **The obvious shortcut is a trap.** Folding the NPCI reference into
  `settlement_reference_number` would overwrite the exact field the `receiver_recon` replay
  guard compares against, silently re-enabling the double-subtract. It is a column that looks
  available and is not.

Read the split this way: **the RECON loop is complete** (`receiver_recon` in,
`on_receiver_recon` out, money applied). The **NPCI settlement loop** is a different
counterparty with a different trigger, and it begins with an outbound sender that is out of
seller scope. `on_settle` stays mounted, authenticating and logging at WARN that it was
acknowledged and ignored; unmount `OnSettleAPI` at cutover unless the sender lands with it.

**Deferred, with reasons** (raised by review, not blockers):

- **Seller `frfs_recon` rows are indistinguishable from buyer rows.** Both write
  `entity_type = FRFS_TICKET_BOOKING`, but the buyer writes one row per TICKET with
  `difference_amount = NULL` and the seller one row per ORDER with a live balance. No code
  reads either column today, so nothing is broken — but any cross-cutting query over
  `frfs_recon` silently mixes them. The seller constructor is a YAML enum change plus a
  regen, as this section originally proposed.
- **`FrfsTicket.yaml:857` still documents `differenceAmount` as `# Nothing`**, which is the
  buyer's meaning. The shared definition site now describes only half its writers.
- **`FrfsTicket.yaml` has TWO `constraints:` blocks for FRFSRecon** (lines 866 and 891).
  Duplicate YAML key: the first is silently dropped. Pre-existing, and a live trap for
  anyone adding a constraint there.


**⚠ These are not ONDC:TRV11. RSF carries domain `ONDC:NTS10`** (`integration_constant.go:45`,
enforced `on_settle_validation.go:11`). TRV11 is the *order* domain. That distinction has teeth:

**They cannot be mounted under `SellerSignatureAuth`.** It parses the **last path segment** into
`Context.Action` (`Tools/SellerSignatureAuth.hs:95-97`), and `receiver_recon` / `on_settle` /
`on_report` are not constructors of it. The request dies with
`InternalError "Could not parse api name"` **before the handler runs**. Worse, `authCheck` takes a
`Domain` for the registry lookup and `Kernel.Types.Beckn.Domain` has **no NTS10 constructor**.
Fix without a `flake.lock` bump: a sibling combinator in `Tools/` that supplies `SubscriberType`
and `Domain` directly and skips the action parse — `SellerSignatureAuth` is already documented as
deliberately "a thin shell over `SA.authCheck`" for exactly this class of change.

**And beckn-spec has no RSF surface at all** — no `ReceiverReconReq`, `OnSettleReq`,
`OnReportReq`, no API proxies, nothing NTS10 anywhere. The whole wire layer is net-new. Note
amounts arrive as **string *or* bare number** (Go's `FlexAmount`), so the parser must accept both.

**`SettlementReportIngestion.hs` is not RSF and is not reusable.** It ingests **Juspay
payment-gateway** settlement reports into finance-kernel on a daily job. Different problem.

**Table verdict: reuse `frfs_recon` for state, add exactly one table for application history.**
`frfs_recon` is the only ONDC-shaped recon table and every NOT NULL column is available at seller
confirm; `entity_type` is plain text so a seller constructor is a YAML regen with no SQL. But it
has `PRIMARY KEY (id)` only — no unique business key — and holds one row per booking, i.e. state,
not history. Go's idempotency **is** a unique index (`UNIQUE(order_id, settlement_ref_id)` on
`pt.settlement_history`, deliberately consumed as a caught duplicate-key error). So add
`frfs_seller_settlement_event` with `UNIQUE(network_order_id, settlement_reference_number)`,
listed in `disableForKV`, and keep `frfs_recon` as pure state.

**A UNIQUE index alone is not sound here.** `disableForKV` is runtime config in `system_configs`,
not schema — someone can KV-enable the table without a migration, and then the violation is found
by the *drainer* after we already ACKed, and the row silently vanishes (§2.3 again). So the guard
must also be **logical**: apply `on_settle` only when the stored reference is NULL or equal to the
incoming one, only on `PENDING → SETTLED|PARTIALLY_SETTLED`, serialised by a per-order Redis lock.

**⚠ Seller `confirm` writes no recon row today.** Go creates its `pt.settlement` row at confirm
with `difference_amount = the full amount, status = PENDING`, and that row is what
`receiver_recon` reconciles against. Without adding a `QRecon.create` to
`FRFSSeller/Confirm.hs`, **every inbound order falls into the "extra order on the buyer side"
branch and is reported as not-paid.** Note the seller row differs from the buyer's on purpose:
buyer rows set `differenceAmount = Nothing`.

**Understand what recon compares before writing it.** Not "expected fare vs paid" but *remaining
unsettled balance* vs *this payment* — `difference_amount` starts as the full amount and is driven
to zero. That handles partial settlement naturally, but it means a **re-delivered
`receiver_recon` on an already-settled order computes `0 − amount` → OVERPAID**. The idempotency
record is the only thing stopping us reporting a bogus overpayment to a buyer.

**Do not port Go's `on_settle`; it has three defects.** No idempotency guard (a blind
`UPDATE … WHERE order_id`), no status-transition guard, and it defines `ValidateSettlementAmount`
with a 0.01 tolerance and **never calls it**. The controller also ACKs even when the work failed,
with a comment claiming idempotency it does not have. A settlement applied twice is a real
financial error — this is the one place where "match Go exactly" is the wrong instruction.

**Money arithmetic.** Go compares `float64` for exact equality. Ours is `HighPrecMoney`
(a `Rational`) persisted to `double precision`, with `Show` going via `Double`. **Round to paise;
never test exact equality.**

**`on_report` is log-only in Go and no outbound `report` is ever sent** — the initiator is
unimplemented. Implementing it yields a handler nothing calls unless NBBL pushes unsolicited.
**Scope question, not a task.**

**Unverifiable without NPCI/NBBL access:** whether inbound `on_settle` signatures verify at all
(the sender is the settlement agency, which must resolve in the registry under a domain we do not
model), the live `ondcnbbl.npci.org.in/nocs/v2` contract, and whether the RSF mock matches
production field-for-field. Go's transformer carries a legacy fallback branch, implying two
payload shapes have been seen in the wild.

---

## 4.7 KMRL / Kochi — a second operator, not a second route

**This is a different shape of work from everything above.** Routes 1-10 were messages on a
transport we already had. KMRL is the transport: the same ten messages, spoken to a different
operator behind a gateway rider-app cannot currently reach.

`getStationList` throws `Unimplemented!` today, and the provider sum type
(`Domain.Types.Extra.IntegratedBPPConfig`) has `CMRL | CMRLV2 | EBIX | DIRECT | CRIS` and no
KMRL constructor. But the adapter itself is the easy half — it mirrors CMRL's module set
(`GetFare`, `BookTicket`, `GetTicketStatus`, `CancelTicket`, `GetStations`) plus ~10 dispatch
branches in `ExternalBPP/ExternalAPI/CallAPI.hs`.

**Two PLATFORM capabilities are missing, and neither is adapter code:**

1. **JWE.** KMRL's payloads are a JWS(RS256) wrapping a JWE compact token
   (`RSA-OAEP-256` / `A256GCM` / `zip:DEF`) — encrypt-then-sign (`kochi_metro.go:155`).
   There is **no JOSE library anywhere in the monorepo or the pinned package set**.
2. **Outbound mTLS.** `initializeCertificate` builds a `tls.Config` with a client certificate
   and hands it to the HTTP client (`kochi_metro.go:76-95`). Nothing in rider-app sets a
   client certificate on an outbound call.

**No new dependency is needed, which is the good news.** Everything required is already in the
pinned set: `cryptonite-0.30` (RSA-OAEP, AES-GCM), `zlib-0.6.3.0` (DEFLATE),
`base64-bytestring`, and `tls-1.6.0` + `x509*` for the client certificate. So this is a code
problem, not a `flake.lock` problem.

### Slice 0 — DONE, and it was the risk

The JWE envelope is written and **proven against Go**, not against itself:
`hurl/frfs-seller/kmrl/` (outside the repo; see its README).

A self round-trip proves nothing here — any wrong-but-internally-consistent implementation
passes it. So the harness generates vectors with the **same Go libraries the live service
uses** (`lestrrat-go/jwx`, `square/go-jose`) and checks both directions: Haskell decrypts a
token Go produced, Go verifies and decrypts a token Haskell produced, and a tampered signature
is refused.

Two traps that cost real time and would have shipped silently:

- **base64url must be UNPADDED.** A trailing `=` yields a token every strict JOSE parser
  rejects.
- **The protected header is also the AAD.** Re-serialising the same JSON with different key
  order or spacing produces a token that decrypts to *garbage* rather than failing cleanly, so
  the header is a byte literal rather than a re-encode.

### Remaining slices

1. **Port the crypto into rider-app** as `ExternalBPP/ExternalAPI/Metro/KMRL/Crypto.hs`, with
   the interop vectors as a committed test rather than a scratch harness.
2. **Outbound mTLS** — a client-certificate path on the HTTP manager. Platform-level, and worth
   checking whether any other integration wants it before shaping it around KMRL alone.
3. **`KMRL` provider constructor** + config (endpoint, cert paths/material, token creds), then
   the dispatch branches.
4. **The adapter modules**, mirroring CMRL's set.
5. **Station cache + id minting** — §2.6 applies: ids are contract and derive from the
   operator's own station names, so Kochi's must be minted from KMRL's list, not GTFS.

**Not startable in parallel with a Chennai cutover** only because of shared dispatch edits;
otherwise independent. The credentials and the certificate are the long-lead item — they come
from KMRL, not from us.

## 4.6 Land a prep commit before splitting the work

All ten messages touch the same few shared modules, and four helpers are **already duplicated**
across the built four. Landing these first turns the remaining work from constant conflicts into
additive edits — this is the single highest-leverage thing to do before parallelising:

1. `nonZeroBuyerFinderFee` — verbatim in `Init.hs:181-195` **and** `Confirm.hs:572-584`, and
   Confirm's copy dropped the "unparseable is not zero" Haddock, which *is* the safety argument.
2. `settlementAccount` — verbatim in both, differing only in an import alias.
3. `formatPrice` — named once in `Confirm.hs:377`, inlined in `Search.hs`, `Select.hs`, `Init.hs`.
   One two-decimal contract that buyers string-compare, expressed four times.
4. `mkPrice` — `OnSelect.hs:186-192` redefines what `OnInit` already exports.
5. The inline `providerDescriptor` block (with its "NOT tfDescriptor" comment) is copy-pasted in
   three ACL modules; a shared `mkProviderIdentity` stops the tenth copy.
6. Move `sellerSearchId` from `Confirm.hs` to `Common.hs` — `status` and `cancel` both need it.

**Two real defects to fix in the same commit**, both introduced by this session: `OnSearch.hs:92`
cites `Domain.Action.Beckn.FRFSSeller.Search.mkJourneyId`, which no longer exists (it moved to
`Common.journeyIdFromStationNames`); and `API/Beckn/FRFSSeller/Select.hs:15-18` has Search's
module header spliced into Select's mid-sentence — a merge artifact.

**One behavioural question to settle first, because the next two messages will copy whichever
pattern they see.** `Search.hs:81-85` deliberately takes `now` **after** the upstream calls, with
a comment explaining that an earlier timestamp back-dates `context.timestamp`, burns part of the
PT300S TTL, and can trip a BAP freshness check. `Confirm.hs:125-127` takes it **before** the
operator call. The TTL half of the argument is weaker for `on_confirm` (tickets are not an offer)
but the freshness half applies identically. `status` and `cancel` both call the operator — decide
explicitly rather than by copy-paste.

---

## 5. Cutover blockers that are not code

These do not depend on how many messages are built, and at least one is on the critical path.

- **ONDC registry / subscriber identity.** Post-migration rider-app is both BAP and BPP on
  ONDC:TRV11. Flagged as *the* blocking decision when the migration was specced, and still
  open. Go serves `/onboarding/on_subscribe` and `/ondc-site-verification.html`; rider-app
  appears to serve neither.
- **Real `beckn_config` values.** Dev holds `localhost:8013/...` and `xyz@upi`/`xyz` for the
  settlement account. `init` correctly refuses to publish an `on_init` without them
  (`test_missing_settlement.sh`), so incomplete cutover config fails loudly rather than
  silently — but it does fail.
- **`provider_name` still emits `chennai_metro`** rather than the registered name. It comes
  from `integratedBPPConfig.providerName`, falling back to `agencyKey`, and the dev row leaves
  `providerName` null. The fix is to seed that column. Do **not** reach for
  `FRFSUtils.getProviderName` (`SharedLogic/FRFSUtils.hs:143`) as the shortcut: when
  `providerName` is null it returns the literal `"Chennai Metro Rail Limited v2"` for CMRLV2,
  and publishing an internal adapter version to the network is worse than the current
  placeholder.
- **KMRL is unimplemented.** `getStationList` throws `Unimplemented!` for it, and it needs
  per-operator signing keys. Chennai-only until then.
- **CMRL egress allowlist.** Go calls CDAC from the metro cluster. rider-app runs elsewhere
  (GKE `atlas`), so the source IP changes at cutover. The pre-prod box turned out not to be
  allowlisted at all, so this is *unconfirmed* for prod — worth checking with CMRL before
  cutover rather than discovering it during.
- **Open question for the CMRL integration owner:** does CMRL validate or reconcile on
  `fareQuoteId`? We now carry the real one quote-to-book, but the long-standing fallback sent
  our booking id and was demonstrably accepted, so it is not validated today.
- **Two data observations to raise with CMRL:** `New Washermenpet Metro` (`SNW|0147`) and
  `Washermanpet Metro` (`SWA|0101`) report identical coordinates; and the pre-prod auth
  endpoint takes credentials over plain HTTP on a publicly reachable IP.
