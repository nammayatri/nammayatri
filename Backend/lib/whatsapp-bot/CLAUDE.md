# CLAUDE.md — `whatsapp-bot`

Scoped to `Backend/lib/whatsapp-bot/` and its rider-app adapters. Everything you
need to pick this work up cold. Instructions here **override** default behaviour
for this package; the repo-root `CLAUDE.md` still applies for everything else.

---

## 1. What this package is

A **pure, handle-parameterized Haskell port** of a TypeScript WhatsApp booking
bot. A rider messages a WhatsApp number, shares a location pin, and gets a taxi.

Three layers:

```
shared-kernel  Kernel.External.Meta          Meta/WhatsApp wire primitives
      ↓
lib/whatsapp-bot                             THE PURE ENGINE — this package
      ↓
rider-app  Main/src/WhatsappBot/Adapter/*    Flow-monad implementations of the handles
```

**Source of the port:** `ny-connectors@77325a7` (2026-07-09). Every `*.ts:NNN`
citation in this package resolves against **that revision and nothing at
ny-connectors HEAD** — HEAD is +747/−321 past the port basis and commit
`5cfdc7a` re-architected the flexi flow. `migration/TS-DRIFT.md` has the diff.

### The single most valuable property — do not weaken it

> **`BotEnv m` is `Monad m =>` ONLY.**

Every effect — backend calls, outbound sends, session/person stores, the ride
registry, time, delay — goes through a handle in `BotEnv`. Never add `MonadIO`,
`MonadThrow`, `MonadReader`, or any other constraint to the engine.

That constraint is what lets the *same engine code* run in `IO` over `IORef`s
under test and in rider-app's `Flow` in production. If you think you need a
constraint, you need **a new field in a handle** instead.

---

## 2. Module map (post-refactor)

```
src/WhatsappBot/
  Engine.hs        336  ROUTER ONLY: handleMessage, the 26-arm intercept chain,
                        stateSwitch, handleIdle, language, more/help/support
  Flow/Booking.hs  631  The booking flow — how a booking gets CREATED.
                        Entry/pickup capture, flexi search, regular search,
                        the four poll loops, and the global menu row.
  Ride.hs          435  FLOW-AGNOSTIC ride lifecycle — cancel, status, tracking,
                        SOS, mark-safe, call-driver, registration. Knows about
                        bookings, NOT about how one was created.
  Env.hs           263  BotEnv/BotConfig + conversation primitives + ensureAuth
  Types.hs         268  FlowState (12 ctors), FlowContext (15 fields), DTOs, codecs
  Handles.hs       126  The six port records
  Cities.hs        195  Hardcoded city table + geofence — DO NOT TOUCH
  Messages.hs      155  Driver card / arrived / started / ended / cancelled
  Tracker.hs       155  Background poller pushing ride-stage updates
  Inbound.hs        63  Meta envelope → InboundEvent
  Util.hs           30
  I18n.hs, I18n/Types.hs, I18n/{En,Hi,Gu,Kn,Ta,Te}.hs, I18n/Detect.hs
```

### Dependency direction — must stay acyclic

```
Engine → {Ride, Flow.Booking, Env}
Flow.Booking → {Ride, Env}
Ride → {Env}
Env → {Handles, Types}
```

**Nothing imports `Engine`** except `Adapter/Env.hs` and the test harness, both
only `(handleMessage)`. **`Ride` must not import `Flow.Booking`.** **`Env` must
import neither.** If a change seems to need one of those edges, the design is
wrong — don't force it.

**A second WhatsApp flow** sits beside `Flow/Booking.hs` as
`Flow/<Name>.hs`, reuses `Ride.hs` and `Env.hs`, and does **not** import
`Flow.Booking`. That is the whole point of the decomposition.

---

## 3. Build and test — READ THIS BEFORE RUNNING ANYTHING

### Verify the REAL cabal exit code, never the nix wrapper's 0

`nix develop … --command` exits 0 even when cabal inside it failed. **Always**
use the `; echo CABAL_EXIT $?` form, with the echo *inside* the bash string:

```bash
# Build the library
cd /Users/mitran/Documents/voice/nammayatri && nix develop .#backend --command bash -c \
  'cd Backend && cabal build whatsapp-bot; echo CABAL_EXIT $?'

# PRODUCTION flag set — a local build is WEAKER, see below
cd /Users/mitran/Documents/voice/nammayatri && nix develop .#backend --command bash -c \
  'cd Backend && cabal build whatsapp-bot --flags="-Local"; echo CABAL_EXIT $?'

# Adapters must still link (slow — several minutes)
cd /Users/mitran/Documents/voice/nammayatri && nix develop .#backend --command bash -c \
  'cd Backend && cabal build rider-app:exe:rider-app-exe; echo CABAL_EXIT $?'

# Regenerate the .cabal after adding/removing any .hs file
cd /Users/mitran/Documents/voice/nammayatri && nix develop .#backend --command bash -c \
  'cd Backend && , hpack; echo HPACK_EXIT $?'
```

When piping through `tail`, use `${PIPESTATUS[0]}` instead of `$?`.

`CABAL_EXIT 0` is the only acceptable result. **Builds take many minutes — that
is normal.** Use a 600000 ms timeout; never conclude a failure from slowness.

### Gotchas that will cost you an hour each

| Gotcha | What happens |
|---|---|
| **Nix resets the working directory** | Every nix command must `cd Backend` *inside* the bash string. |
| **`cabal.project` sets `flags: +Local`** | Adds `-Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-record-updates -O0`. **A local green build is weaker than production.** Never claim "it compiles" without saying which flag set. For record-field changes, `--flags="-Local"` is the gate that matters. |
| **`ormolu` 0.1.4.1 in this devShell BREAKS `RecordDotPreprocessor`** | It rewrites `en.welcome` → `en . welcome`, silently corrupting the package. **Never run a formatter on this package.** |
| **`.cabal` is hpack-generated** | Never hand-edit `whatsapp-bot.cabal`. Add/remove a `.hs` file → run `, hpack` (the leading comma *is* part of the command name). |
| **A pre-commit hook runs hpack** | Adding a file rewrites `whatsapp-bot.cabal` mid-commit and **aborts the commit**. Stage the regenerated `.cabal` alongside your change. |
| **`-Werror` is on** | `-Wall -Wcompat -Werror -Widentities -Wunused-imports -Wwarn=ambiguous-fields`. An unused import is a build failure. Moving code between modules will orphan imports on *both* sides. |
| **`Kernel.Prelude`, not `Prelude`** | |
| **GHC 9.2.7** | Not 9.6. Do not trust a snippet validated on another GHC. |
| **Never edit `src-read-only/`** | NammaDSL-generated, anywhere in the repo. |

`cabal.project:79-80` carries `package whatsapp-bot / tests: True` — scoped
deliberately, **not** project-wide: a top-level `tests: True` enables
`lib/location-updates`' suite, which needs `tasty-hspec`, absent from the pinned
nix set, and the cabal **solver** fails before anything builds.

---

## 4. Conventions that are not negotiable

- **Wire strings are byte-identical contracts.** Button ids, session keys, and
  persisted JSON field names are contracts with Redis and with WhatsApp. A
  rename is a **breaking change, not a refactor**. `renderVerb`-style emission
  must produce exactly what the engine emits today.
- **Codecs stay co-located with their types.** A `ToJSON`/`FromJSON` instance in
  a module separate from its type is an orphan, and this repo's remedy for
  orphans is a `-Wno-orphans` pragma — which is exactly the "weaken a convention
  to make the change fit" move that is forbidden. Instances for `FlowContext`,
  `Session`, `FlowState` stay in `Types.hs`.
- **Never weaken a convention to make a change fit.** Find the cooperative fix.
- **Keep every `*.ts:NNN` citation attached to its function** when moving code.

### Redis is delete-on-decode-failure — this is a data-loss footgun

`Adapter/SessionStore.hs` reads the whole `Session` through `Redis.get`. In
shared-kernel, `get = safeGet` and `safeGet key = get' key (del key)` — **a
decode failure DELETES the key.** `wab:user:*` (`StoredPerson`) is written with
**no TTL**, so a decode failure there is *permanent data loss*.

Therefore: **a persisted type whose decoder can fail is a data-loss bug**, not
an inconvenience. Note `FromJSON SupportedLanguage` calls `fail` on an unknown
code.

**What is safe:** `FlowContext` derives bare
`deriving (Show, Eq, Generic, ToJSON, FromJSON)` under `DeriveAnyClass` — no
custom `Options`, no `rejectUnknownFields`. So aeson ignores unknown keys and
treats an absent key as `Nothing` for `Maybe` fields. **Adding or removing a
`Maybe` field is wire-safe in both directions and needs no keyspace bump.**
Renaming or retyping one is **not**.

---

## 5. State of the work

Branch `whatsapp-booking`. The refactor decomposed a 1227-line `Engine.hs`
monolith so a second WhatsApp flow can be built beside the booking flow.
**Behaviour-preserving throughout** — verified against a golden-replay
equivalence oracle at every step (see §6 for why that oracle is no longer in
the tree).

| Done | What |
|---|---|
| ✅ | Pinned the TS source revision (`77325a7`) in `Engine.hs` + `package.yaml` |
| ✅ | Enabled + fixed the test suite; **zero `-Werror` fallout**, all fixtures passed first try |
| ✅ | Fixture-set guards + a CI job scoped to this package |
| ✅ | Made the oracle see the persisted schema (JSON round-trip on every save) |
| ✅ | Widened goldens from 6 → 18 fixtures, exported from the pinned TS revision |
| ✅ | Moved conversation primitives + `ensureAuth` into `Env.hs` |
| ✅ | Extracted `Ride.hs`; named the two ride-resolution policies |
| ✅ | Extracted `Flow/Booking.hs`; **`Engine.hs` 1227 → 336, router only** |
| ✅ | Deleted five write-only `FlowContext` fields (20 → 15) |

### Not done — pick up here

1. **Extract `Poll.hs`** — the four poll loops in `Flow/Booking.hs`
   (`pollFlexiQuotes` :305, `pollFlexiDriver` :320, `pollEstimates` :449,
   `pollRegularBooking` :511) share one shape. Factor out
   `pollFor :: Monad m => Int -> m () -> m (Maybe s) -> (s -> m (Maybe a)) -> (Int -> m ()) -> m (PollOutcome a)`.
   **See §7 — this task has a trap no test can catch.**
2. **Extract `Verb.hs`** — typed button ids, replacing four hand-computed
   offsets (`T.drop 10`, `T.drop 14`, …) and two different parsers for one id
   shape. **See §7 for three claims about it that are FALSE.**
3. **`PROVENANCE.md`** — a **symbol-keyed** TS→Haskell index. Line-keyed
   citations are stale at birth: drift from the pin is ~0 at the top of
   `engine.ts`, **+128** at the bottom, and *bidirectional*, so no constant
   re-base repairs it. **TS method names survived the churn; line numbers did
   not.**
4. **Spike two flows, keep neither** — the falsification step. Write the
   prediction *and a numeric pass threshold* down **before** spiking, or it
   isn't a threshold. A hotspot-OTP ride is by definition **not dispatched**, so
   the claim "a second flow needs zero new `BackendHandle` methods" is probably
   false.

---

## 6. The oracle — removed from the tree, recoverable from history

A golden-replay suite (`test/src/GoldenReplay.hs`, `test/src/CodecSpec.hs`, 18
JSON fixtures) validated every step of this refactor: **40 tests green through
all nine tasks.** It has been **deliberately removed from the PR head**. It is
fully recoverable:

```bash
# Restore the whole suite
git checkout <commit-before-the-removal> -- Backend/lib/whatsapp-bot/test
# Then re-enable it
#   cabal.project already has `package whatsapp-bot / tests: True`
```

**If you resume this refactor, restore it first.** Tasks in §5 were each proven
behaviour-preserving *by* this suite; continuing without it means the next
change is unverified.

### How it worked, and what it could NOT see

It fed each fixture's webhook payload through the *real* decoder, ran the *real*
`Engine.handleMessage` against mock handles recording every call, then fired
`Tracker.trackerTick`, asserting two **ordered** lists as deep-equal aeson
`Value`s.

**Blind spots — every one of these is load-bearing:**

- It recorded **only `BackendHandle` methods**. Never `SessionStore.getContext`,
  never session or person *writes*.
- It projected only `kind`, `to`, `merchant`, `buttons`, `link` from outbound
  messages — **message copy was never asserted**.
- It **could not see `FlowContext` field deletions at all.** Measured: zeroing
  all five deleted fields together left all 40 tests green. `CodecSpec`'s pinned
  blob couldn't catch it either — aeson ignores unknown keys, so both sides of
  the comparison shrink together. **For a field deletion, the oracle is the GHC
  type checker**, which is sound and total: removing a field removes its
  `HasField` instance, so every `ctx.field` read fails to compile under
  `-Werror`.
- It could not see `getBookingDetails`' auth argument (the harness dropped it).

**Green tests never proved a move was verbatim.** That was always established by
reading the diff.

---

## 7. Traps in the remaining work

### `Poll.hs` (task 1 above)

1. **The pre-check MUST run BEFORE the probe on every attempt, including attempt
   0.** `pollFlexiDriver` reads the session context first, and *that read is the
   cancel-abort check*. The oracle recorded only `BackendHandle` calls, never
   `SessionStore.getContext` — so moving the read after the probe changes the
   cancel race window and **breaks no test**. That is what the `m (Maybe s)`
   pre-check parameter is for. Do not "simplify" it.
2. **`flexiQuotePollAttempts = 10` is pinned** by `driver-not-found.json` (it
   asserts `getFlexiQuotes` exactly 10 times). But `driverPollAttempts = 90` and
   `driverPollNotifyEvery = 15` have **zero** coverage — an off-by-one there is
   invisible.
3. **`pollFlexiQuotes`/`pollEstimates` return `Either BotError [a]` and
   distinguish `Left` from `Right []`.** A `PollOutcome` with no error
   constructor collapses them. That is observationally safe *only* because both
   callers route to `flexiNoAuto` — verify it against the call sites, and say so
   in the commit message.
4. Refold **one loop at a time, running the suite after each.** Three folded
   loops and one honest exception beats four folded loops and a changed race
   window.
5. `Poll.hs` should import **nothing but `Kernel.Prelude`**. It is a pure
   control-flow combinator. Wanting to import `Env`/`Types` means the
   abstraction is wrong.

### `Verb.hs` (task 2 above) — three claims that are FALSE

Do not let these appear in code, comments, or commit messages:

1. ~~"A forgotten arm fails the build."~~ `runEngine` **must keep**
   `| otherwise -> stateSwitch`: six arms carry `ctx` guards that fall through
   by design, and `__location_pin__` must fall through when the state is
   `AwaitingRegularDrop`. **No exhaustiveness check ever runs on routing.**
   `Verb.hs` is hygiene, not a proof.
2. ~~"No rendered verb contains a trigger substring."~~ Unsatisfiable:
   `renderVerb VStatus` must be `"status"` and `renderVerb RvBook` must be
   `"book"`, and both *are* trigger elements. Pin the known-intentional overlap
   set instead, so a *new* verb joining it fails.
3. ~~`RvBook` and `RvDrop` are router verbs.~~ They are not. `"book"` is matched
   by `T.isInfixOf` **inside `handleIdle`**; `regdrop:` is consumed state-scoped
   **inside `handleConfirmingRegularDrop`**. Routing them from the chain would
   be a **behaviour change**.

When rewriting the chain, preserve **exactly**: the textual order of the arms,
every extra `ctx` guard on every arm, first-match-wins, and the trailing
`| otherwise -> stateSwitch`.

---

## 8. Landmines already found — don't re-derive these

- **`flexiNoAuto` is called five times from the *regular* path** despite its
  name. **Do not rename it** — behaviour and wire strings stay fixed.
- **`handleFlexiEndOtp`'s `registry.getRide` is an IDOR guard, not a ride
  lookup.** It authorises a *user-typed* booking id (gated by `isValidBookingId`
  and `owned.userKey == uk`). **Do not route it through `currentRide`** — that
  would change what is authorised.
- **Two ride-resolution policies exist deliberately, and must not be merged.**
  `currentRide` is ledger-first (`RideRegistry`, written atomically via `SET
  NX`), returning `BotAuth` from the ledger entry. `listedBookings` is the
  backend-listing path. `cancel-mid-search` pinned
  `[searchFlexi, getFlexiQuotes, getActiveBookings, cancelRide, confirmQuote]`
  with **no ride registered** — a ledger-first lookup returns `Nothing` there
  and skips the cancel entirely.
- **`listedBookings` takes a `BookingWindow` ADT** (`SinceExactly` /
  `SinceSelectStartOr24h` / `SinceSelectStart`) because three different
  `createdAfter` policies exist across its call sites. **They are not
  interchangeable.** The name says *how bookings were fetched*, not whether they
  are registered — 3 of its 5 call sites run *after* registration.
- **`menuRow` is the bot's global menu but lives in `Flow/Booking.hs`**, because
  it hardcodes the booking wire strings `ride_type:flexi` / `ride_type:regular`.
  `Ride.handleCancel` takes the row as a **parameter** so `Ride` needs no edge
  to `Flow.Booking`. A second flow cannot reuse the global menu without
  importing `Flow.Booking` or duplicating it — **this is unresolved design
  debt.**
- **Port divergence, recorded in `migration/MODULARIZATION.md` §6 item 8:** the
  TS `getBookingDetails` takes an `allowListFallback` option that **does not
  exist anywhere in the Haskell**, so all three Haskell call sites are
  identical. Severity is **low**: `Adapter/Backend.hs:143-153` does
  `QRB.findById` guarded by `booking.riderId == pid` with no list fallback —
  i.e. the port picked the *safe* branch uniformly and added an ownership check.
  It is a lost resilience fallback in the driver poll, **not** a
  booking-substitution hazard.
- **`Env.hs`'s `BotConfig` still carries four booking-only poll constants**
  (`flexiQuotePollAttempts/IntervalMs`, `regularEstimatePollAttempts/IntervalMs`)
  read only by `Flow/Booking.hs`. Its haddock overclaims "everything here is
  flow-agnostic".
- **`Engine.hs`'s `handleMore` still hardcodes `"ride_type:regular"`**, so the
  router is not booking-string-free despite its header comment.

---

## 9. Working agreement

- **The human runs ALL git writes.** You may run `git status`, `git log`,
  `git diff`, `git show`, `git rev-parse`. You may **not** run `git commit`,
  `git add`, `git rebase`, `git push`, `git checkout -b`, `git worktree`, or
  `git init`. Print the exact command and wait.
- **Commit trailer:** `Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>`
- **Commit format:** `<sub-project>/<type>: <summary>` — e.g.
  `rider-app/refactor: …`. Types: `feat, fix, chore, ci, docs, perf, refactor, test`.
- **Never edit a test fixture to make a test pass.** If a fixture goes red, the
  code is wrong. Stop and report.
- **Say which flag set you used** when claiming something compiles.
