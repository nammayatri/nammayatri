# The driver wallet

No top-up, no work: the wallet, the top-up gateways, the daily charge and the dispatch list built from it.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## The driver wallet — `driver-wallet.sql`, `maps-shim/wallet.js`

**30 MRU a day, taken at his first ride.** The client's model, 2026-09-06, and
it replaces the subscription entirely.

A driver loads credit — never less than 30 MRU, as much above as he likes — and
**nothing is taken until he works**. At his first ride of a day 30 MRU comes off
and he is covered for 24 hours; every ride inside that window is free.

That removes more than it adds. Nobody is ever charged without driving, so the
whole pay-then-extend apparatus Algerian cards forced on the subscription has
nothing left to guard. Moosyl *does* have a subscriptions API with automatic
billing, and needing none of it is the safer half.

Two rules the client confirmed, both about someone's money:

| | |
|---|---|
| The 30 comes off when a ride **starts**, not when it is accepted | a driver who accepted a ride the passenger then cancelled drove nothing |
| A driver who starts a ride under 30 **goes negative** rather than being cut off | reachable only under the old soft restriction. Since 2026-09-14 accepting needs `canWork`, so a charge at ride start always finds the credit — two simulated drivers went to −60 / −90 MRU before that |

### What it replaced — the monthly subscription, retired 2026-10-07

From 2026-08-26 to 2026-09-07 a driver paid **3 000 DA a month** through
Chargily (`maps-shim/subscription.js`, `/subscription/*`, the tables
`movin.subscription` and `movin.subscription_payment`). The wallet replaced it
because nobody should be charged for a month he does not drive, and because
Algerian cards cannot be debited automatically — the subscription had to be
pay-then-extend, with every renewal a driver's own action.

Retired in phase 6, once measured unused: no phone had called `/subscription/`
since 2026-09-02 and its tables had no write after 2026-08-28 (33 drivers, 9
checkouts, 1 paid). The edge now answers `/subscription/` **410**; the code is
gone; the three objects were dumped, encrypted, to the backups
(`subscription-final-*.sql.gpg`, locally and offsite) and then dropped by
`db/retire-subscription.sql`. `movin.invoice_seq` stays: the wallet's receipts
number from it. The full design and its reasoning are in git: this README as of
commit `fe4a44c3a8`, section *Driver subscriptions*.

### The obvious condition was wrong, and the data said so

The first version charged rides in state `INPROGRESS`. A ride that starts and
finishes between two sweeps is **never seen in that state**, so every short ride
would have been free and nobody would have known until an audit. The honest
marker is `trip_start_time`, and the 88 rides in the database prove it:

| status | rides | with a start time |
|---|---|---|
| COMPLETED | 58 | 58 |
| CANCELLED | 30 | **1** |

The 29 cancelled before pickup have none — never charged, exactly the client's
rule. The one cancelled *after* starting has one: that driver drove.

### The gate is NOT "has an active day"

The day only begins at the first ride, so gating on it would stop a driver who
has just topped up from ever getting the ride that starts it — he would watch a
full wallet do nothing, with every figure on screen correct. The rule is
**`day_until > now() OR balance >= PRICE`**, in `restricted.js`, and the app is
forbidden from recomputing it: `GET /wallet/status` returns `canWork` and the
screens use that. Two opinions about it is a man told he is fine while the pool
skips him.

### It became a HARD block on 2026-09-07

Asked whether an unpaid driver should be stopped or merely deprioritised, the
client answered: *"Let's not allow him to go online."* So `canWork` no longer
only orders the dispatch pool — **it decides whether he may go online at all.**

Enforced in the app, in `driver/duty.tsx` (and since 2026-09-14 in the guard
and dispatch as well — see below): the toggle re-reads `/wallet/status` and
refuses. Two properties of that are deliberate.

**Unreachable does not block.** A wallet we cannot read is our failure, not his,
and it must never be the thing that stops a man working.

**It re-reads rather than trusting what the screen loaded.** He has very likely
just come back from the top-up screen, and refusing him over a figure fetched
minutes ago would refuse him for a debt he has already settled — his money gone
and the app still saying no.

### No top-up, no work — 2026-09-14, and hard at every layer

The client's rule, stated because it had been half-understood:

- **The wallet holds only what the driver loads** through Chargily Pay
  (Algeria) or Moosyl (Mauritania). **Never ride money**: Movin takes **0 %**
  on rides, the passenger pays the driver directly, and the two are entirely
  separate. The ledger's kinds are `topup`, `day` and `adjustment` — no ride
  fare ever enters it.
- **A driver without the credit for a day (100 DA / 30 MRU) and no day already
  paid for does not work** — however much he earned from rides.

That closed the two gaps the 2026-09-07 block had left open:

| Layer | What refuses | Since |
|---|---|---|
| Dispatch (Haskell) | `movinOnlyPaying`: an unpaid driver is **never** offered a job — the old `movinPreferPaying` still offered him one when no paid driver was in the pool | backend 09dc606410 |
| `auth-guard` | `POST /ui/driver/setActivity?active=true` and `quote/respond` with `Accept` → **403 `WALLET_EMPTY`** when the driver's own `/wallet/status` says `canWork: false`. Holds for an older APK too. **Fails open** when the wallet cannot be read | 2026-09-14 10:01 |
| App | the switch refuses; while online and between rides the wallet is re-read every minute, and a driver whose paid day ran out without credit for the next is **taken offline** with the reason. Never during a request or a ride | app 613517d |

The "undecided" case of 2026-09-07 — a day expiring while he is still online —
is therefore decided: he goes offline, and dispatch would skip him anyway.

**Proving the binary.** The new rule reads a new key, `movin:unpaid`, and that
string is the only thing in the binary that tells the hard rule from the soft
one. `deploy-backend.sh` refuses to swap an image without it. `restricted.js`
writes the same list under both key names, so the old binary and the new one
each find theirs, and a rollback needs nothing.

**The simulated fleets are not exempt** (the client's choice, 2026-09-14): they
have no credit and get no rides. Test with a real driver account topped up
through the gateway page.

**That test stopped being free on 2026-09-20**, and only on one side. Chargily
is still a test key, so an Algerian top-up is the real flow with no real money.
**Moosyl is live**: a Mauritanian top-up moves a real 30 MRU out of a real
account, and there is no sandbox to fall back to — the same URL serves both and
only the key differs. To rehearse the Mauritanian flow without paying, put the
old test key back for the length of the test (`install-moosyl-key.sh` keeps
every previous key as `/opt/ny/secrets/moosyl.env.before-<stamp>`), or credit
the wallet row directly in Postgres and leave the gateway out of it.

### The Moosyl contract, measured rather than read

Their published OpenAPI is wrong about the two things this needs. Both were
established by calling the API on 2026-09-06:

- `POST /checkout-session` returns **`checkoutUrl` at the top level**, outside
  `data`. The schema documents only `data`, so it looks absent.
- **The status lives on the checkout session, not the payment request.** Nothing
  in the payment-request family carries one, `refresh-status` included. The
  session's is `open | completed | expired | cancelled`.

Auth is `Authorization: <raw key>`. `Bearer` is refused, and a wrong key answers
`404 Invalid API key` rather than 401. `GET /configuration` is the free key
test — it authenticates and reports the environment without moving anything.

### Why a webhook can never grant credit

Moosyl documents **no webhook signature scheme anywhere**. So the webhook here
is only a *hint to go and look*: it reads the session status back from Moosyl
with our own key and credits from that. An unsigned POST from anyone on the
internet therefore cannot put money in a wallet — a stronger property than
verifying a signature we would have had to guess. Proven: a forged `completed`
webhook returned 200 and created zero entries.

### The ledger is the truth, the balance is a cache

`wallet_entry.amount` is signed — a top-up positive, a day negative — and
`wallet.balance` is `sum(amount)`. `movin.wallet_check` reports any drift
between the two. **Revenue is the sum of the `day` entries**, not of the
top-ups: a top-up is money taken and not yet earned, and confusing the two
would report the fleet's unspent credit as income.

Invoice numbers are drawn from `movin.invoice_seq` at the moment a payment is
*applied*, never when a page is opened, so an abandoned checkout burns none and
the series has no holes.

### Routes

`GET /wallet/status` · `GET /wallet/history` · `POST /wallet/topup?amount=N` ·
`GET /wallet/topup/{id}` · `GET /wallet/done` · `POST /wallet/webhook`

⚠ **They are 404 from every phone until the edge has a `location /wallet/`.**
See the gotcha below — this cost an hour on 2026-09-07 with the server perfectly
healthy on `127.0.0.1:8030`.

### Proving it — `probe-wallet-screens.py`

Run it **on the VPS**. It asserts the field names and types the app parses,
against the deployed server, with a real driver's token — because `wallet.js`
and `lib/wallet.ts` were written against each other, and two files written in
one sitting agree about a typo as readily as about a contract.

The check worth the most is `canWork == dayActive OR balance >= dayPrice`. It
also catches the failure JavaScript hides: a missing field is not a crash, it is
a silent zero, which is how a driver holding 300 MRU is shown an empty wallet.

It opens one real checkout and deletes the row afterwards; left behind it would
sit in that driver's own history as a payment he never started. 33 checks,
all passing as of 2026-09-07.

### The key — `install-moosyl-key.sh`

The secret lives at `/opt/ny/secrets/moosyl.env` (root, 600) and **never in
git**. It is the only place it has ever lived: the app does not hold it, the
Haskell backend does not know Moosyl exists, and `docker-compose.yml` carries
`MOOSYL_BASE` but deliberately not the key. So changing keys is one file and one
container, and nothing to build.

    ./install-moosyl-key.sh '<key>'          # the key is an argument, never a line in the repo

**Test and live are the same base URL.** `https://api.moosyl.com` serves both;
there is no `/test` prefix the way Chargily has one, and no setting anywhere
that says which you are on. The *key* decides, and the only way to know which
one you hold is to ask: `GET /configuration` lists the payment methods with an
`isTestingMode` flag each. That is the last check the install script runs, and
it refuses the install — telling you how to roll back — unless every method
comes back false. A key that authenticates is not the same as a key that takes
money. A wrong key answers `404 Invalid API key`, never 401.

**`env_file` is read when the container is created, not when it starts.** A
plain `docker compose restart maps-shim` keeps the old key and every check then
passes or fails for a reason that has nothing to do with the key you just
installed. The script uses `up -d --force-recreate --no-deps maps-shim`. Same
trap as `install-moorsyl-key.sh`.

| | |
|---|---|
| 2026-09-06 | test key. All five methods — bankily, masrivi, sedad, bim_bank, bci_pay — `isTestingMode: true`, which is what made it safe to build the whole wallet before production existed. |
| 2026-09-20 | **production key** from the client. Valid one year — **expires 2026-09-20 + 1y = 2027-09-20**, and nothing warns you; a lapsed key reads as `404 Invalid API key` and every top-up returns `not_configured`. |

Once the live key is in, `probe-wallet-screens.py` still opens one real checkout
session against the real gateway. It pays nothing and deletes its own row, and
an unfinished session expires on Moosyl's side — but it is no longer a rehearsal,
so read what it opened before running it on a driver who is not yours.

### Dispatch — `maps-shim/restricted.js` and two lines of Haskell

How dispatch skips a driver who may not work. Built on 2026-08-26 for the
monthly subscription, when the rule was soft — an unpaid driver **stayed
online** and a request reached him only when no paying driver was in the pool —
and kept by the wallet, which changed only who is on the list. Plus a cap of
300 rides per paid day, which lands in the same place.

This is the one part of billing that is genuinely a Haskell change, and it is
deliberately the smallest one available.

**The binary is never told what a wallet is.** It reads one Redis key
holding a JSON array of driver ids and prefers everybody else. That is the whole
of its knowledge — not a day's price, not 300 rides. Who is on the list is computed
in `restricted.js` and can change in the time it takes to restart a container.
A number compiled into the binary would mean a 45-minute build every time the
client revised it.

    movin.wallet + ride counts                <- policy, in the shim
      -> dynamic-offer-driver-app:movin:unpaid       (JSON array of ids;
         also written as :movin:restricted, the key the pre-2026-09-14 binary reads)
        -> calculateDriverPool skips them entirely  <- one filter, in Haskell

> **Superseded 2026-09-14 — the filter is HARD now.** Everything below about
> an unpaid driver still being offered a job "as the only one in the area" was
> the 2026-08-26 rule. The client's rule since 2026-09-14 is *no top-up, no
> work*: `movinOnlyPaying` never offers an unpaid driver a job. See **No top-up,
> no work** under the driver wallet.

**The key name is the whole integration, and it was measured.** Hedis prefixes
keys with the app name: plain calls land under `dynamic-offer-driver-app:`,
`withCrossAppRedis` under `driver-offer:` — read off the live Redis, not
guessed. Get it wrong and *nothing fails*: the binary reads a missing key,
restricts nobody, and the feature is silently off for ever.

**The patch needs no signature changes.** `Redis` is already imported in
`SharedLogic/DriverPool.hs`, and `CacheFlow m r` already implies `HedisFlow m r`
— so `calculateDriverPool` can read Redis without touching its constraints.
Both sites were checked against the **real 2023 baseline fetched from GitHub**,
not against this branch, which has diverged in exactly that file.
`try-dispatch-patch.py` in the scratch dir applies them and prints the result.

**Applied at `DriverSelection`, deliberately not at `Estimate`.** Estimate is
what a passenger is quoted before booking. Filtering there would delete a
vehicle tier from her price list whenever the only driver of that variant owed
us money — so she would never see it, never book it, and he would never receive
the request he was still entitled to as the only one in the area. A passenger
should not be shown fewer options because a driver has not paid us.

**"The only one in the area" means the current radius**, which widens step by
step. A lapsed driver can therefore be offered a job at the first narrow step
while a paying driver sits just outside it. That is the honest reading; holding
the request back to see whether a wider ring finds somebody paid would delay a
real passenger to enforce a billing rule.

**Every failure means nobody is restricted.** Missing key, unparseable value,
query that throws, shim that has never run — all leave dispatch behaving exactly
as it does today. A stale list is preferred to no list: wrong for minutes rather
than wrong until somebody notices. The failure worth designing against is the
other direction, and no path produces it.

**Topping up restores him at once**, not on the next five-minute tick —
`wallet.js` republishes the list the moment it credits a top-up. A driver who
has just paid and then watches five more minutes of requests go past him has,
from where he is sitting, paid for nothing.

The shim half is tested in CI (`tests/restricted.test.js`,
`tests/wallet-dispatch.test.js`, against a real Postgres).
`investigations/probe-restricted-drivers.py` proved it on the live stack in
August against the subscription-era policy and no longer matches the wallet's.

It must also be **visible in the app**, and it is: the wallet screen and the
duty toggle say why (*It became a HARD block*, above). A driver whose rides
quietly stop concludes the app is broken and rings the office, not that his
wallet is empty.
