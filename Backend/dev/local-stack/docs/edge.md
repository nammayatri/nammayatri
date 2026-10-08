# On the internet — TLS, the domain, and what a person may send

The nginx edge: certificates and the lock that had to come first, the domain switch, and the size limits on everything a caller can send.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Published on the internet — TLS, and the lock that had to come first

```
https://api.169-58-139-65.sslip.io/v2/...
```

Until now everything was reached through an SSH tunnel: safe, and useless for
handing an APK to anyone else. This publishes **exactly one thing** — the rider
API — over TLS, and nothing else.

### The lock went in first, and that ordering is the whole point

`POST /v2/auth` answers with `attempts: 3`, and the backend enforces nothing.
Measured on this stack: **62 consecutive wrong codes, the counter never moved,
and the same session still accepted the right code afterwards.** Four digits is
10,000 possibilities — about ten minutes.

Harmless while every port but SSH is shut. Not harmless one second after 443
opens. So `auth-guard` was built, deployed and **proved** while the stack was
still private, and only then did the edge go up.

| | Before | Now |
|---|---|---|
| Wrong codes per session | unlimited | **3**, then locked 15 min |
| Auth session lifetime | forever | **10 minutes** |
| Sign-ins per phone number | unlimited | **5/hour** |
| Requests per address | unlimited | 20/min on auth, 240/min otherwise |
| Full 10,000-code sweep | **~10 minutes** | **~28 days per number** |

The session lock alone would have been theatre: an attacker just asks for a new
`authId` and spends three guesses on that. **Throttling session creation is what
makes the session lock mean anything** — and it is the same control that stops
someone burning our SMS credit the day a real gateway exists.

```bash
./prove-lockout.sh https://api.169-58-139-65.sslip.io   # six checks, all must pass
```

### Why the guard is not in the backend

That is where it belongs — the counter already exists in the response and
enforcing it would be a few lines of Haskell. But this stack runs **prebuilt
binaries** from a CI job with a `timeout-minutes: 350` budget and a cache that
accumulates across runs. Rebuilding to change a counter means a multi-hour cycle
and a real chance of ending up with binaries that differ from the ones every
test so far has run against. When the backend is next rebuilt for another
reason, the check should move into it and the guard becomes belt-and-braces.

### What is exposed, and what is not

Verified by scanning from outside, not by reading the config:

| Open | Closed |
|---|---|
| 22 SSH · 80 redirect + ACME · **443 rider API `/v2/`, driver API `/ui/`, tiles** | Postgres, Redis, demo pages, mock-google, OSRM, auth-guard, **Swagger**, and the driver binary's **`/dashboard/` office routes** |

Swagger in particular is a complete, executable description of the API, and
there is no reason to publish it.

The driver binary serves 96 routes: 47 under `/ui/`, which is the driver's own
app, and **41 under `/dashboard/`**, which is the office — the API that enables a
driver, attaches his vehicle and reads his documents. Publishing `/ui/` must not
carry `/dashboard/` with it. Two independent things refuse it: nginx's catch-all
404, and the guard, which routes only prefixes it knows and 404s the rest.

Beyond those, the edge publishes a handful of **exact** paths, each for one
caller and each a `location =`, never a prefix:

| Path | To | For |
|---|---|---|
| `/driver/documents`, `/driver/declaration` | admin-api | the driver's papers and what he typed |
| `/rider/report` | admin-api | « Signaler » mid-ride (2026-09-27) |
| `/driver/sanction` | admin-api | a suspended driver's countdown and reason (2026-09-27) |
| `/whatsapp/webhook` | auth-guard | Meta's webhook (2026-09-27) — see *[WhatsApp](sign-in.md#whatsapp--the-webhook-since-2026-09-27)* below |
| `/sms/inbox` | auth-guard | the office phone's SMS forwarder (2026-09-29) — see *[The SMS inbox](sign-in.md#the-sms-inbox-and-sign-in-by-an-sms-he-sends-2026-09-29)* below |

admin-api's `/internal/*` is deliberately **not** among them: it is how the
guard reports a driver's rating (see *[Ratings](riders.md#ratings--apply-ratingssh)*), and it answers the box only.
Probed from outside on 2026-09-27: 404 / 405 on all three hostnames.

**Every `/v2/` and every `/ui/` request goes through the guard.** That is not a
detail: an nginx `location` that reached either backend directly would quietly
undo the whole thing, so no such location exists.

`edge` runs with `network_mode: host` **on purpose**. A bridged container with
published ports bypasses ufw entirely — the trap that makes `ufw default deny`
insufficient on this box. Bound to the host directly, 80 and 443 are governed by
ufw like anything else.

### Certificate

Let's Encrypt, HTTP-01, issued with `--standalone`, renewed by the `certbot`
container over `--webroot` twice a day. nginx **reloads itself every six hours**
because it reads its certificate once at start — that is how a stack serves an
expired certificate two months after renewing it successfully.

Renewal was dry-run against staging before anything real was requested, and the
challenge path was proved reachable from outside. Two things worth knowing:

- Registered **without an email**, so there are no expiry warnings. Add one with
  `certbot update_account -m <address>` if you want the safety net.
- The hostname is `sslip.io`, which is public DNS resolving any
  `*.169-58-139-65.sslip.io` to this box. Swapping to a real domain is one
  `server_name`, one certificate, and nothing else.

### The map, published too

```
https://api.169-58-139-65.sslip.io/tiles/...
```

A release APK cannot reach a loopback tile server, and **Android refuses
cleartext HTTP in release builds** — so "just open 8035" was never an option
either. Both problems end at the same place: the tiles go through the same TLS
edge, read-only.

The exposure is bandwidth rather than data — it is one `.mbtiles` file we built
ourselves. A map view costs 1–3 MB the first time and close to nothing
afterwards, because the responses are marked `immutable` for a week: a tile at
a given z/x/y cannot change until we rebuild the whole extract, which is a
deliberate act. Rate-limited at 1200 r/min per address, which sounds enormous
and is not — one screenful at z14 is thirty-odd requests and panning fires
hundreds a minute, so a limit tight enough to *feel* like security would just
make the map stutter for real riders. Deleting the `location` block revokes the
whole thing in seconds.

**`--public_url` had to change at the same time.** The style document's
internal URLs — the vector source and the glyphs — are absolute and baked in by
the tile server, so a style served from the public host that still pointed
clients back at `localhost:8035` would load and then draw nothing.

Three things that cost time here, all worth knowing:

- **A single-file bind mount breaks if you replace the file.** Deploying
  `nginx.conf` with `tar -x` unlinks and recreates it, so the container keeps
  the old inode and serves stale config — while `nginx -t` passes and a reload
  reports success, because both are validating the config it still has. Use
  `scp` (which truncates in place), or recreate the container.
- **`docker-compose.yml` is the one file you must never copy wholesale.** The
  deployed copy is a *superset*: it also carries the website's `admin-api`
  service and an `edge-web` mount, which live in the movin-website repo and are
  added to `/opt/ny/local-stack/docker-compose.yml` directly. Copying this
  repo's version over it deletes them from the definition — done on 2026-09-06,
  and the only reason nothing broke is that the container was never restarted.
  Compose does say so, in the line it is easiest to read past:
  `Found orphan containers (movin-admin-api) for this project`. "Orphan" means
  compose no longer knows what that container is for, and the next
  `--remove-orphans` deletes it. Edit the deployed file in place, or re-add the
  missing blocks after copying.
- **`expires` generates a `Cache-Control` of its own**, and the tile server
  sends one too, so the naive block emitted the header three times and left the
  client to choose. One `add_header`, with `proxy_hide_header` for the
  upstream's.
- The only font in the tileset is **Noto Sans Regular**. Asking for Open Sans
  returns a 400 that looks exactly like a proxy fault and is not one.

## The domain — `./switch-domain.sh`

The API answers on `api.169-58-139-65.sslip.io`, a hostname containing the
VPS's own IP address, which therefore dies the day the box moves. The client
bought **movinapp.net** on 2026-08-26.

    ./switch-domain.sh --check api.movinapp.net    # will it work?
    ./switch-domain.sh api.movinapp.net            # do it

**The old hostname keeps answering afterwards, and that is not politeness.**
Chargily writes the callback address into each checkout *at the moment it is
created*, so a payment started before the switch still calls back to the sslip.io
name. If that stops answering, the driver pays and the webhook lands nowhere —
money in, no month out, and nothing on any screen to say so. The certificate is
therefore *expanded* to cover both names rather than replaced. Retire the old one
only after a fortnight with no checkouts referencing it (the subscription's
checkouts recorded it in `movin.subscription_payment.event`; that table was
dropped on 2026-10-07, long after the fortnight).

**The script refuses to run against Cloudflare's proxy**, and the reason is
specific rather than tidy-mindedness. Measured 2026-08-26: `api.movinapp.net`
resolved to 104.21.75.212 / 172.67.182.57 with `server: cloudflare` and
answered 404 — the record was created with the orange cloud on, so the name
reaches Cloudflare, which has no origin for it. Two reasons not to work around
that:

- **Chargily's webhook would arrive through Cloudflare's bot filtering.** That
  filtering has already blocked a legitimate automated client of ours — HTTP
  403, error 1010, hitting Chargily's own Cloudflare-fronted API from the VPS. A
  webhook silently dropped is a payment taken and never applied.
- The apps would gain a component between them and us that nobody here can
  debug.

DNS only — the grey cloud — and the certificate stays ours.

**Two things the script deliberately leaves alone**, because both are the point
of no return: `PUBLIC_URL` in `.env` (what Chargily writes into *new*
checkouts) and `API_BASE_URL` in the app, which is compiled in and needs every
phone updated. Do the second while the only phones are ours.

**Certificate renewal was checked while doing this** and it works, despite
looking as though it should not: the certificate was issued `standalone` and
the renewal container runs `--webroot`. The command-line flag overrides the
stored authenticator — `certbot renew --dry-run` succeeds. Expiry 9 Nov 2026.

## What a person may send — the bounds, audited 2026-09-27

**No SQL is built from user input anywhere we own.** All 67 queries in
`maps-shim` are parameterised (`$1`…), and the one interpolation is a table
name picked from two constants; admin-api's 45 routes all parse through Zod,
and its only interpolations are constants (country predicates, merchant ids).
The Haskell backend is upstream's, on Beam. So "injection" here means size and
shape, and these are the bounds, outermost first:

| Where | Bound |
|---|---|
| nginx | 1 MB per request by default; tighter per route (16 KB declaration and report, 256 KB WhatsApp and SMS inbox, 512 KB avatar, 8 MB driver register, 10 MB documents) |
| the database | every typed text field upstream stores is `varchar(255)` and refuses more |
| auth-guard | a driver's reply to an office message ≤ 1000 characters (`message_report.reply` is the one unbounded `text`); must be JSON with a string `reply` |
| maps-shim `/avatar/` | JPEG or PNG by **the file's first bytes**, not by its header; ≤ 512 KB |
| maps-shim `/wallet/topup` | at least the minimum, **at most 100 days of credit** (`WALLET_MAX_TOPUP_DAYS`): 3000 MRU / 10000 DA |
| maps-shim search | input cut at 100 characters; coordinates must be on Earth (±90/±180); at most 8 waypoints |
| admin-api | every field has a Zod bound; the driver's declaration is truncated, not refused, by design |

Each was proved on the live stack the day it went in: a script sent as
image/jpeg and a PNG sent as JPEG → 415, a real JPEG → 200; a top-up of
99 999 999 → 400 `amount_too_large`; a 1001-character reply → 400
`REPLY_TOO_LONG`; latitude 999 → 400. The console renders every stored string
through React, which escapes it.
