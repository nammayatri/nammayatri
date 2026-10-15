# Sign-in — SMS and WhatsApp

The auth guard's codes: the SMS gateway, sign-in by an SMS the person sends, and WhatsApp.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## The SMS gateway — Moorsyl, since 2026-09-06

Codes are real now. `7891` no longer signs anybody in from the internet.

**It is not a backend integration, and the obvious place to put it was a trap.**
`Sms_MyValueFirst` in `merchant_service_config` is the same shape as the
`Maps_Google` row that `maps-shim` hijacks, so repointing it looks like the
whole job. It is not: `useFakeSms = Some 7891` short-circuits the SMS path
*before* that config is read, and that setting is in dhall, inside the image.
Turning it off delivers nothing at all — the gateway it would then look for is a
dead port on 4343. **When a config knob sits behind a compiled-in switch, the
knob is not the integration point.**

So `auth-guard` does it, in front, with no rebuild: it obtains a code, checks
what the caller typed, and forwards `7891` upstream regardless. The backend
still believes in its fixed code and has never been told otherwise.

### Two products, and only one of them works on this account

Measured 2026-09-06, and worth re-measuring rather than assuming, because it is
the client's paperwork that changes it:

| | |
|---|---|
| `POST /api/sms` | **403 `COMPLIANCE_REQUIRED`** |
| `POST /api/verify/send` | 200, returns a `verificationId` |

The 403 came back identically with `from` set to `"Movin"`, to `"moorsyl"`, and
omitted entirely — so it is the *account* that is not cleared for branded
sending, not the name. That distinction is the difference between changing one
string and the client filling in forms, and it is why all three were tried.

`SMS_MODE` selects between them. `verify` today: Moorsyl makes the code, sends
it under its own registered sender, and checks it. `sms` is written and tested
and one environment variable away — it sends our own French wording under
"Movin", and is what to switch to the day compliance clears.

### Things that will bite

- **Codes are SIX characters in both modes.** Verify's check takes exactly six
  (`too_small` otherwise), and `sms` mode matches it deliberately so the app is
  built once and the switch is invisible to it. `CODE_LENGTH` in the app's
  `config.ts` must agree with `codeDigits` on the guard's routes.
- **`SMS_BYPASS` is empty since 2026-10-01** (see *Test accounts* above);
  what follows is why it existed, for the day a test number is needed again.
  It is not a convenience. Moorsyl only delivers to real `+222`
  mobiles, and everyone building this tests from Algeria with invented numbers.
  Without the exemption list this change locks the team out of the product.
  The exempt numbers send nothing and use `SMS_BYPASS_CODE`. **Both live in
  `/opt/ny/secrets/test-accounts.env`, not in git** (since 2026-09-27: they
  were in `docker-compose.yml` and in the guard's default, in a public
  repository). Without a private six-digit code — or with the old public
  `111111` — the guard honours **no** exempt number and says so at startup.
  **Empty the list before the first real rider**, together with `TEST_OTP` in
  the app.
- **The key is in `/opt/ny/secrets/moorsyl.env`**, mounted with `env_file:
  required: false` so a checkout without it still starts — loudly warning, and
  refusing rider sign-ins, which is the honest failure.
- **You can test the key for free.** There is no balance or account endpoint —
  the API has exactly five routes (`/sms`, `/sms/get`, `/verify/send`,
  `/verify/check`, `/verify/get`). But validation runs *before* authentication,
  and `POST /verify/check` on an invented id sends nothing: a good key gets
  `404 "does not belong to this organization"`, a bad key `401`.
- **The docs are JavaScript and fetch as an empty page**, but
  `api.moorsyl.com/api-reference` carries the entire OpenAPI document inline,
  HTML-escaped in an attribute. Unescape that rather than guessing at the API.
- **`+222 25/35/45…` is the fixed-line range** and Moorsyl's regex accepts it.
  That makes it the safe destination for a live test: valid to the API, no
  handset behind it. It is how the send path was proven without texting a
  stranger.

**Never observed: delivery to a real handset.** Everyone on the build side has
an Algerian number, and Moorsyl only delivers to `+222`. The chain is proven as
far as the gateway accepting the message and no further.

### What bounds the bill — since 2026-09-23

Starting a sign-in is the only request on this box that spends money, and
before this date nothing capped the total. `MAX_STARTS` keys on the phone
number, and an attacker does not reuse a number, he rotates them; the only
other thing in the way was nginx's 20 requests a minute on the `auth` zone —
**1,200 texts an hour from one address**, all billed to us.

It compounds with a second fact: **Moorsyl publishes no balance route.**
`/api/balance`, `/api/account` and `/api/me` all answer 404, so an emptied
account cannot be detected from here. It would appear as every registration
failing, silently, with nothing in any log to say why.

Three controls, in the order they bite:

| Control | Where | Default |
|---|---|---|
| `signin` zone, 6r/m burst 3 | `edge/nginx.conf`, sign-in **start** only | — |
| `MAX_STARTS_PER_IP` | guard, per address per hour | 30 |
| `MAX_SMS_PER_HOUR` / `MAX_SMS_PER_DAY` | guard, rolling, all numbers | 60 / 400 |

Three things about them are deliberate and should not be "tidied":

- **The tight zone is on the start, not on `/auth/{id}/verify`.** Typing a
  wrong code is normal and costs nothing; a rider on his third guess must not
  be refused by the edge before the guard can tell him the code was wrong.
- **`auth` itself was left at 20r/m.** The same zone carries
  `/driver/documents`, and lowering it would throttle a driver halfway through
  sending five photographs.
- **`MAX_STARTS_PER_IP` is generous on purpose.** Mauritanian mobile networks
  are behind carrier-grade NAT, so thousands of real handsets share a few
  public addresses: a tight per-address cap does not hit an attacker, it hits a
  city. The global budget is the control that actually bounds the bill.

Past the budget the guard refuses and logs why; **enrolled drivers keep their
personal codes throughout**, exactly as during a gateway outage, because this
must never be the thing that grounds the fleet. Exempt numbers count against
neither new counter — they send nothing, so they cost nothing.

The spend is on `/healthz` under `gateway.budget`, and it is the only view of
the bill that exists. Worth a cron and an alert, not just a glance.

    MAX_SMS_PER_HOUR=120 MAX_SMS_PER_DAY=800   # raise, restart, no build

### The bot — `movin-bot.py`, since 2026-09-23

The console holds twelve screens and none of it reaches anybody who is not
looking at it. Audited on 2026-09-23: the validation queue held two
registrations six and two days old, and the last decision on the whole system
was three weeks before that. Nothing was broken. Nobody had been told.

`movin-bot.service` is a long-running Python process on the host — the same
shape as `server-state.py`, and for the same reasons: there is no Node here,
Postgres needs either a client library or the docker socket from a container,
and an admin service account would hand a chat process a console session. The
standard library covers all of it, so **nothing is installed for this**.

**It reads and it tells, and it has no write anywhere.** The owner chose
read-only commands on 2026-09-23; validating, messaging the fleet and the
tariff stay in the console behind a login. A bot in a pocket that can enable a
driver is a mistake waiting for a thumb.

Twenty checks, grouped by what they are for:

| | |
|---|---|
| **Money** | SMS budget at 80% and exhausted; the gateway refusing; wallets negative; top-ups |
| **Law** | a deletion request arrives; its `delete_by` deadline approaching or passed |
| **Silence** | no rides; searches returning no offer; driver positions stale; nobody online |
| **Machine** | a container not `running`; the API unreachable from outside; disk; TLS expiry; a failed backup |
| **Rhythm** | a daily digest, a weekly one |
| **Quality** | a passenger's report (« Signaler »), at any hour; a one or two star rating with a written complaint; a run of cancellations; a driver suspended, closed or blocked, with the console's reason and end date |

The *Silence* group is the point. This stack's documented faults — stale
positions, the BECKN negative coordinate, Redis-cached merchant rows — each
produced **no error anywhere** and each cost an afternoon. A check that
notices nothing happening is worth more here than one that reads a log.

Commands, all read-only: `/file`, `/jour`, `/semaine`, `/chauffeur <numéro>`,
`/flotte`, `/serveur`, `/budget`, `/aide`. Only the configured chat id is ever
obeyed.

Four things about it are deliberate:

- **It seeds on first run.** A bot switched on beside a fleet that has been
  running for weeks would otherwise open with a wall of history, so the first
  pass records what is already true and says nothing.
- **It says a thing once**, and only again once the cause has cleared and come
  back. A notifier that repeats itself is one people mute.
- **Quiet hours hold everything but money, law and a dead stack.**
- **A database that does not answer is never reported as zero.** That
  distinction is how a monitoring system invents an outage, and there is a
  test for it.

`TELEGRAM_BOT_TOKEN` and `TELEGRAM_CHAT_ID` go in `.env`, which is not in git
and is in the backup set. Without them the process exits 0 and does nothing,
so the unit is safe to enable before the bot exists. Thresholds are all `BOT_*`
environment variables — change and restart, no rebuild.

**Both countries, since 2026-09-27.** Until then every driver query said
`merchant_id = MR`, so a driver registering in Algeria was never announced —
while his Chargily top-up was, the top-up query having no merchant filter at
all. Found by the owner registering himself in Algeria and hearing nothing.
Every driver query now names both merchants, every message carries 🇲🇷 or 🇩🇿
(a driver's country is his merchant, a passenger's his number — the console's
rule), and the digests and `/flotte` give one block per country so ouguiyas
and dinars are never added. Every query was run through `EXPLAIN` on the live
schema before deploying.

**Reports and deletions.** A passenger's report (`movin.ride_report`) is sent
at once and through quiet hours — the owner asked to hear each one as it
happens, and it may be about a driver still on the road. A deletion request
(`movin.deletion_request`, either side) is announced with the reason given,
then again as its 30-day deadline nears.

**A trap fixed on the way:** `psql()` used to `strip()` the output, and Python
counts the column separator `\x1f` as whitespace — so a row whose **last
column was empty** came back one field short and was silently skipped. A
deletion with no reason, or a closure with no end date, would never have been
announced. The test suite now has a row like that.

*It replaces `registration-notify.sh`, which did the registration half only.*

## WhatsApp — the webhook, since 2026-09-27

Movin's WhatsApp Business number is **+213 783 07 91 61** ("MovinApp", Cloud
API). Business account `2899338557098050`, phone number id
`1428730883648447` — checked against Meta's Graph API with the client's
System User token (never expires). The Meta app **movindz** was subscribed to
the business account the same day.

The plan is sign-in **without a template**: the app opens WhatsApp with
`MOVIN 483920` already typed, the passenger presses Send, and the sender's
number — WhatsApp's word, not something he typed — proves he holds it.
Messages people send a business are free and need no approval.

Built (2026-09-27): **sign-in by WhatsApp**, end to end, in the guard
(`auth-guard/whatsapp.js` for Meta's side, `server.js` for the sessions).

- `POST {/v2,/ui}/auth/whatsapp` is a sign-in start with every check the SMS
  start has, and no SMS: the answer carries a code and a `wa.me` link with
  `MOVIN <code>` already written. nginx gives it the `signin` bucket.
- `GET {/v2,/ui}/auth/{id}/whatsapp` → `{confirmed}`; the app polls it.
- The usual verify accepts the code **only** once Meta has delivered it,
  signed with the app secret, from the number signing in. The code alone was
  handed to the caller and opens nothing. Numbers are compared without the
  Algerian trunk zero (WhatsApp writes `213555…`, the app `+2130555…`).
- The sender is answered « vérifié ✅ » on WhatsApp — free, inside the 24 h
  his own message opened.

Proved live the same day through the public edge with a delivery signed by
the real app secret: verify before the message → 400; after → 200 with a
token. `tests/auth-guard-whatsapp.test.js` covers unsigned, forged,
wrong-number and wrong-code deliveries. A closed country's WhatsApp start is
refused like an SMS one; **Algeria is open by WhatsApp** since the same
evening (`SMS_COUNTRIES`, above), and by an SMS he sends us since 2026-09-29.

Secrets: `/opt/ny/secrets/whatsapp.env` (root, 600) holds the verify token,
the app secret, the access token, the phone number id and the number.

When the compose `env_file` list changes, `docker compose up -d --no-deps
auth-guard` — a `restart` does not re-read it. And the deployed compose is a
superset of this one (see its header): patch it in place, never copy it.

### The SMS inbox and sign-in by an SMS he sends (2026-09-29)

The client's plan (2026-09-28) is the WhatsApp flow over plain SMS: the
passenger texts `MOVIN <code>` to a SIM in an Android phone at the office,
and that phone's forwarder (the client's own, "chatty-sms") POSTs what it
received to us. It would replace Moorsyl — which **stays** until this is
proved on the real phone (owner's decision).

`POST /sms/inbox`, `Authorization: Bearer <SMS_INBOX_TOKEN>`, body
`{"source":"chatty-sms","count":n,"messages":[{"from":…,"body":…}]}`. The
guard answers it itself (`auth-guard/sms-inbox.js`) and files each message's
code under its sender, local or international (`41234567`, `0555123456`,
`+222…`, `00213…` all resolve). Sender and text are read under several
field names, because the forwarder is not ours. An empty list is a
heartbeat; `/healthz` → `smsInbox.lastAt` is the phone's pulse.

**Phase 2, the sign-in — built 2026-09-29; live for Algeria, proved end to end on 2026-09-30** (the owner, from the APK: button, Messages, send, signed in; the boss's forwarder posting incoming texts and a heartbeat every 5 min).
The WhatsApp flow, route for route:

- `POST {/v2,/ui}/auth/sms-in` — a start with every check a start has; the
  answer carries `smsIn: {code, number, text}`, the SIM being the one for the
  caller's country. No SIM for that country → 503 `SMS_IN_UNAVAILABLE`, backend
  never asked. nginx gives it the `signin` bucket.
- `GET {/v2,/ui}/auth/{id}/sms-in` → `{confirmed}`; the app polls it.
- `GET {/v2,/ui}/auth/sms-in/countries` → `{countries: ["+222"]}` — asked by
  the phone screen before it shows « Confirmer en nous envoyant un SMS ».
  Superseded on 2026-09-30 by `GET {/v2,/ui}/auth/channels` →
  `{sms: ["+222"], smsIn: ["+213"], whatsapp: true}`: the phone screen now
  shows **only the ways in a country has** (Algeria: WhatsApp and « SMS to
  us »; Mauritania: « Continuer avec SMS » and WhatsApp), read from
  `SMS_COUNTRIES`, the SIM list and WhatsApp's readiness — so a country
  changing is still a setting here, not an app build. The old route stays for
  APKs built before that date.
- verify accepts the code only once the office phone has forwarded it FROM
  the number signing in. Only texts that carry a code are filed, so a « merci »
  sent after it does not bury it.

**Switching it on is a setting, not a build:** `SMS_INBOX_NUMBERS` in
docker-compose.yml (`+222=+222XXXXXXXX,+213=+213XXXXXXXXX`), then `docker
compose up -d --no-deps auth-guard`. The app (since 2026-09-29) shows the
button in any country listed there and hides it everywhere else.
**Set 2026-09-29: Algeria only**, `+213=+213783079161` — the client's office
phone. No Mauritanian SIM yet (a `+222` text to an Algerian SIM would be
international), so Mauritania keeps Moorsyl and WhatsApp.

**Only what the office phone RECEIVED counts.** The client's forwarder posts
its sent messages too — its first real sample was `direction: 'outgoing'`,
with `sender` holding the number it was sent TO — so `direction` outgoing /
sent / outbox is dropped (`smsInbox.outgoing` on `/healthz` counts them).
Believing one would sign in whoever the office phone texts.
`tests/auth-guard-sms-in.test.js` uses his exact message shape.

The token proves the POST came from our phone; it does **not** prove the
sender. An SMS sender can be forged on some international routes — the
weakness WhatsApp's signature does not have, and the reason this is the
client's call, told to him on 2026-09-28. `tests/auth-guard-sms-inbox.test.js`.

Secret: `/opt/ny/secrets/sms-inbox.env` (root, 600), `SMS_INBOX_TOKEN`.

### Signing back in on a phone that already proved the number (2026-10-03)

A session lasts a year, so a code was asked again only after « Se
déconnecter », a reinstall or a new phone — and for anyone who is both
passenger and driver, at every switch between the two, since those are two
sign-ins on two backends. The boss: the phone that already confirmed the
number should not be asked again.

**How.** Every verify the backend accepts — SMS, WhatsApp, an SMS sent to us
— comes back with a `deviceTrust` key beside the token
(`auth-guard/trusted-phones.js`). The phone keeps it per number, through
sign-out, and presents it to `POST /v2/auth/trusted` or `/ui/auth/trusted`
with the number: a match is a start and a verify in one request, with the
backend's fixed code, and nothing is texted. So the key is what proves the
handset; the number alone opens nothing. One key opens both sides, which is
the switch the boss asked about.

| | |
|---|---|
| Kept | sha256 of each key only, in `/state/trusted-phones.json` on the `auth-guard-state` volume (`/app` is read-only, and the file must survive a recreate). `TRUSTED_PHONES_FILE`. |
| Life | a year from last use (`TRUSTED_PHONES_TTL_DAYS`); at most 5 phones per number, least recently used dropped. |
| Gates | every start gate still applies — open country, sign-up closed, both start throttles. Only the one about how a code travels does not, since none does. |
| Edge | under the `^/v2/auth` / `^/ui/auth` rate limit, not the start's: it texts nobody, and a 32-byte key is not guessed. No nginx change. |
| Refused | `401 PHONE_NOT_TRUSTED`; the app forgets the key and offers its usual buttons, so a lapsed key costs one code, never a sign-in. |
| Fails closed | a missing or unreadable file trusts nobody: everyone gets a code, as before. |

Not touched by an account deletion (`anonymise.sql` cannot reach the file):
a deleted person's phone that signs back in gets a **new** account, the same
as signing up again — never the erased one. `/healthz` → `trustedPhones`
counts them, never which. Proved by `tests/auth-guard-trusted.test.js`.

### Changing one's own number, keeping the account (2026-10-03)

Signing in with a new number made a **new** account, and a driver's wallet,
papers and acceptance stayed on the old one. The boss approved a self-service
change in « Mon compte », with no office step.

| Step | Where | What |
|---|---|---|
| `POST {v2,ui}/number/change` | auth guard | needs the caller's session token; `{mobileCountryCode, mobileNumber, channel: sms\|whatsapp\|sms-in}`. Same gates as a sign-in start (open country, the channel available there, both throttles). Asks the shim to `check` **before** any code is spent, then sends one exactly as a sign-in would. Under the `signin` rate limit at the edge (exact location), since it texts. |
| `GET {v2,ui}/number/change/{id}` | auth guard | WhatsApp / SMS-to-us: has the message come, from the new number? |
| `POST {v2,ui}/number/change/{id}/confirm` | auth guard | the code, three strikes and locked like a sign-in; only the session that started it may finish it. Then the shim's `apply`, and a trusted-phone key for the new number. |
| `POST /internal/number-change` | maps-shim | loopback only. Reads whose account it is **from the token** (identity.js), never from the request. Refuses `WRONG_COUNTRY`, `SAME_NUMBER`, `NUMBER_TAKEN` (country code + hash on that side, the way the backend looks a person up). Writes `mobile_number_encrypted` (passetto, `S"…"`), `mobile_number_hash` (sha256 of `NUMBER_HASH_SALT` + number) and `unencrypted_mobile_number`; one `admin_audit` row, `number.change`, with neither number. |

Why not the backend's own dashboard `updatePhoneNumber` for drivers: it exists
and is correct, but it signs the driver out of every session; doing both sides
the same way here keeps the person signed in, and the rider side has no such
route at all. Neither backend caches a person (checked at `03a7531`), so the
very next sign-in finds the account by its new number.

**Same country only.** A driver belongs to his country's merchant, tariff and
wallet currency; that is a transfer, not a number change. **One side at a
time:** the passenger and driver accounts are separate; the other is changed
from its own « Mon compte », and the trusted-phone key spares the second code.

**The passenger's photograph moves with the number (2026-10-03).** It is
stored under her number's hash (`avatars.js`, `h_<hash>`), so the first
change left it under the old key and her profile went blank — the owner's
own, on the first real use. `apply` now moves it to the new key in the same
step (`avatars.moveRiderKey`); a driver's is keyed by his id and never moves.
The app moves its own local copy the same way.

**So does her rating (2026-10-03).** What drivers made of her is not on her
person row but on the provider's `rider_details`, one row per number (unique
on hash + country code), found by the number's hash on every booking. A change
left it on the old number and her profile read « Nouveau ». `apply` now
re-points that row at the new number in the same transaction as the person
row, keeping its id so past bookings stay linked; if the new number already
had a row, count and sum are added and the average recomputed (score ÷ count,
as the provider does), and that row is set aside with its hash cleared —
nothing deleted, bookings point at both. A driver's own rating is by id and
never moves.

`NUMBER_HASH_SALT` is in `/opt/ny/secrets/number-change.env` (the backend's
encHashSalt — verified against stored hashes on both schemas before use); the
shim answers `not_configured` without it. Proved by
`tests/auth-guard-number-change.test.js` and `tests/number-change.test.js`.
