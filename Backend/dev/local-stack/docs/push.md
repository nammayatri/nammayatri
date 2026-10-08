# Push notifications

Firebase for Android, the push relay for iPhones, and what each notification says.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Push notifications — `./apply-fcm.sh`

Live since 2026-08-18, Firebase project **`movin-dz`**. Worth reading before
touching, because almost everything written down about this was wrong.

**Push was never missing.** `Kernel.External.FCM.Flow` is compiled into both
binaries, eleven message types exist, and the rider app has been collecting
device tokens since it shipped. The only broken thing was the key: upstream's
placeholder ships with `project_id: jp-beckn-dev` and a private key that is
literally `xxxxxxx`, so every send died at JWT signing with

```
[FCM] |> error while sending message to person with id … : "Bad RSA key!"
```

Three columns and a restart fixed it. **No rebuild, no new container** — the same
trick as maps and routing.

```bash
./apply-fcm.sh /path/to/service-account.json
```

It writes **both** sides: `atlas_app.merchant` for the rider and
`atlas_driver_offer_bpp.transporter_config` for the driver, which carried the
same dead placeholder under different column names. One Firebase service account
is scoped to the *project*, not to an app, so the key installed today already
serves the driver app the day it exists.

### `fcm_url` must be the whole endpoint

Neither binary contains a `projects` or `messages:send` string, so nothing is
assembled at runtime — the column holds the complete URL, project id included:

```
https://fcm.googleapis.com/v1/projects/movin-dz/messages:send
```

That was a guess until the first real send put the path in the log. Override with
`FCM_URL=` if it ever changes.

### Reading the result

The three outcomes are easy to tell apart and only one is a problem:

| In the log | Means |
|---|---|
| `Bad RSA key!` | the key did not take |
| `404` on the URL | `fcm_url` is wrong |
| `INVALID_ARGUMENT` on `message.token` | **everything is right** — that device token is not a real FCM token |

The last one is what a probe will always produce, because probes invent their
device tokens. It is a pass, not a failure.

### The text is English in the binary, and it does not matter

The notification wording is compiled in — `"Driver assigned!"`, `"Karim will be
your driver for this trip."` — with **no template table and no merchant column**
to override it. Checked by searching the executable and by listing every table in
both schemas. The client wants French only, which looks like a rebuild.

It is not, because of one detail in the payload:

```json
{"message":{"token":"…","apns":{…},"android":{"data":{…}}}}
```

The Android half carries **`data` and no `notification` block**. Android renders
a `notification` message itself, with the server's words, and cannot be stopped.
A **data-only** message it does not render at all — it wakes the app and hands
the payload over. So the server says *what happened* (`notification_type`) and
the app chooses every word. The French text lives in the app, in
`src/lib/notifications.ts`.

### The eleven types

`QUOTE_RECEIVED`, `DRIVER_QUOTE_INCOMING`, `DRIVER_ASSIGNMENT`,
`DRIVER_ON_THE_WAY`, `DRIVER_HAS_REACHED`, `TRIP_STARTED`, `TRIP_FINISHED`,
`CANCELLED_PRODUCT`, `REALLOCATE_PRODUCT`, `EXPIRED_CASE`,
`REGISTRATION_APPROVED`.

Four are shown, on the client's instruction: a driver answered, ride confirmed,
driver on the way, driver arrived. The rest are translated and silent.

### The device tokens already in the table are useless

44 of 55 riders have a `device_token` and **not one is an FCM token** — the app
minted them with `Math.random` as a stand-in while push was believed impossible.
Firebase rejects every one. No existing rider receives anything until they open a
build that registers a real token and posts it to `/v2/profile`.

### iPhones — the push relay, `maps-shim/push-relay.js`, since 2026-09-16

**`fcm_url` no longer points at Google.** Both sides point at the shim:

```
atlas_app.merchant                        http://localhost:8030/push/rider/v1/projects/movin-dz/messages:send
atlas_driver_offer_bpp.transporter_config http://localhost:8030/push/driver/v1/projects/movin-dz/messages:send
```

iPhones received nothing, for two reasons that stack. iOS hands the app an
**APNs** token, which Firebase rejects. And even with a real FCM token, iOS would
draw the backend's own `apns.alert` itself — the English compiled into the binary,
for **every** type, including the ones the client keeps silent. The payload has
no `mutable-content` (searched in the binary: `FCMApnsConfig`, `FCMaps`,
`FCMAlert` are there; `content-available` and `mutable-content` are not), so the
app cannot rewrite it. Both fixes in the backend are rebuilds; the relay is not.

What it does with each send:

| Token | Goes to | Words |
|---|---|---|
| FCM (long, contains `:`) | Google, **byte for byte**, with the backend's own bearer | the app's, as before |
| `apns:{fr\|ar\|en}:{hex}` (app since 2026-09-16) | APNs directly | the app's shown entries, in that language |
| bare 64-hex (older iOS builds) | APNs directly | French |

The words are a **copy** of the shown entries in the app's
`src/lib/notifications.ts` — change a word or a `shown` flag there, then in
`push-relay.js`. A silent type answers `200` and sends nothing. Loopback callers
only; the edge does not expose `/push/`.

**The APNs key** is not in git: `key.p8` and `config.json`
(`keyId`, `teamId` `3T75H4J6T7`, `topic` `net.movinapp.app`) inside
`ny-maps-shim:/data/avatars/.apns/` — the shim's one persistent volume, and a
path `avatars.js` cannot serve (it only resolves hash-named `.jpg`/`.png`). Read
on every send, so installing it needs no restart. `/healthz` → `push.apns`.

**Proving it without an iPhone:** POST a send with a fake token
`apns:fr:000…0` (64 zeros) to the relay. `400 BadDeviceToken` from Apple means
the key was **accepted**; `InvalidProviderToken` means key id, team or key is
wrong. Measured 2026-09-16: `BadDeviceToken`.

**The switch needed a cache drop.** Both backends cache this config in Redis —
`app-backend:CachedQueries:Merchant:Id-…` and
`driver-offer:CachedQueries:TransporterConfig:MerchantId-…` — so an `UPDATE` and a
restart alone would have changed nothing until the entries expired. True of every
column on those two tables.

Rollback: replace `http://localhost:8030/push/{rider,driver}/` with
`https://fcm.googleapis.com/` in both tables, drop the two cache keys, restart
`ny-rider ny-driver`. The pre-switch values are in
`backups/fcm-url-pre-relay-20260916T102737Z.txt` on the server.
