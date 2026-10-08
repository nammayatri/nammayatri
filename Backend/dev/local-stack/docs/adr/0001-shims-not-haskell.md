# 0001 — Shims, not Haskell

**Status:** in force since August 2026. **Where it shows:** `stack/maps-shim/`,
`stack/auth-guard/`, `stack/edge/nginx.conf`.

## Context

The services run as prebuilt binaries (upstream `03a7531` plus our patches —
[0003](0003-patches-at-build-time.md)). Rebuilding them is affordable — 44
minutes cold, 8 warm — but every measurement in this project (routes, prices,
dispatch, the ride flow in both countries) was taken against the binaries that
are running. New binaries mean re-proving all of it. And the source tree in this
repository is ~10 800 commits newer than what runs, so reading it is not a
reliable guide to what the binary does ([driver-api.md](../driver-api.md)).

## Decision

When the deployment needs new behaviour, put it **beside** the backend, not in
it, in this order of preference:

1. **Config or SQL** the backend already reads ([0002](0002-config-not-code.md)).
2. **A shim** — a small Node service in front of or behind the backend:
   `maps-shim` (routing via OSRM, place search, the wallet, the dispatch list,
   the push relay, avatars, ratings, account deletion requests) and
   `auth-guard` (sign-in codes over SMS/WhatsApp, attempt limits, the
   `WALLET_EMPTY` gate, rating reports).
3. **nginx**, for what is about requests rather than data (the OTP attempt
   limit lives there, not in the Haskell that already had a counter).
4. **A Haskell patch** only when nothing above can reach it — and then the
   smallest one: the dispatch filter reads one Redis key and knows nothing of
   wallets or prices ([wallet.md](../wallet.md), *Dispatch*).

## Consequences

- A rule change is a container restart, not a build. Prices, the day's cost,
  open countries, SMS limits are environment variables or rows.
- The shims carry real business rules (who may work, who may sign in), so they
  are tested like software: golden files per route, the money path in a real
  Postgres, every test in CI on every push ([testing.md](../testing.md)).
- There is more than one place to look. `stack/README.md` and the layout in
  the [README](../../README.md) say which service owns what.
- The trap it creates: a config knob that sits behind a compiled-in switch is
  not an integration point. `useFakeSms = Some 7891` is why the SMS gateway is
  in the guard, not in `Sms_MyValueFirst` ([sign-in.md](../sign-in.md)).
