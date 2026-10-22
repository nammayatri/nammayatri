# 0004 — One driver merchant per country

**Status:** in force since 2026-09-13. **Where it shows:**
`stack/db/two-countries-merchants.sql`, `mauritania-tariff.sql`,
`algeria-tariff.sql`, every per-merchant script.

## Context

The client ran Algeria, replaced it with Mauritania on 2026-09-03, then chose to
run both. Prices live on the driver side: `fare_policy` is per merchant and
vehicle variant, and this binary has no operating-city level. Two countries
need two currencies and two price tables.

## Decision

Each country is its own **driver merchant** — `favorit0-…` for Mauritania,
`algeria0-0000-0000-0000-00000algeria` for Algeria, a clone of the proven one —
and the rider side keeps **one** merchant, `YATRI`, serving both. The gateway
sends every search to both driver merchants; each drops the other country's.
Geofences exist on both sides, and both must say the same thing
([countries.md](../countries.md), *Two countries*).

## Consequences

- **Every per-merchant statement must name its merchant.** An unkeyed tariff
  reprices the other country; both tariff files are keyed now.
- **Audit every per-message lock.** The search handler locked on the message id
  alone and silently dropped whichever merchant arrived second; patched to
  merchant + message ([0003](0003-patches-at-build-time.md)).
- Anything that reports or charges per driver goes by the driver's merchant:
  the wallet's day price and gateway, the bot's queries, the console.
- A third country is another merchant clone, another tariff file and another
  extract on the combined map.
