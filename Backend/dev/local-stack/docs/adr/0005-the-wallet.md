# 0005 — The wallet

**Status:** in force since 2026-09-07 (hard block since 2026-09-14). Replaced
the monthly subscription, retired 2026-10-07. **Where it shows:**
`stack/db/driver-wallet.sql`, `maps-shim/wallet.js`, `maps-shim/restricted.js`,
the guard's `WALLET_EMPTY`.

## Context

Drivers first paid 3 000 DA a month through Chargily. Algerian cards (CIB,
Edahabia) cannot be debited automatically, so the subscription had to be
pay-then-extend, every renewal the driver's own action — and a driver who did
not drive that month still paid for it. The backend has no table for money of
any kind ([riders.md](../riders.md), *Switching off a driver who has not paid*).

## Decision

**No top-up, no work.** A driver loads credit into a wallet — Moosyl in
Mauritania, Chargily in Algeria — and the day's price (30 MRU, 100 DA) comes
off at his first ride of a day, covering 24 hours. The wallet holds only his
top-ups, never ride money; Movin takes 0 % on rides. Without credit for a day
and no day paid, he may not work, enforced at three layers: dispatch never
offers him a job, the auth guard refuses him (`403 WALLET_EMPTY`), and the app
takes him offline ([wallet.md](../wallet.md)).

## Consequences

- Nobody is charged without driving; there is no renewal and no auto-debit.
- The ledger (`wallet_entry`) is the truth; `wallet.balance` is a cache.
- A webhook never grants credit: the shim reads each top-up back from the
  gateway with our own key, so an unsigned POST cannot put money in a wallet.
- The three layers must agree. Softening any one into a preference breaks the
  client's rule.
- The subscription's tables were dumped and dropped on 2026-10-07 (phase 6);
  its design is in git, the README as of `fe4a44c3a8`.
