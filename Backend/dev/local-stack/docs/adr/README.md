# Decisions

Short records of the choices that shape this deployment: what we decided, why,
and what it costs. Each was taken earlier and is written down here after the
fact (phase 7, 2026-10-08), with a pointer to where it was measured.

| # | Decision | In one line |
|---|---|---|
| [0001](0001-shims-not-haskell.md) | Shims, not Haskell | New behaviour goes in a small Node service beside the backend, not into the binaries |
| [0002](0002-config-not-code.md) | Config, not code | Point the backend at our services through its own database config |
| [0003](0003-patches-at-build-time.md) | Patches at build time | The Haskell we run is upstream `03a7531` plus 54 patches applied in CI, not this tree |
| [0004](0004-one-merchant-per-country.md) | One merchant per country | Each country is its own driver merchant; the rider side keeps one |
| [0005](0005-the-wallet.md) | The wallet | Drivers pay per day worked from a wallet they top up; no subscription, no auto-debit |

A new record gets the next number. A decision that is reversed is not
deleted: its record says *Superseded by* and links the new one.
