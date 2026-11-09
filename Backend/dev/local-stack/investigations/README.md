# investigations/

How this system was understood: 36 probes, written between August and
November 2026, each answering one question by asking the running stack —
how a booking behaves, what a route costs, why a driver was not offered a
ride, whether a push arrives. Kept, because several answer questions that
will be asked again. **None of them is part of anything that runs**, and
nothing here is deployed.

Most run ON the server (the rider API on 8013 and the driver API on 8017 are
loopback-only): copy one to `/tmp` there and run it. Several sign in with the
backend's fixed code behind the auth guard — that creates accounts on the live
server, so read a probe's header before running it. The ride test that is
still in use is not here: it is `ops/checks/probe-two-country-rides.py`.

Three of them test the driver's monthly subscription, retired on 2026-10-07
(phase 6) — `probe-subscription-flow.py`, `probe-subscription-live.py` and
`probe-restricted-drivers.py`. Its routes now answer 410 and its tables are
gone, so they fail; they stay as the record of how it was proved.
`probe-subscription.sql`, which measured that the backend has no billing
tables of its own, is still true.

`probe-place-types.sh` (2026-11-09, « Lieux proposés ») is a pair with its
`.sql` and needs `stack/db/suggested-places.sql` copied beside it: it counts
the place index per type and country with the mapping that ships. Read-only
map data; its header has the two commands. See docs/maps.md, *Proposed
places*.
