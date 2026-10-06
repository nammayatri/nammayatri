# investigations/

How this system was understood: 35 probes, written between August and
October 2026, each answering one question by asking the running stack —
how a booking behaves, what a route costs, why a driver was not offered a
ride, whether a push arrives. Kept, because several answer questions that
will be asked again. **None of them is part of anything that runs**, and
nothing here is deployed.

Most run ON the server (the rider API on 8013 and the driver API on 8017 are
loopback-only): copy one to `/tmp` there and run it. Several sign in with the
backend's fixed code behind the auth guard — that creates accounts on the live
server, so read a probe's header before running it. The ride test that is
still in use is not here: it is `ops/checks/probe-two-country-rides.py`.
