# driver-pool-state stream check

End-to-end test for the `driver-pool-state` Kafka topic that the driver app
publishes through `publishDriverPoolData`.

It runs any test command (normally a ride suite) and records, side by side:

- every event on the `driver-pool-state` topic, and
- every change to each driver's `DriverPoolData` in LTS Redis
  (`dynamic-offer-driver-app-lts:driver-pool-data:<driverId>`), polled every 100 ms.

Then it checks, per driver:

| Check | Fails when |
|---|---|
| ts | an event's `ts` is not wall-clock epoch ms from this run |
| shape | an event has empty `changes` or a field `DriverPoolData` doesn't have |
| coverage | a field changed in LTS but no event carried that new value around the same time |
| final | folding all events in order gives a value LTS doesn't end with |

## Run

Needs the local stack (`, run-mobility-stack-dev` or `-full`) with
`driverPoolStateTopic = Some "driver-pool-state"` and config synced. From this
directory's parent, inside `nix develop .#backend`:

```bash
cd Backend/dev/integration-tests
uv run driver-pool-state/check_stream.py -- ./run-tests.sh auto NY_Bangalore 01-AutoRideFlow
```

Exit code 0 means the ride suite passed and the stream matched LTS; 1 otherwise.

Options: `--bootstrap` (default `127.0.0.1:29092`), `--redis-port` (6379),
`--prefix` (LTS key prefix), `--poll-ms`, `--settle-s`, `--slack-s`. See `--help`.

## Limits

- LTS is polled, so two changes to the same field within one poll interval show
  up as one; the final check still covers the end value.
- Changes made before a driver's LTS entry exists (the sync skips those drivers)
  have nothing in LTS to compare against; they are reported, not failed.
