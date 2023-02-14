# Mock Google

Mock for Google with hardcoded values for using in stack test
`SnapToRoads` results is quite huge. So that, instead of hardcoding them, storing results from real google into file was implemented. When run stack test next time these results already will be the part of repository, so no need to call real google again.

**Changes in dhall configs**

`mock-google.dhall` has two options:
- use real google for snap to road api if required results are not there in `mock-data` files
- do not use real google, throw `501 Not Implemented` error if required results are not there in `mock-data` files

default config value - do not use real google

`integration-tests.dhall`contains:
- google config (we can specify real google or mock-google)
- encryption options

default config value - mock-google

Before starting integration tests, `MerchantServiceConfig` for google service in bap and bpp cache will be changed to the value from this dhall config. DB config values don't  change. After integration tests passed, cache cleared, and we can use old values from DB.

If some test fails, try to run `stack exec mock-google-exe` manually before running tests
