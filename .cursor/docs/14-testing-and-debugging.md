# Debugging

## Mock Services

For local development and testing, mock services simulate external dependencies:

| Mock | Path | Simulates |
|------|------|-----------|
| Google Maps | `app/mocks/google/src/` | Maps API |
| Idfy | `app/mocks/idfy/src/` | Document verification |
| FCM | `app/mocks/fcm/src/` | Push notifications |
| SMS | `app/mocks/sms/src/` | SMS gateway |

Start all mocks with: `, run-mobility-stack-dev`

## Debugging Patterns

### Common Compilation Errors

| Error | Cause | Fix |
|-------|-------|-----|
| `Variable not in scope` | Missing import | Add import to module |
| `Unused import` | Extra import (is `-Werror`) | Remove the unused import |
| `Ambiguous occurrence` | Same name from multiple imports | Use qualified import |
| `Not in scope: type constructor` | Missing type import | Add to YAML spec imports or Haskell imports |
| `No instance for (FromJSON X)` | Missing deriving | Add `derive: HttpInstance` in YAML or manual instance |
| `Couldn't match expected type` | Type mismatch | Check `fromTType`/`toTType` in YAML spec |

### Debugging Tips

1. **Fast feedback loop**: `, ghcid lib/<package-name>` for continuous compilation
2. **Check generated code**: Look in `src-read-only/` to see what was generated from YAML
3. **Logging**: Use `logInfo`, `logDebug`, `logError` from `Kernel.Utils.Logging`
4. **Redis inspection**: Check cached values when data seems stale
5. **Multi-cloud issues**: Remember Redis doesn't replicate — check both instances

### Build Debugging

```bash
# Build specific package to isolate errors
cabal build rider-app
cabal build dynamic-offer-driver-app

# Regenerate .cabal files if module list is wrong
, hpack

# Regenerate all code from specs
, run-generator --all

# Kill lingering processes
, kill-svc-ports
```

### Common Runtime Issues

| Issue | Check |
|-------|-------|
| 404 on API endpoint | Verify API spec YAML, check generated API module |
| DB query returns nothing | Check table name, column names, migration applied |
| Cache stale | Check TTL, invalidation logic, multi-cloud Redis |
| Beckn callback not received | Check BPP URL, subscriber registration, ACL module |
| Driver not getting requests | Check allocator service, driver pool config |

## Related Docs

- Build commands: `02-build-and-dev.md`
- Database patterns: `08-database-patterns.md`
- Multi-cloud debugging: `12-multi-cloud.md`
