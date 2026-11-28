# Haskell Conventions

## Module Organization

| Directory | Purpose | Editable? |
|-----------|---------|-----------|
| `Domain/Types/` | Domain entity definitions | Generated (`src-read-only/`) |
| `Domain/Types/Extra/` | Supplementary types, orphan instances | Yes (`src/`) |
| `Domain/Action/UI/` | Business logic handlers for UI APIs | Yes (`src/`) |
| `Domain/Action/Beckn/` | Beckn callback handlers | Yes (`src/`) |
| `Storage/Queries/` | Database queries | Generated + extra (`src/`) |
| `Storage/CachedQueries/` | Redis-cached queries | Generated + extra (`src/`) |
| `Storage/Beam/` | Beam ORM table definitions | Generated (`src-read-only/`) |
| `API/` | Servant API type definitions | Generated (`src-read-only/`) |
| `Beckn/ACL/` | Beckn protocol translation layers | Yes (`src/`) |
| `SharedLogic/` | Shared business logic | Yes (`src/`) |
| `Tools/` | External service integrations (Maps, SMS, etc.) | Yes (`src/`) |
| `spec/API/` | YAML API specifications | Source of truth |
| `spec/Storage/` | YAML Storage specifications | Source of truth |

## Common Patterns

### ID Generation
```haskell
newId <- generateGUID  -- from Kernel.Utils.Common
```

### Error Handling
```haskell
-- Find with error on missing
person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

-- Custom errors
throwError $ InvalidRequest "Ride already completed"
```

### Database Operations
```haskell
-- Single create (no transaction wrapper)
QBooking.create booking

-- Multiple creates in transaction
runInTransaction $ do
  QBooking.create booking
  QRide.create ride

-- Query
bookings <- QBooking.findAllWithOptionsKV
  [Se.Is BeamB.riderId $ Se.Eq (getId riderId)]
  (Se.Desc BeamB.createdAt)
  (Just limit)
  (Just offset)
```

### Logging
```haskell
logInfo  $ "Processing booking: " <> bookingId.getId
logDebug $ "Debug details: " <> show details
logError $ "Failed to process: " <> show err
```

### Phantom Types
Many domain types use phantom types for safety:
- `UsageSafety` — marks types as safe for use
- `EncryptionStatus` — tracks encryption state

## Import Conventions

### YAML Imports
- Always use **full module paths**: `Domain.Types.IntegratedBPPConfig`
- Never use short names

### Haskell Imports
```haskell
-- Qualified imports for query modules
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.CachedQueries.Person as CQPerson

-- Domain types
import Domain.Types.Booking
import qualified Domain.Types.Booking as DBooking  -- when need qualified

-- Common utilities
import Kernel.Utils.Common
import Kernel.Prelude
```

### Ambiguous record updates (GHC-02256 / `-Wambiguous-fields`)

With `DuplicateRecordFields`, an update like `x {field = …}` is ambiguous when several in-scope types share `field`. Prefer **module qualification of the rarer type**. Use **lenses only when qualification is impractical**.

Keep the common import; add a second qualified import aliased as the rarer type, and qualify fields at that type's update sites:

```haskell
import Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Notification.FCM.Types as FCMAlert (FCMAlert (..))

def{FCMAlert.fcmBody = ..., FCMAlert.fcmTitle = ...}
```

**`Domain.Types.*`:** alias with a `D` prefix (repo convention), not the bare type name:

```haskell
import Domain.Types.RouteStopTimeTable
import qualified Domain.Types.RouteStopTimeTable as DRouteStopTimeTable (RouteStopTimeTable (..))

rt {DRouteStopTimeTable.serviceTierType = ...}
```

Choose which type to qualify by **usage frequency** (qualify the one used less). Many constructions of both types are fine — still prefer qualify. Same-module types (both defined in one imported module) are also fine: dual-import under an alias for the rarer type, as above.

If qualify does not fit, prefer splitting types into separate modules over reaching for lenses.

**Aeson `Options` vs OpenApi / FormUrlEncoded:** when `fieldLabelModifier` / `omitNothingFields` clash with `SchemaOptions` or `FormOptions`, keep the unqualified Aeson import and add a short synonym:

```haskell
import Data.Aeson
import qualified Data.Aeson as Aeson

defaultOptions { Aeson.fieldLabelModifier = ..., Aeson.omitNothingFields = True }
```

**Fallback — lenses (`field'` / `setField`):** only when both types are already unqualified **and** that cannot be fixed simply (e.g. both are defined in the module being edited):

```haskell
logEnv
  & field' @"level" .~ newLogLevel
  & field' @"logRawSql" .~ (newLogLevel == DEBUG)
```

**Avoid:**
- Type annotations `(x :: T){field = ...}` — GHC-02256; type-directed disambiguation is going away
- Blanket `-Wno-ambiguous-fields` for these updates

## Naming Conventions

| Element | Convention | Example |
|---------|-----------|---------|
| Modules | PascalCase | `Domain.Action.UI.Search` |
| Functions | camelCase | `findByPersonId` |
| Types | PascalCase | `BookingStatus` |
| Constructors | UPPER_SNAKE | `TRIP_ASSIGNED` |
| Query modules | `Q<Entity>` prefix | `QBooking`, `QRide` |
| Cached query modules | `CQ<Entity>` prefix | `CQPerson` |
| API endpoints | camelCase paths | `/nyRegular/`, `/driverOffer/` |

## Beckn Tag Rules

All BECKN tags **must be defined** in:
```
lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs
```
before they can be used in any ACL module.

## Orphan Instances

Orphan instances for external types go in `Domain/Types/Extra/*.hs` files. These are opt-in extension modules generated when `EXTRA_DOMAIN_TYPE_FILE` is in the YAML spec.

## Compilation

- Project uses **`-Werror`**: all GHC warnings are treated as errors
- Unused imports, dodgy imports, incomplete patterns → all fail compilation
- Use `cabal build <target>` for comprehensive checks
- `cabal repl` alone doesn't guarantee full compilability
- If a `.hs` file is deleted, update `.cabal` file (run `, hpack`)

## Commit Messages

```
<sub-project>/<type>: <issue-number> <short summary>
```

Types: `feat`, `fix`, `chore`, `ci`, `docs`, `perf`, `refactor`, `test`

Examples:
```
backend/feat: #341 Driver onboarding flow
backend/fix: #789 Fix stale cache in booking lookup
```

## Branch Naming

```
<sub-project>/<type>/<issue-number><short-description>
```

Example: `backend/fix/GH-123/driver-allocation-bug`

## Related Docs

- NammaDSL conventions: `07-namma-dsl.md`
- Database patterns: `08-database-patterns.md`
- Build commands: `02-build-and-dev.md`
