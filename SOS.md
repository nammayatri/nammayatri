# Safety Shared-Services Revamp — Implementation Plan

## Overview

Migrate the SOS/Safety domain from duplicate rider-app and driver-app implementations into a unified shared-services library using SOLID principles and the **Bridge + Builder** design patterns. The goal is a single source of truth for all Safety handler logic, with platform differences (rider vs driver) resolved through a `SafetyHandle` bridge and environment-injected at startup.

---

## Design Patterns

| Pattern | Role |
|---|---|
| **Builder** | Constructs uniform `SafetyCtx` from platform-specific auth tuples (2-tuple for rider, 3-tuple for driver) |
| **Bridge** | Decouples Safety operation abstraction from platform-specific implementations via `SafetyHandle` |
| **Facade** | `withSafetyCtx` hides Builder complexity behind a single domain entry point |

---

## Core Design

### 1. Domain-Level State — `SafetyCtx`

Uniform downstream state produced by the Builder. All domain functions consume this — no platform-specific types leak in.

```haskell
-- shared-services/src/Safety/Common/Types.hs

data SafetyCtx = SafetyCtx
  { personId         :: Id Safety.Common.Person
  , merchantId       :: Id Safety.Common.Merchant
  , merchantOpCityId :: Id Safety.Common.MerchantOperatingCity
  , personDefaults   :: SafetySettingsPersonDefaults
    -- rider: populated from Person fields
    -- driver: all Nothing (Person has no safety fields)
  }

-- Rider's Person has safety fields inline; driver's Person has none.
-- This record normalises the difference.
data SafetySettingsPersonDefaults = SafetySettingsPersonDefaults
  { falseSafetyAlarmCount       :: Maybe Int
  , hasCompletedMockSafetyDrill :: Maybe Bool
  , hasCompletedSafetySetup     :: Maybe Bool
  , nightSafetyChecks           :: Maybe Bool
  , safetyCenterDisabledOnDate  :: Maybe UTCTime
  , shareEmergencyContacts      :: Maybe Bool
  , informPoliceSos             :: Maybe Bool
  }

emptyPersonDefaults :: SafetySettingsPersonDefaults
emptyPersonDefaults = SafetySettingsPersonDefaults
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- Narrow person type — shared lib never imports app-specific Person
data PersonE = PersonE
  { id               :: Id Safety.Common.Person
  , merchantId       :: Id Safety.Common.Merchant
  , merchantOpCityId :: Id Safety.Common.MerchantOperatingCity
  , mobileNumber     :: Maybe Text
  , name             :: Maybe Text
  , language         :: Maybe Language
  }
```

---

### 2. Bridge Implementor — `SafetyHandle`

Record of callbacks. Shared domain functions depend only on this interface — never on rider-app or driver-app types. Each platform provides its own concrete implementation in its `Environment`.

```haskell
-- shared-services/src/Safety/Common/Handle.hs

data SafetyHandle m = SafetyHandle
  { -- Builder callbacks (used during SafetyCtx construction)
    fetchMerchantOpCityId   :: Id Safety.Common.Person
                            -> Id Safety.Common.Merchant
                            -> m (Id Safety.Common.MerchantOperatingCity)
    -- rider: DB lookup; driver: never called (opCityId is in auth 3-tuple)

  , fetchPersonDefaults     :: Id Safety.Common.Person
                            -> m SafetySettingsPersonDefaults
    -- rider: reads safety fields off Person record
    -- driver: returns emptyPersonDefaults

  -- Domain callbacks
  , findPersonById          :: Id Safety.Common.Person -> m (Maybe PersonE)

  , notifyEmergencyContacts :: SafetyCtx -> NotifyContactsReq -> m ()
    -- rider/driver have different contact resolution logic

  , sendSosPushNotification :: SafetyCtx -> SosPushReq -> m ()

  , resolveRideContext      :: SafetyCtx -> Id Safety.Common.Ride
                            -> m (Maybe RideContextE)

  , createSosTicket         :: SafetyCtx -> SosTicketReq -> m (Maybe Text)
    -- shared: Kapture ticket creation; queue differs per platform

  , onSosResolved           :: SafetyCtx -> Id Sos -> m ()

  , platformCapabilities    :: SafetyCapabilities
  }

data SafetyCapabilities = SafetyCapabilities
  { supportsIvr        :: Bool  -- rider only
  , supportsMockDrill  :: Bool  -- rider only
  , supportsPoliceCall :: Bool  -- rider only
  }
```

---

### 3. Builder — `BuildSafetyCtx`

Resolves the auth tuple asymmetry (rider 2-tuple vs driver 3-tuple) into a uniform `SafetyCtx`. This is the "input function for token verification" — the domain entry point.

```haskell
-- shared-services/src/Safety/Common/Builder.hs

class BuildSafetyCtx authToken m where
  buildSafetyCtx :: SafetyHandle m -> authToken -> m SafetyCtx

-- Rider: 2-tuple — needs extra step to resolve MerchantOperatingCity
instance MonadFlow m =>
    BuildSafetyCtx (Id AppPerson.Person, Id AppMerchant.Merchant) m where
  buildSafetyCtx handle (rawPersonId, rawMerchantId) = do
    let personId   = cast rawPersonId
        merchantId = cast rawMerchantId
    merchantOpCityId <- fetchMerchantOpCityId handle personId merchantId
    personDefaults   <- fetchPersonDefaults handle personId
    pure SafetyCtx {..}

-- Driver: 3-tuple — opCityId already present, no DB call needed
instance MonadFlow m =>
    BuildSafetyCtx (Id DrvPerson.Person, Id DrvMerchant.Merchant, Id DMOC.MerchantOperatingCity) m where
  buildSafetyCtx handle (rawPersonId, rawMerchantId, rawOpCityId) = do
    let personId         = cast rawPersonId
        merchantId       = cast rawMerchantId
        merchantOpCityId = cast rawOpCityId
        personDefaults   = emptyPersonDefaults
    pure SafetyCtx {..}

-- Facade: single domain entry point
withSafetyCtx
  :: BuildSafetyCtx authToken m
  => SafetyHandle m -> authToken -> (SafetyCtx -> m a) -> m a
withSafetyCtx handle token action =
  buildSafetyCtx handle token >>= action
```

---

### 4. Handle in Environment

The `SafetyHandle` lives in each app's `Env`. Handler functions resolve it via a typeclass constraint — no explicit handle passing needed. The generated Servant wiring calls handlers the same way as any other handler.

```haskell
-- shared-services/src/Safety/Common/Handle.hs (addition)
class HasSafetyHandle r m where
  getSafetyHandle :: m (SafetyHandle m)

-- rider-app/src/Environment.hs
data AppEnv = AppEnv
  { ...
  , safetyHandle :: SafetyHandle Flow
  }

instance HasSafetyHandle AppEnv Flow where
  getSafetyHandle = asks (.safetyHandle)

-- rider-app startup
mkAppEnv cfg = do
  ...
  pure AppEnv { ..., safetyHandle = mkRiderSafetyHandle }

-- driver-app/src/Environment.hs — same pattern with mkDriverSafetyHandle
```

---

### 5. Shared Handler Layer — DRY

All handler logic lives here. Both platforms use this. Generated as stubs from the DSL, filled in once.

```haskell
-- shared-services/src/Safety/Domain/Handler/Sos.hs

postSosCreate
  :: (BeamFlow m r, HasSafetyHandle r m, BuildSafetyCtx authToken m)
  => authToken -> SosReq -> m SosRes
postSosCreate token req = do
  handle <- getSafetyHandle
  withSafetyCtx handle token $ \ctx ->
    Action.createSos handle ctx req

getSosGetDetails
  :: (BeamFlow m r, HasSafetyHandle r m, BuildSafetyCtx authToken m)
  => authToken -> Id Ride -> m SosDetailsRes
getSosGetDetails token rideId = do
  handle <- getSafetyHandle
  withSafetyCtx handle token $ \ctx ->
    Action.getSosDetails handle ctx rideId

-- getSosTracking: NoAuth — no token, no ctx
getSosTracking
  :: BeamFlow m r
  => Id Sos -> m SosTrackingRes
getSosTracking = Action.getSosTracking

-- ... same pattern for all shared endpoints
```

---

## DSL and Spec Structure

### Spec Directory Layout

```
Backend/lib/shared-services/spec/Safety/
  safety-common.dhall                        [MODIFY] add API generation support

  RiderPlatform/                             [NEW]
    dsl-config.dhall
    API/
      Sos.yaml                               shared + rider-specific endpoints

  ProviderPlatform/                          [NEW]
    dsl-config.dhall
    API/
      Sos.yaml                               shared + driver-specific endpoints

  Storage/
    Sos.yaml                                 [EXISTING]
    SafetySettings.yaml                      [EXISTING]
    PersonDefaultEmergencyNumber.yaml        [EXISTING]
```

### dsl-config Output Path Mapping

| Field | RiderPlatform | ProviderPlatform |
|---|---|---|
| `_domainHandler` | `shared-services/src/Safety/Domain/Handler/` | `shared-services/src/Safety/Domain/Handler/` (same) |
| `_servantApi` | `rider-app/src-read-only/API/Action/UI/Safety/` | `driver-app/src-read-only/API/Action/UI/Safety/` |
| `_apiRelatedTypes` | `shared-services/src-read-only/API/Types/RiderPlatform/Safety/` | `shared-services/src-read-only/API/Types/ProviderPlatform/Safety/` |
| `_extraApiRelatedCommonTypes` | `shared-services/src/Safety/Common/` | `shared-services/src/Safety/Common/` (same) |

Both platforms point `_domainHandler` to the same shared-services path. Both point `_extraApiRelatedCommonTypes` to the same `Safety/Common/` — one common types file shared between platforms.

### Generator Types Per Config

```dhall
_generate =
  [ GeneratorType.DOMAIN_HANDLER           -- stubs → shared-services/src/Safety/Domain/Handler/
  , GeneratorType.SERVANT_API              -- wiring → each app's src-read-only (auth type differs)
  , GeneratorType.API_TYPES                -- Servant types → shared-services/src-read-only/API/Types/<Platform>/Safety/
  , GeneratorType.EXTRA_API_COMMON_TYPES_FILE  -- shared req/res types → Safety/Common/
  ]
```

---

## File Structure After Implementation

```
Backend/lib/shared-services/

  src/Safety/
    Common/
      Types.hs           SafetyCtx, SafetySettingsPersonDefaults, PersonE, SafetyCapabilities
      Handle.hs          SafetyHandle, HasSafetyHandle
      Builder.hs         BuildSafetyCtx typeclass, instances, withSafetyCtx
      Sos.hs             [GENERATED] shared request/response types (EXTRA_API_COMMON_TYPES_FILE)

    Domain/
      Action/UI/
        Sos.hs           core domain logic — createSos, updateStatus, markSafe, startTracking etc.
      Handler/
        Sos.hs           [GENERATED STUB, filled once] getSafetyHandle >>= withSafetyCtx >>= Action.*

  src-read-only/Safety/
    API/Types/
      RiderPlatform/Safety/    [GENERATED] Servant types with ApiAuthV2 'RIDER_TYPE
      ProviderPlatform/Safety/ [GENERATED] Servant types with ApiAuthV2 'DRIVER_TYPE

Backend/app/rider-platform/rider-app/Main/
  src-read-only/API/Action/UI/Safety/
    Sos.hs               [GENERATED] Servant route wiring → calls Safety.Domain.Handler.Sos.*
  src/
    Environment.hs       [MODIFY] add safetyHandle :: SafetyHandle Flow + HasSafetyHandle instance
    Domain/Action/UI/
      Safety/
        Handle.hs        [NEW] mkRiderSafetyHandle — the concrete Bridge Implementor

Backend/app/provider-platform/dynamic-offer-driver-app/Main/
  src-read-only/API/Action/UI/Safety/
    Sos.hs               [GENERATED] Servant route wiring → calls Safety.Domain.Handler.Sos.*
  src/
    Environment.hs       [MODIFY] add safetyHandle :: SafetyHandle Flow + HasSafetyHandle instance
    Domain/Action/UI/
      Safety/
        Handle.hs        [NEW] mkDriverSafetyHandle — the concrete Bridge Implementor
```

---

## API Split: Shared vs Platform-Specific

### Shared (both RiderPlatform/API/Sos.yaml and ProviderPlatform/API/Sos.yaml)

| Endpoint | Auth |
|---|---|
| `GET /sos/getDetails/{rideId}` | TokenAuth |
| `POST /sos/create` | TokenAuth |
| `POST /sos/{sosId}/status` | TokenAuth |
| `POST /sos/markRideAsSafe/{sosId}` | TokenAuth |
| `POST /sos/{sosId}/updateLocation` | TokenAuth |
| `GET /sos/{sosId}/tracking` | NoAuth |
| `POST /sos/startTracking` | TokenAuth |
| `POST /sos/updateState/{sosId}` | TokenAuth |
| `GET /sos/trackingDetails/{sosId}` | TokenAuth |

### Rider-Only (RiderPlatform/API/Sos.yaml only)

| Endpoint | Purpose |
|---|---|
| `GET /sos/IvrOutcome` | IVR callback |
| `POST /sos/createMockSos` | Mock SOS drill |
| `POST /sos/callPolice` | Police call trigger |
| `POST /sos/{sosId}/updateToRide` | Non-ride → ride SOS upgrade |

### Driver-Only (ProviderPlatform/API/Sos.yaml only)

| Endpoint | Purpose |
|---|---|
| `GET /sos/rideDetails/{rideId}` | Driver ride context for SOS |
| `POST /sos/uploadMedia` | Media file upload for SOS |

---

## Implementation Phases

### Phase 1 — Core Abstractions (shared-services/src/Safety/Common/)
- `Types.hs` — SafetyCtx, SafetySettingsPersonDefaults, PersonE, SafetyCapabilities
- `Handle.hs` — SafetyHandle record, HasSafetyHandle typeclass
- `Builder.hs` — BuildSafetyCtx typeclass, rider/driver instances, withSafetyCtx

### Phase 2 — DSL Config and Spec Files
- Modify `safety-common.dhall` to add API generation support (mkDefaultImports, updated defaultConfigs)
- Create `RiderPlatform/dsl-config.dhall` and `RiderPlatform/API/Sos.yaml`
- Create `ProviderPlatform/dsl-config.dhall` and `ProviderPlatform/API/Sos.yaml`
- Run `make generate-code` — generates Handler stubs, Servant wiring, API types

### Phase 3 — Fill Handler Stubs (shared-services/src/Safety/Domain/Handler/Sos.hs)
- Fill generated stubs with `getSafetyHandle >>= withSafetyCtx >>= Action.*` pattern
- Rider-specific handler functions (IVR, mock SOS, police call)

### Phase 4 — Concrete Bridge Implementors
- `rider-app/src/Domain/Action/UI/Safety/Handle.hs` — `mkRiderSafetyHandle`
- `driver-app/src/Domain/Action/UI/Safety/Handle.hs` — `mkDriverSafetyHandle`
- Modify `AppEnv` in both apps — add `safetyHandle` field + `HasSafetyHandle` instance

### Phase 5 — Wire and Verify
- Remove old duplicate handler bodies from rider-app and driver-app `Domain/Action/UI/Sos.hs`
- Build: `cabal build all`
- Verify generated Servant wiring imports `Safety.Domain.Handler.Sos`
- Runtime test: existing SOS endpoints on both platforms

### Phase 6 — Cleanup
- Remove `rider-app/Main/spec/API/sos.yaml` (old spec)
- Remove generated files from old spec locations

---

## SOLID Mapping

| Principle | How it's applied |
|---|---|
| **Single Responsibility** | `SafetyCtx` owns only auth state; `SafetyHandle` owns platform callbacks; `Handler` owns wiring; `Action` owns domain logic |
| **Open/Closed** | New platform = new `BuildSafetyCtx` instance + `mkNewPlatformHandle`. Zero changes to existing code |
| **Liskov Substitution** | Rider and driver handles are substitutable wherever `SafetyHandle m` is required |
| **Interface Segregation** | `HasSafetyHandle`, `BuildSafetyCtx`, `BeamFlow` are separate constraints — handlers depend only on what they use |
| **Dependency Inversion** | Handlers depend on `SafetyHandle` abstraction, not on concrete rider/driver types |

---

## Dependency Graph

```
Servant combinator
  (rider 2-tuple / driver 3-tuple from ApiAuthV2)
         │
         ▼
  withSafetyCtx  ──────────────────── Facade
         │
         ▼
  BuildSafetyCtx instance  ─────────── Builder
    step 1: cast raw IDs to Safety phantom types
    step 2: fetchMerchantOpCityId  (rider: DB call | driver: skip)
    step 3: fetchPersonDefaults    (rider: Person fields | driver: empty)
         │
         ▼
     SafetyCtx  (uniform from here down)
         │
    ┌────┴─────────────────┐
    │                      │
SafetyHandle            Domain Action fns  ─── Bridge Abstraction
(Implementor interface)     (createSos, updateStatus, etc.)
    │
  ┌─┴──────┐
Rider    Driver  ──────────────────────────── Concrete Implementors
Handle   Handle
(in AppEnv, injected at startup)
```
