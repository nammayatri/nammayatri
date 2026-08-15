# Onboarding Action Guard

**Location**: `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/DriverOnboarding/OnboardingFlags/Guard.hs`

The single choke point for driver / vehicle / fleet-owner state changes. Every guarded action names
three things — an **actor** (who is acting), a **verb** (what they are doing), and a **target** (what
it is done to) — and the guard decides whether that combination is allowed.

Read this before adding a new onboarding-adjacent endpoint, before changing who may call an existing
one, or when an action fails with `<Verb> not allowed: … [<CODE>]`.

---

## Entry points

| Function | Use it for | Side effects |
|---|---|---|
| `guardOnboardingAction cfg actor verb target` | **Reads.** Runs the checks and nothing else. | none |
| `withOnboardingAction cfg actor verb target body` | **Mutations.** Wraps `body`. | Redis lock on the target + onboarding-flag recompute afterwards |
| `withOnboardingActionFanout …` | Mutations that must recompute flags for *extra* entities | as above, plus the returned `RecomputeSpec` |

Never wrap a read in `withOnboardingAction` — it takes a per-entity Redis lock
(`Onboarding:Action:<id>`, 30s TTL, 100ms retry) and triggers a flag recompute, neither of which a
read should do.

---

## The pipeline

`guardOnboardingAction` runs five stages in order. **Only the last two are gated** on
`transporterConfig.unifiedOnboardingFlagsRecompute == Just True` ("unified cities"):

| # | Stage | Applies to | Actor-aware? | Unified-gated? |
|---|---|---|---|---|
| 1 | `guardActorScope` | `View` only | **yes** | no |
| 2 | `guardNoLiveRide` | `Unlink`, `Deactivate`, `Delete`, `Disable` | no | no |
| 3 | `guardAssociationAllowed` | `Link`, `Add`, `LinkToFleet` — `TargetDriver` only | no | no |
| 4 | `guardRcAssociationAllowed` | `Link`, `Add`, `Activate`, `LinkToFleet` — vehicle targets | no | no |
| 5 | `guardActorAllowed` + `checkPrecondition` | all verbs | `guardActorAllowed` only | **yes** |

Two consequences worth internalising:

- **In non-unified cities, stages 1–4 are the whole guard.** No precondition tables, no actor
  flag checks. Stage 1 sits outside the gate deliberately: it is an authorization rule, not a
  derived-flag precondition, so it must hold everywhere.
- **Stages 2–4 block Admin exactly as they block a fleet owner.** There is no admin override for
  the live-ride guard or the association guards.

---

## Actors

```haskell
data Actor
  = ActorFleet (Id Person)                    -- also carries operator ids at some call sites
  | ActorDriver (Id Person)
  | ActorFleetAndDriver (Id Person) (Id Person)
  | None                                      -- admin, operator, schedulers, onboarding stages
```

> **The Actor is whatever the call site passes — it is not derived from authentication.** Several
> endpoints in the Fleet namespace pass `None`. For those, the guard applies no fleet self-check and
> no ownership check even though a fleet owner is the caller. See *Known gaps*.

`guardActorAllowed` checks the actor's **own** flags, not ownership:

- `ActorFleet` — checked for **every** verb: must be enabled, not blocked, not disabled
  (`ACTOR-1/2/3`).
- `ActorDriver` — checked only for `Link`, `Activate`, `Add` (`driverActorGoverned`). Teardown and
  onboarding verbs must stay available while a driver is still disabled, or a disabled driver could
  never be cleaned up — nor onboarded, since they are disabled until their documents land.
- `ActorFleetAndDriver` — fleet half always, driver half only for those three verbs.
- Missing `FleetOwnerInformation` / `DriverInformation` row ⇒ the check is **skipped, not failed**
  (`whenJust`).

`TargetFleetOwner` is not actor-governed (`isActorGovernedTarget = False`), so the acting fleet's own
flags never gate a fleet-owner-targeted action.

---

## Targets and ownership (`guardActorScope`)

Fires on `View` only. `None` and `ActorDriver` pass unconditionally — that is how Admin gets
unrestricted read access.

| Target | Fleet actor must satisfy | Error |
|---|---|---|
| `TargetDriver` | active FDA (`associatedTill > now`) with this fleet | `DRIVER_NOT_PART_OF_FLEET` |
| `TargetVehicle` | RC's `fleetOwnerId` = this fleet | `VEHICLE_NOT_PART_OF_FLEET` |
| `TargetVehicleById` | same, by RC id | `VEHICLE_NOT_PART_OF_FLEET` |
| `TargetFleetOwner` | must be itself | `InvalidFleetOwner` |

Reuses `QFDA.findByDriverIdAndFleetOwnerIdWithStatus` and `RCQuery.findLastVehicleRCFleet'`.

---

## Preconditions by target

Skipped entirely outside unified cities, and skipped when the entity row is missing.

### Driver (`checkDriver`)

| Verb | Requires | Code |
|---|---|---|
| `Enable` | is admin-disabled, **not** `FleetDisabled`, verified, approved | `DI-3`, `DI-6`, `DI-2` |
| `Disable` | not already disabled | `DI-3` |
| `Link` | enabled, not blocked, approved | `DI-1`, `D-BLOCKED` |
| `Activate` | enabled, not blocked | `DI-1`, `D-BLOCKED` |
| `Delete` | **not** enabled — disable first | `D-DELETE` |
| `LinkToFleet` | **not** enabled — use `changeFleetOwner` to move an active driver | `DI-9` |
| `SetOnboardingAs` | **not** enabled | `DI-8` |
| `Unlink`, `Deactivate`, `Add`, `Block`, `Unblock`, `Approve`, `Reject`, `View` | always pass | — |

### Vehicle (`checkVehicle`)

| Verb | Requires | Code |
|---|---|---|
| `Link` | `verificationStatus = VALID`, verified, approved | `RI-2`, `RI-1` |
| `Activate` | `VALID`, approved | `RI-2`, `RI-1` |
| `Enable`, `Disable`, `Block`, `Unblock`, `SetOnboardingAs`, `LinkToFleet` | **always rejected** — vehicles have no such flags | `R-UNSUPPORTED` |
| `Unlink`, `Deactivate`, `Add`, `Delete`, `Approve`, `Reject`, `View` | always pass | — |

### Fleet owner (`checkFleet`)

| Verb | Requires | Code |
|---|---|---|
| `Enable` | is admin-disabled, verified, approved | `FI-1`, `FI-2` |
| `Disable` | not already disabled | `FI-1` |
| `Block`, `Unblock` | **always rejected** — use disable/enable | `FI-3` |
| `SetOnboardingAs` | **always rejected** | `F-UNSUPPORTED` |
| everything else | always pass | — |

---

## Admin vs Fleet — the short version

| | Admin (`None`) | Fleet (`ActorFleet…`) |
|---|---|---|
| Read (`View`) | any driver / vehicle / fleet | **only its own** |
| Mutations | full precondition tables | identical, **plus** the fleet must itself be enabled / not blocked / not disabled (unified cities only) |
| Ownership on mutations | n/a | **not checked** |
| Live-ride guard | applies | applies |
| Association guards | apply | apply |

---

## API map

Routes are relative to the module base (`"driver" :> …`) under `/provider/{merchantShortId}/{city}/`.

### `View`
| API | Actor | Module |
|---|---|---|
| `getDriverInfo` | `ActorFleet` for fleet roles, else `None` | `Dashboard/RideBooking/Driver.hs` |

The only endpoint in the codebase that currently gets an ownership check from the guard.

### `LinkToFleet`
`postDriverFleetAddDrivers` (`POST /driver/fleet/addDrivers`) · `postDriverFleetVerifyJoiningOtp` ·
`postDriverOperatorVerifyJoiningOtp` · `postDriverLinkToFleet` (UI) · `postOperatorConsent` (UI) ·
`postFleetConsent` (UI/WMB) · `addReferral` (UI/Referral) — all `ActorFleetAndDriver`.

### `Link`
| API | Target | Actor |
|---|---|---|
| `postDriverFleetDriverChangeFleetOwner` — `POST /driver/fleet/driver/{driverId}/changeFleetOwner` | Driver | `ActorFleetAndDriver` |
| `postDriverFleetLinkRCWithDriver` — `POST /driver/fleet/linkRCWithDriver` | VehicleById | `ActorFleetAndDriver` |
| `postDriverAddVehicle` | VehicleById | `ActorDriver` or `ActorFleet` |
| `postFleetManagementFleetLinkSendOtpUtil`, `postFleetManagementFleetLinkVerifyOtp` | FleetOwner | `ActorFleet` (operator id) |

### `Unlink`
| API | Target | Actor |
|---|---|---|
| `postDriverFleetRemoveDriver` — `POST /driver/{driverId}/fleet/remove/driver` | Driver | `ActorFleetAndDriver` |
| `postDriverFleetRemoveVehicle` — `POST /driver/{vehicleNo}/fleet/remove/vehicle` | Vehicle | `ActorFleet` |
| `unlinkVehicleFromDriver` | Driver | `ActorDriver` / `ActorFleetAndDriver` |
| `postDriverLinkToFleet` (revoke path) | Driver | `ActorFleetAndDriver` |
| `postDriverDeleteRC`, `postDriverUnlinkAadhaar`, `postDriverUnlinkDL`, `postDriverDeleteAadhaar`, `postDriverDeletePanCard`, `postDriverEndRCAssociation` | Driver | `None` |

### `Activate` / `Deactivate`
`postDriverFleetVehicleDriverRcStatus` (`POST /driver/{driverId}/fleet/vehicleDriverRCstatus`,
actor computed) · `postDriverSetRCStatus` (`None`) · `postDriverUnlinkVehicle` (`None`) ·
`processDocumentExpiryReminder`, `processReminderByType` (`None`, scheduler jobs).

### `Enable` / `Disable` — all `None`
`postDriverEnable` · `postDriverDisable` · `postAccountVerifyAccount` (fleet owner, both verbs) ·
`disableDriverForMandatoryReminder` (job).

### `Block` / `Unblock` — all `None`
`postDriverBlock` · `postDriverBlockWithReason` · `postDriverUnblock`
(`POST /driver/{driverId}/unblock`).

### `Delete` — `deleteDriverPermanentlyDelete` (`None`)

### `Approve` / `Reject`
| API | Target | Actor |
|---|---|---|
| `postDriverFleetApproveDriver` | Driver (both verbs) | `ActorFleetAndDriver` |
| `postDriverSubmitReviewRequest` | Driver + FleetOwner | `None` |
| `postDriverUpdateVehicleVariant` — `POST /driver/updateVehicleVariant/{driverId}` | Vehicle | `None` |
| `postDriverUpdateFleetOwnerInfo`, `enableFleetOwnerOnDocsValid` | FleetOwner | `None` |
| `validateImageHandler` (UI) | varies | `None` |

### `Add` — all `None`, all fleet-owner targets
`createFleetOwnerDetails`, `fleetOwnerRegister` — in both `Fleet/Registration.hs` and
`Fleet/RegistrationV2.hs`.

### `SetOnboardingAs`
`postDriverFleetDriverUpdate` (`POST /driver/fleet/driver/{driverId}/update`) and `updateDriver` (UI)
— both `None`.

---

## Adding a new guarded action

1. Pick the verb. If none fits, add a constructor to `ActionVerb` — `-Werror` will then force you to
   handle it in `checkDriver`, `checkVehicle` and `checkFleet`.
2. Decide whether it belongs in `guardNoLiveRide` / `guardAssociationAllowed` /
   `guardRcAssociationAllowed`. An unlisted verb no-ops in all three.
3. Pass a **real** actor. `None` means "no actor accountable"; use it for admin, schedulers, and the
   onboarding/registration stages only.
4. Use `withOnboardingAction` for mutations, `guardOnboardingAction` for reads.

## Known gaps

- **Mutations have no ownership check.** `guardActorScope` fires on `View` only. Widening it needs
  per-verb care: `LinkToFleet` exists precisely to link a driver who is *not yet* in the fleet, and
  `Operator/FleetManagement.hs` passes `ActorFleet operator.id` with a `TargetFleetOwner` that the
  operator does not "own" in the FDA sense. A naive "must already own the target" rule breaks both.
- **Fleet endpoints passing `None`** (e.g. `postDriverFleetDriverUpdate`,
  `postDriverUpdateFleetOwnerInfo`) get no actor check at all.
- **`ActorFleet` is overloaded** — some call sites pass an operator id, so the name is misleading
  and an ownership check will resolve it against `fleet_driver_association.fleet_owner_id`.
- **Silent passes** — a missing entity or `FleetOwnerInformation` row skips its check rather than
  failing it.
- **Non-unified cities** get only stages 1–4.
