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

`guardOnboardingAction` runs seven stages in order. **Only stage 7 is gated** on
`transporterConfig.unifiedOnboardingFlagsRecompute == Just True` ("unified cities"); stages 1–6 run
everywhere.

| # | Stage | Applies to | Actor-aware? | Unified-gated? |
|---|---|---|---|---|
| 1 | `guardFleetMembership` | `LinkToFleet`, `ChangeFleetOwner` — `TargetDriver` only | no | no |
| 2 | `guardFleetVehicleRelations` | `Link`, `Unlink` — vehicle targets, fleet-bearing actors | **yes** | no |
| 3 | `guardActorScope` | `View` only | **yes** | no |
| 4 | `guardNoLiveRide` | `Unlink`, `Deactivate`, `Delete`, `Disable`, `Expire` | no | no |
| 5 | `guardAssociationAllowed` | `Link`, `Add`, `LinkToFleet` — `TargetDriver` only | no | no |
| 6 | `guardRcAssociationAllowed` | `Link`, `Add`, `Activate`, `LinkToFleet` — vehicle targets | no | no |
| 7 | `guardActorAllowed` + `checkPrecondition` | all verbs **except** `ChangeFleetOwner` | `guardActorAllowed` only | **yes** |

| Consequence | Detail |
|---|---|
| Non-unified cities run stages 1–6 only | no precondition tables, no actor flag checks. Stages 1–3 sit outside the gate deliberately — they are authorization rules, not derived-flag preconditions, so they must hold everywhere |
| Stages 4–6 bind Admin too | no admin override for the live-ride guard or the association guards |

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

### Own-flag checks (`guardActorAllowed`, stage 7)

Checks the actor's **own** flags, never ownership. Unified cities only.

| Actor | Checked on | Must be | Codes |
|---|---|---|---|
| `ActorFleet` | every verb except `ChangeFleetOwner` | enabled, not blocked, not disabled | `ACTOR-1/2/3` |
| `ActorDriver` | `Link`, `Activate`, `Add` only (`driverActorGoverned`) | enabled, not blocked, not disabled | `ACTOR-1/2/3` |
| `ActorFleetAndDriver` | fleet half always; driver half on those three verbs | both halves as above | `ACTOR-1/2/3` |
| `None` | never | — | — |

Teardown and onboarding verbs stay available to a disabled `ActorDriver` on purpose: otherwise a
disabled driver could never be cleaned up, nor onboarded, since they are disabled until their
documents land. A missing `FleetOwnerInformation` / `DriverInformation` row means the check is
**skipped, not failed** (`whenJust`).

### Actor × Target matrix

Two kinds of check exist. **●** = the actor's own flags (stage 7). **◆** = the actor must prove a
relationship to the target (stages 2 and 3). **—** = nothing applies.

| Actor | `TargetDriver` | `TargetVehicle` / `TargetVehicleById` | `TargetFleetOwner` |
|---|---|---|---|
| `None` | — | — | — |
| `ActorDriver` | ● `Link` `Activate` `Add` | ● `Link` `Activate` `Add` | — |
| `ActorFleet` | ● all verbs<br>◆ `View` | ● all verbs<br>◆ `View` `Unlink` | — |
| `ActorFleetAndDriver` | ● both halves<br>◆ `View` | ● both halves<br>◆ `View` `Link` `Unlink` | — |

`TargetFleetOwner` is never actor-governed (`isActorGovernedTarget = False`), so an acting fleet's
own flags never gate a fleet-owner-targeted action. Its only ◆ is `View`, where a fleet may read
only itself.

Every ◆ in full. Stage 3 (`guardActorScope`) covers reads; stage 2
(`guardFleetVehicleRelations`) covers vehicle link/unlink.

| Stage | Verb | Target | Actor must prove | Query | Error |
|---|---|---|---|---|---|
| 3 | `View` | Driver | active FDA with this fleet | `QFDA.findByDriverIdAndFleetOwnerIdWithStatus` | `DRIVER_NOT_PART_OF_FLEET` |
| 3 | `View` | Vehicle | `rc.fleetOwnerId` = this fleet | `RCQuery.findLastVehicleRCFleet'` | `VEHICLE_NOT_PART_OF_FLEET` |
| 3 | `View` | VehicleById | same, by RC id | `RCQuery.findById` | `VEHICLE_NOT_PART_OF_FLEET` |
| 3 | `View` | FleetOwner | target is the actor itself | — | `InvalidFleetOwner` |
| 2 | `Link`, `Unlink` | Vehicle / ById | named driver has an FDA with this fleet, `isActive = True`, within `associatedTill` | `QFDA.findByDriverIdAndFleetOwnerId … True` | `DRIVER_NOT_PART_OF_FLEET` |
| 2 | `Unlink` | Vehicle / ById | RC has a `FleetRCAssociation` with this fleet within `associatedTill` | `FRCA.findLinkedByRCIdAndFleetOwnerId` | `VEHICLE_NOT_PART_OF_FLEET` |

| Note | |
|---|---|
| Stage 2 purpose | stops a fleet attaching an RC to a driver that is not its own, and stops it detaching an RC it does not hold |
| `TargetVehicle` resolution | registration number → RC id via `RCQuery.findLastVehicleRCWrapper`; an unresolvable number fails the RC check |
| Stages 2 and 3 disagree by design | stage 3 reads `rc.fleetOwnerId`; stage 2 reads the `FleetRCAssociation` table |
| Who skips stage 3 entirely | `None` and `ActorDriver` — that is how Admin gets unrestricted read access |

---

## Preconditions by target

Skipped entirely outside unified cities, and skipped when the entity row is missing.

### Driver (`checkDriver`)

| Verb | Requires | Code |
|---|---|---|
| `Enable` | is admin-disabled, **not** `FleetDisabled`, **and** verified + approved + enabled | `DI-3`, `DI-6`, `DI-2` |
| `Disable` | not already disabled, **and** verified + approved + enabled | `DI-3`, `DI-2` |
| `Link` | enabled, not blocked, approved | `DI-1`, `D-BLOCKED` |
| `Activate` | enabled, not blocked | `DI-1`, `D-BLOCKED` |
| `Delete` | `disabledReasonFlag` is set — disable first. Uses the flag rather than `enabled`, because in unified cities an admin disable sets only the flag and leaves `enabled` derived | `D-DELETE` |
| `LinkToFleet` | **not** enabled — use `changeFleetOwner` to move an active driver | `DI-9` |
| `LinkToFleet` (stage 1) | rejected only if the driver has an FDA with `isActive = True` and `associatedTill > now` **and** is enabled — a disabled driver may still be linked. No `onboardingAs` requirement: an `INDIVIDUAL` driver can be added to a fleet | `DRIVER_ALREADY_LINKED_WITH_FLEET` |
| `SetOnboardingAs` | **not** enabled | `DI-8` |
| `ChangeFleetOwner` (stage 1) | `onboardingAs = FLEET_DRIVER`, and at least one FDA with `associatedTill > now` (`QFDA.findAllByDriverIdWithStatus`). The new owner is not compared against the current one, so re-transferring into the same fleet is a no-op rather than an error | `DRIVER_NOT_FLEET_DRIVER`, `DRIVER_HAS_NO_ACTIVE_FLEET_ASSOCIATION` |
| `UnlinkDocument` | **not** enabled — invalidate the doc before unlinking documents | `DI-10` |
| `Block` | not already blocked, no `blockReasonFlag`, **and** verified + approved + enabled | `D-BLOCKED`, `DI-2` |
| `Unblock` | currently blocked, has a `blockReasonFlag`, **and** verified + approved + enabled | `D-BLOCKED`, `DI-2` |
| `Unlink`, `Deactivate`, `Add`, `Approve`, `Reject`, `View`, `Expire` | always pass | — |

The rows marked **(stage 1)** live in `guardFleetMembership`, not `checkDriver`:

| | stage 1 rows | rest of this section |
|---|---|---|
| Runs in | every city | unified cities only |
| Bypassed by `merchant.overwriteAssociation` | no | `guardAssociationAllowed` is |
| Source | `guardFleetMembership` | `checkPrecondition` |

The two verbs are mirror images — `LinkToFleet` requires the driver to be free, `ChangeFleetOwner`
requires them to be attached and to already be a `FLEET_DRIVER` — and for `ChangeFleetOwner` they are
the only checks that run at all.

| Design point | Why |
|---|---|
| `LinkToFleet` counts only `isActive = True` rows | keeps consent flows working: `postFleetConsent` fetches its row with `FDV.findByDriverId driverId False`, so the pending association it is about to activate is invisible to the check |
| `LinkToFleet` skipped for a disabled driver | an unenabled driver can be re-linked without first tearing down a stale association |
| `LinkToFleet` does not require `onboardingAs = FLEET_DRIVER` | a driver who signed up as `INDIVIDUAL` must still be addable to a fleet; `onboardingAs` is set by the link flow itself, so requiring it beforehand made first-time adds impossible |
| `ChangeFleetOwner` does not reuse `Link` | `Link` pulls in `guardAssociationAllowed`, which rejects a driver who already has an active fleet association, plus the `Link` preconditions — neither holds for a driver being moved between fleets |
| Destination validation stays in the handler | new owner must hold a fleet role (`DCommon.checkFleetOwnerRole`) and, when `merchant.fleetOwnerEnabledCheck` is on, be enabled (`DCommon.checkFleetOwnerVerification`) |

### Vehicle (`checkVehicle`)

| Verb | Requires | Code |
|---|---|---|
| `Link` | `verificationStatus = VALID`, verified, approved | `RI-2`, `RI-1` |
| `Activate` | `VALID`, approved | `RI-2`, `RI-1` |
| `Enable`, `Disable`, `Block`, `Unblock`, `SetOnboardingAs`, `LinkToFleet`, `ChangeFleetOwner`, `UnlinkDocument` | **always rejected** — vehicles have no such flags | `R-UNSUPPORTED` |
| `Unlink`, `Deactivate`, `Add`, `Delete`, `Approve`, `Reject`, `View`, `Expire` | always pass | — |

### Fleet owner (`checkFleet`)

| Verb | Requires | Code |
|---|---|---|
| `Enable` | is admin-disabled, **and** verified + approved + enabled | `FI-1`, `FI-2` |
| `Disable` | not already disabled, **and** verified + approved + enabled | `FI-1`, `FI-2` |
| `Block`, `Unblock` | **always rejected** — use disable/enable | `FI-3` |
| `SetOnboardingAs`, `ChangeFleetOwner` | **always rejected** | `F-UNSUPPORTED` |
| everything else | always pass | — |

---

## Admin vs Fleet — the short version

| Aspect | Admin (`None`) | Fleet (`ActorFleet…`) |
|---|---|---|
| Read (`View`) | any driver / vehicle / fleet | **only its own** |
| Preconditions on mutations | full tables | identical |
| Own-flag check | none | must be enabled / not blocked / not disabled (unified cities only) |
| Ownership on `Link` / `Unlink` of a vehicle | n/a | **checked** — stage 2 |
| Ownership on every other mutation | n/a | **not checked** |
| Live-ride guard | applies | applies |
| Association guards | apply | apply |

---

## Call-site map

Every `SGuard` call site in the codebase, grouped by verb. Routes are relative to the module base
(`"driver" :> …`) under `/provider/{merchantShortId}/{city}/`.

Not every entry is an endpoint. Two markers are used:

- **[job]** — scheduler job, no route: `disableDriverForMandatoryReminder`,
  `processDocumentExpiryReminder`, `processReminderByType`
- **[helper]** — called by an endpoint rather than being one: `unlinkVehicleFromDriver`,
  `createFleetOwnerDetails`

Three sites compute their target at runtime, so one handler can land in more than one bucket —
see the target column for each.

| Verb | API | Endpoint | Does | Target | Actor |
|---|---|---|---|---|---|
| **`View`** | `getDriverInfo` | `GET /driver/info` | reads a driver profile by phone / id / wallet / vehicle | Driver, or Vehicle when searched by `vehicleNumber` | `ActorFleet` for fleet roles, else `None` |
| **`LinkToFleet`** | `postDriverFleetAddDrivers` | `POST /driver/fleet/addDrivers` | drivers a fleet adds in bulk | Driver | `ActorFleetAndDriver` |
| | `postDriverFleetVerifyJoiningOtp` | `POST /driver/fleet/verifyJoiningOtp` | driver joins a fleet once the OTP checks out | Driver | `ActorFleetAndDriver` |
| | `postDriverLinkToFleet` | `POST /driver/linkToFleet` (UI) | driver's own request to join a named fleet | Driver | `ActorFleetAndDriver` |
| | `postFleetConsent` | `POST /fleet/consent` (UI) | driver consents to a fleet-initiated request | Driver | `ActorFleetAndDriver` |
| **`ChangeFleetOwner`** | `postDriverFleetDriverChangeFleetOwner` | `POST /driver/fleet/driver/{driverId}/changeFleetOwner` | moves a driver between fleets | Driver | `ActorFleetAndDriver` |
| **`Link`** | `postDriverFleetLinkRCWithDriver` | `POST /driver/fleet/linkRCWithDriver` | associates a fleet vehicle with a fleet driver | VehicleById | `ActorFleetAndDriver` |
| | `postDriverAddVehicle` | `POST /driver/{driverId}/addVehicle` | associates a vehicle with a driver, or with a fleet | VehicleById | `ActorDriver` or `ActorFleet` |
| **`Unlink`** | `postDriverFleetRemoveDriver` | `POST /driver/{driverId}/fleet/remove/driver` | detaches a driver from a fleet | Driver | `ActorFleetAndDriver` |
| | `postDriverFleetRemoveVehicle` | `POST /driver/{vehicleNo}/fleet/remove/vehicle` | detaches a vehicle from a fleet | Vehicle | `ActorFleet` |
| | `postDriverUnlinkVehicle` | `POST /driver/{driverId}/unlinkVehicle` | detaches a driver from a vehicle | Driver | `None` |
| | `postDriverFleetUnlink` → `unlinkVehicleFromDriver` **[helper]** | `POST /driver/{driverId}/{vehicleNo}/fleet/unlink` | detaches a fleet driver from a fleet vehicle | Driver | `ActorDriver` / `ActorFleetAndDriver` |
| **`Activate` / `Deactivate`** | `postDriverFleetVehicleDriverRcStatus` | `POST /driver/{driverId}/fleet/vehicleDriverRCstatus` | toggles which RC a fleet driver is currently driving | **Driver** | computed (fleet / driver) |
| | `postDriverSetRCStatus` | `POST /driver/{driverId}/setRCStatus` | toggles which RC a driver is currently driving | **Driver** | `None` |
| **`Enable` / `Disable`** | `postDriverEnable` | `POST /driver/{driverId}/enable` | turns a driver back on after an admin disable | Driver | `None` |
| | `postDriverDisable` | `POST /driver/{driverId}/disable` | turns a driver off by admin decision | Driver | `None` |
| | `postAccountVerifyAccount` | `POST /account/verifyAccount` | turns a fleet owner on or off on account verification | FleetOwner | `None` |
| **`Block` / `Unblock`** | `postDriverBlock` | `POST /driver/{driverId}/block` | bars a driver from taking rides | Driver | `None` |
| | `postDriverBlockWithReason` | `POST /driver/{driverId}/blockWithReason` | bars a driver, recording a reason and duration | Driver | `None` |
| | `postDriverUnblock` | `POST /driver/{driverId}/unblock` | lifts a block | Driver | `None` |
| **`Expire`** | `disableDriverForMandatoryReminder` **[job]** | — | turns a driver off when a mandatory document expires | Driver | `None` |
| | `processDocumentExpiryReminder`, `processReminderByType` **[job]** | — | switches an RC off when a mandatory document expires | VehicleById | `None` |
| | `processDocumentExpiryReminder` **[job]** | — | invalidates the expired document itself | FleetOwner if `SDO.isFleetRole`, else Driver | `None` |
| **`UnlinkDocument`** | `postDriverRegistrationUnlinkDocument` | `POST /driver/{personId}/unlink/document/{documentType}` | deletes a driver-domain document, its images and its denormalised field | Driver or FleetOwner by role | `None` |
| **`Delete`** | `deleteDriverPermanentlyDelete` | `DELETE /driver/{driverId}/permanentlyDelete` | deletes a driver record permanently — driver must be disabled first | Driver | `None` |
| **`Approve` / `Reject`** | `postDriverFleetApproveDriver` | `POST /driver/fleet/approveDriver` | decides a fleet's join request for a driver — both verbs | Driver | `ActorFleetAndDriver` |
| | `postDriverUpdateVehicleVariant` | `POST /driver/updateVehicleVariant/{driverId}` | approves a corrected vehicle variant on the RC | Vehicle | `None` |
| | `postDriverUpdateFleetOwnerInfo` | `POST /driver/{driverId}/updateFleetOwnerInfo` | approves a change of fleet type / role | FleetOwner | `None` |
| | `validateImageHandler` | `POST /driver/register/validateImage` (UI) | records the verification result on an uploaded document image | VehicleById when `mbRcId` is set; else FleetOwner for `FLEET_OWNER` / `FLEET_BUSINESS`; else Driver | `None` |
| **`Add`** | `createFleetOwnerDetails` **[helper]** | — | creates the fleet owner person record | FleetOwner | `None` |
| | `fleetOwnerRegister` | — | registers a fleet owner, attaching GST / business-licence images | FleetOwner | `None` |
| **`SetOnboardingAs`** | `postDriverFleetDriverUpdate` | `POST /driver/fleet/driver/{driverId}/update` | sets `onboardingAs` from the dashboard | Driver | `None` |
| | `updateDriver` | — (UI, `API/UI/Driver.hs`) | sets `onboardingAs` from the driver app | Driver | `None` |

`getDriverInfo` is the only endpoint that currently gets an ownership check from the guard.

Routes shown as `—` are the jobs and helpers, plus `fleetOwnerRegister` and `updateDriver`, whose
paths are assembled inside hand-written servant alternations rather than a generated endpoint type —
they were not confirmed and are deliberately left blank rather than guessed.

### `Unlink` — the code has not caught up

Seven document / association endpoints still pass `Unlink` despite not being one of the four
operations above: `postDriverLinkToFleet`'s revoke path, `postDriverDeleteRC`,
`postDriverEndRCAssociation`, `postDriverUnlinkAadhaar`, `postDriverUnlinkDL`,
`postDriverDeleteAadhaar` and `postDriverDeletePanCard`. Re-verbing them would take those seven out
of `guardNoLiveRide`, which is why it has not been done as a side effect of a doc change.

### Releasing a document from a driver

`postDriverRegistrationUnlinkDocument` (`Management/DriverRegistration.hs`) is the single API
responsible for delinking a driver-domain document.

| | |
|---|---|
| Endpoint | `POST /driver/{personId}/unlink/document/{documentType}` (helper variant adds `?requestorId=`) |
| Document types | `DriverLicense`, `PanCard`, `AadhaarCard`, `GSTCertificate` |
| Does | clears the denormalised field (PAN / Aadhaar number, or the fleet owner's image ids), deletes the document row, deletes its images via `QImage.deleteByPersonIdAndImageType`, then recomputes onboarding flags |
| Shared logic | `SDO.unlinkDriverDocument` in `SharedLogic/DriverOnboarding.hs` — the same function the re-registration path calls |
| Requestor check | `isAssociationBetweenTwoPerson` when `requestorId` is given and that person exists at BPP, so a fleet owner may only unlink for a driver associated with them |
| Guard | `UnlinkDocument` / `TargetDriver` or `TargetFleetOwner` / `None` |

`postDriverUnlinkDL` (`POST /driver/{driverId}/unlinkDL`, `Management/Driver.hs`) still exists and
also deletes a DL, but only calls `QDriverLicense.deleteByDriverId` plus analytics — no image
cleanup, no flag recompute. Prefer the unlink-document API.

Both take the **driverId of the current holder**. A fleet owner holding only a licence number cannot
resolve it: `getDriverInfo` accepts `dlNumber`, but its fleet branch rejects that with
`FLEET_SEARCH_PARAM_NOT_SUPPORTED`. So releasing a document is admin-reachable end to end, and
fleet-reachable only when the holder's driverId is already known.

### `Activate` / `Deactivate` is the lightweight pair

`postDriverFleetVehicleDriverRcStatus` and `postDriverSetRCStatus` pick `Activate` or `Deactivate` at
runtime from the request flag, and despite their names both target **Driver**, not Vehicle. The jobs
only ever deactivate.

**This pair is deliberately lighter than `Link` / `Unlink`, and must stay separate from it.**
`Link` / `Unlink` create and tear down an association; `Activate` / `Deactivate` only flip the active
flag on an association that already exists. The guard encodes that in four places:

| | `Link` | `Activate` | applies to |
|---|---|---|---|
| Driver preconditions | enabled, not blocked, **and approved** | enabled, not blocked | `checkDriver` |
| Vehicle preconditions | `VALID`, **verified**, approved | `VALID`, approved | `checkVehicle` |
| `guardAssociationAllowed` | applies | does **not** apply | `TargetDriver` |
| `guardFleetVehicleRelations` | applies (`Link` / `Unlink`) | does **not** apply | vehicle targets |

The teardown halves are flatter: `Unlink` and `Deactivate` both pass every precondition table and
both sit in `guardNoLiveRide`.

Folding `Activate` into `Link` would break RC status toggling: `postDriverSetRCStatus` and
`postDriverFleetVehicleDriverRcStatus` both pass `TargetDriver`, so `guardAssociationAllowed` would
start rejecting every driver who already holds an active fleet association — exactly the drivers
whose RC status a fleet toggles.

`Deactivate` and `Unlink` are otherwise treated identically by every other stage, so
`guardFleetVehicleRelations` is the only thing that currently distinguishes them: a fleet must prove
it holds an RC before *detaching* it, but not before *deactivating* it.

---

## Adding a new guarded action

1. Pick the verb. If none fits, add a constructor to `ActionVerb` — `-Werror` will then force you to
   handle it in `checkDriver`, `checkVehicle` and `checkFleet`.
2. Decide whether it belongs in `guardNoLiveRide` / `guardAssociationAllowed` /
   `guardRcAssociationAllowed`. An unlisted verb no-ops in all three.
3. Pass a **real** actor. `None` means "no actor accountable"; use it for admin, schedulers, and the
   onboarding/registration stages only.
4. Use `withOnboardingAction` for mutations, `guardOnboardingAction` for reads.
