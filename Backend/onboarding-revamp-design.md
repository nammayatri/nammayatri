# Onboarding Revamp — Design (Part 1: Schema + statusHandler)

Everything is gated per city by `TransporterConfig.enableBotFlow`. Flag off ⇒ existing/legacy flow, untouched. `enableBotFlow` cities use `PREPAID_SUBSCRIPTION`.

> End-to-end flowcharts for all four entities (DCO driver, fleet driver, vehicle, fleet owner) live in **`onboarding-flows.md`**. This doc covers the schema + code changes and the rationale.

---

## Requirements

### Flow (driver)

```
LOGIN (phone + OTP, T&C consent)
  ▼
DRIVER PROFILE — selfie-first KYC
  Selfie → DL → Aadhaar → PAN → PAN-Aadhaar linkage → Local Residence Proof
  → PVC → Nominee → Banking? → NOC?
  Operations Hub selection (after DL+Aadhaar+PAN verified-w/-selfie + other
  pre-inspection mandatory docs) → DRIVER inspection request created
  ▼
VEHICLE PROFILE (optional to start)
  RC (eligibility → auto-verified) → Vehicle photos
  Vehicle Operations Hub (after mandatory veh docs) → VEHICLE inspection request created
  ▼
OPS-HUB INSPECTION APPROVED (driver + vehicle, by ops partner)
  • OperationHubRequest → APPROVED ⇒ DriverInspectionHub / InspectionHub doc reads VALID
    (derived from request status; ops-approve writes NO flags — NOT verified here)
  • verified stays FALSE: TrainingForm + OperatorCode are still mandatory & not yet VALID
  • unlocks TRAINING; CRC triggers (owned elsewhere)
  ▼
TRAINING → videos → MCQ (Plasma) → trainingComplete ⇒ TrainingForm doc VALID
  ▼
OPERATOR CODE entry (DCO only; fleet drivers skip) ⇒ OperatorCode doc VALID
  ▼
[ statusHandler: driver.verified = true ]   ← NOW all isMandatory docs VALID
                                              (inspection + training + operator-code + the rest)
  ▼
BOT CHECK #1 (pre-subscription, external) → review docs → driver.approved = true
  ▼
SUBSCRIPTION PURCHASE (PREPAID_SUBSCRIPTION) ⇒ driver.subscribed = true   (no backend gate)
  ▼
BOT CHECK #2 (post-subscription, external) → uploads MSDS + Medical (auto-VALID) → triggers statusHandler
  ▼
[ statusHandler: driver.enabled = true ]
  = all isMandatory VALID  AND  all isMandatoryForEnabling VALID (incl. MSDS+Medical)  AND  approved == Just True
  ▼
GO ONLINE (enabled AND subscribed AND active vehicle)
```

### Flow (vehicle / RC)

```
RC entered → eligibility checks → auto-verified ;  Vehicle photos ;  Vehicle Ops Hub → inspection request
  ▼
OPS-HUB INSPECTION APPROVED ⇒ InspectionHub doc VALID  (writes NO flags — NOT verified here)
  ▼
[ statusHandler: vehicle.verified = true ]   ← only once ALL isMandatory vehicle docs are VALID
                                              (InspectionHub is one of them, not the sole trigger)
  ▼
BOT review → vehicle.approved = true   (BOT-owned; vehicle has no `enabled` flag / no statusHandler enable)
```

### Flow (fleet) — no inspection, no driver-style operator-code entry

```
FLEET PROFILE (Business / Individual) → docs (Aadhaar, PAN, GST/Business-License, etc.)
  ▼
[ statusHandler: fleet.verified = true ]   ← all isMandatory fleet docs VALID (excludes OperatorCode)
  ▼
BOT inputs the fleet's OperatorCode (FleetOwnerDVC row, isMandatoryForEnabling=true, per role) ⇒ OperatorCode doc VALID
  ▼
[ statusHandler: fleet.enabled = true ]
  = all isMandatoryForEnabling fleet docs VALID (incl. BOT-set OperatorCode)   — no `approved` conjunct
  ▼
FLEET OPERATIONAL — can add vehicles & drivers ;  fleet subscription ⇒ subscribed
```

> The BOT-controlled `OperatorCode` going VALID is itself the fleet enable gate (fleet has no inspection and no `approved` field).

### Flag rules

An entity (**driver / vehicle / fleet**) is verified/enabled purely from `DocumentVerificationConfig` validity:

```
verified = all isMandatory docs VALID                                              (ForVerified)
enabled  = all isMandatory docs VALID
           AND all isMandatoryForEnabling docs VALID                               (ForEnabling)
           AND approved == Just True
```

- The two conjuncts are written explicitly (not relying on `ForEnabling ⊇ ForVerified`), so the rule self-documents.
- **MSDS + Medical** are `isMandatory = false`, `isMandatoryForEnabling = true`: excluded from `verified`, required for `enabled`. **MSDS = "Maruti Suzuki Driving School Certificate" → reuse the existing `DrivingSchoolCertificate` doc-type** (no new MSDS type needed); Medical → `MedicalCertificate`.
- `approved` is set by BOT #1; the enabling docs (incl. MSDS/Medical) go VALID at BOT #2.

**Inspection feeds `verified` via config, not special-casing.** `InspectionHub` / `DriverInspectionHub` are ordinary vehicle/driver docs; their VALID status is derived from the ops-inspection `OperationHubRequest` (`APPROVED → VALID`). So they only gate `verified` when seeded `isMandatory = true`.

> ⚠️ **Seeding requirement (BOT cities):** `InspectionHub` and `DriverInspectionHub` `DocumentVerificationConfig` rows **must have `isMandatory = true`** for the relevant city/vehicle-category. The check `checkAllVehicleMandatoryDocsValid` iterates these docs (they’re in `defaultVehicleDocumentTypes`), but a config with `isMandatory = false` (or no config) is treated as non-blocking (`Nothing → True`), so `verified` would flip **without** waiting for inspection approval.

### DCO vs fleet driver (generic via `applicableTo`)

Both are `role = DRIVER` and share the same driver `DocumentVerificationConfig` set. The fleet-vs-individual distinction is expressed **generically** through each doc’s `applicableTo` (`FLEET | INDIVIDUAL | FLEET_AND_INDIVIDUAL`):

```
mandatoryForDriver = isMandatory(mode) AND ( applicableTo == FLEET_AND_INDIVIDUAL
                                             OR applicableToMatches isFleetDriver )

applicableToMatches isFleetDriver = case applicableTo of
  FLEET_AND_INDIVIDUAL -> True          -- applies to everyone (no fleet/individual check)
  FLEET                -> isFleetDriver
  INDIVIDUAL           -> not isFleetDriver
```

- `isFleetDriver`: `QFDA.findByDriverId driverId True` is `Just` (active fleet association).
- `OperatorCode` seeded `applicableTo = INDIVIDUAL` ⇒ mandatory for DCO, auto-excluded for fleet drivers. Generalises to any doc.
- Fleet driver **still creates inspection requests** (driver + vehicle).

### Two BOT checks (external API; not ours)

| Check | When | Sets | Effect on `enabled` |
|---|---|---|---|
| **#1** | pre-subscription | `approved = true` | not yet — MSDS/Medical missing |
| **#2** | post-subscription | uploads MSDS + Medical (→ VALID), then triggers statusHandler | now `enabled = true` |

- Upload-alone never enables (`approved` gate at #1). Approve-alone never enables (MSDS/Medical must be VALID at #2). Both ⇒ enabled.

### Who sets what (under `enableBotFlow`)

| Entity | `verified` | `approved` | `enabled` |
|---|---|---|---|
| **Driver** | statusHandler (isMandatory VALID) | BOT #1 | statusHandler (isMandatoryForEnabling VALID && approved) |
| **Vehicle (RC)** | statusHandler (isMandatory VALID) | BOT | — (no enabled flag) |
| **Fleet** | statusHandler (isMandatory VALID) | — (no field) | statusHandler (all isMandatoryForEnabling VALID; no approved conjunct) |

**Fleet operator code.** Fleet has no inspection and no `approved` flag. Its `OperatorCode` lives in `FleetOwnerDocumentVerificationConfig` (keyed by `role`; no `applicableTo` column), seeded `isMandatory = false`, `isMandatoryForEnabling = true` — excluded from fleet `verified`, required for fleet `enabled`. **Only the BOT inputs the fleet’s operator code** (making `OperatorCode` VALID); that BOT-controlled doc becoming VALID is itself the gate, so fleet enable needs no separate `approved`. statusHandler then writes `fleet.enabled` **directly** (no cascade to drivers under `enableBotFlow` — see §4). The BOT operator-code-input handler is external (other dev).

---

## 1. Schema (YAML)

| Field | Spec | Why |
|---|---|---|
| `TransporterConfig.enableBotFlow :: Maybe Bool` | `Merchant.yaml` | per-city switch; NULL ⇒ legacy flow |
| `DriverInformation.firstVerifiedAt :: Maybe UTCTime` | `DriverInformation.yaml` | stamp first verification (audit); backfill separately |
| `VehicleRegistrationCertificate.verified :: Maybe Bool` | `DriverOnboarding.yaml` | ops/onboarding verified, distinct from BOT `approved` |
| `FleetOwnerDocumentVerificationConfig.isMandatoryForEnabling :: Maybe Bool` | `DriverOnboarding.yaml` | fleet config now mirrors driver: enable-only docs (fleet `OperatorCode`) are `isMandatory=false, isMandatoryForEnabling=true` |
| `DocumentType` += `OperatorCode, MedicalCertificate, Rating` | `DriverOnboarding.yaml` | model operator-code/medical/rating as DVC docs |
| `AddressDocumentType` += `VoterId, LifeInsurancePolicy, Others` | `DriverInformation.yaml` | extend local-residence-proof types |

## 2. Setters (verified writers)

All write `verified` in isolation (without touching `enabled`), so statusHandler can compute the two flags independently.

| Setter | File | Signature | Notes |
|---|---|---|---|
| `updateVerified` | `Storage/Queries/DriverInformationExtra.hs` | `Bool -> Id Driver -> m ()` | sets driver `verified` (both directions); on transition to verified, stamps `firstVerifiedAt` once via `stampFirstVerifiedAtIfNull` |
| `stampFirstVerifiedAtIfNull` | `Storage/Queries/DriverInformationExtra.hs` | `Id Driver -> m ()` | atomic `UPDATE … WHERE first_verified_at IS NULL` — race-free "stamp once" (no read-then-write) |
| `updateVerifiedByCertificateNumberHash` | `Storage/Queries/VehicleRegistrationCertificateExtra.hs` | `Maybe Bool -> DbHash -> m ()` | sets RC `verified` by reg-no hash (statusHandler holds the reg-no, not the RC id); mirrors `updateDocsVerificationStatusByCertificateNumberHash` |
| `updateFleetOwnerVerifiedStatus` / `updateFleetOwnerEnabledStatus` | generated (`FleetOwnerInformation`) | `Bool -> Id Person -> m ()` | set fleet `verified` / `enabled` in isolation |

`DriverInformationExtra.updateVerified` body — sets the flag, then stamps `firstVerifiedAt` atomically:
```haskell
updateVerified isVerified driverId = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDI.verified isVerified, Se.Set BeamDI.updatedAt now]
                  [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  when isVerified $ stampFirstVerifiedAtIfNull driverId   -- atomic: ... WHERE first_verified_at IS NULL
```

## 3. Doc-validity predicate split (`Status.hs`)

`verified` (isMandatory) and `enabled` (isMandatoryForEnabling) need different mandatory sets from the **same** doc-validity check, so it is parameterised by a mode:

```haskell
data MandatoryMode = ForVerified | ForEnabling

isDocRequiredFor :: MandatoryMode -> DocumentVerificationConfig -> Bool
isDocRequiredFor ForVerified config = config.isMandatory
isDocRequiredFor ForEnabling config = fromMaybe config.isMandatory config.isMandatoryForEnabling
```

- **Legacy names keep `ForEnabling`** (the original behaviour, `fromMaybe isMandatory isMandatoryForEnabling`):
  `checkIfDocumentValid`, `checkAllDriverDocsVerified`, `checkAllVehicleDocsVerified` are now thin wrappers over `…' ForEnabling`.
- **New `…ForVerified` variants** drive the `verified` computation:
  `checkAllDriverMandatoryDocsValid`, `checkAllVehicleMandatoryDocsValid` over `…' ForVerified`.
- The fleet (`Left`) branch of `checkIfDocumentValid'` also honours the mode now (fleet configs gained `isMandatoryForEnabling`): `ForVerified → isMandatory`, `ForEnabling → fromMaybe isMandatory isMandatoryForEnabling` (resolved inline, since `isDocRequiredFor` is typed for the driver config).
- **`applicableTo` match (driver):** `checkIfDocumentValid'` also takes `mbIsFleetDriver :: Maybe Bool`. A driver doc blocks only if `isDocRequiredFor mode config && docAppliesToDriver mbIsFleetDriver config.applicableTo`. `docAppliesToDriver`:
  - `Nothing` → legacy, no filtering (every doc applies) — legacy callers pass `Nothing`, unchanged.
  - `Just isFleet` → `FLEET_AND_INDIVIDUAL` applies to all; `FLEET` only to fleet drivers; `INDIVIDUAL` only to DCOs.
  Legacy wrappers `checkAllDriverDocsVerified[ForVerified]` pass `Nothing`. The BOT paths call `checkAllDriverDocsVerified' <mode> (Just isFleetDriver) …` directly, where `isFleetDriver = isJust <$> QFDA.findByDriverId person.id True` (computed in `recomputeDriverVerifiedAndEnabled` and in the ops-approve `checkAllDriverDocsVerifiedForDriver` under `enableBotFlow`). Vehicle checks pass `Nothing` (no DCO/fleet-driver axis).
- **Why:** one predicate, two modes + an applicableTo filter. `verified` excludes enabling-only docs (MSDS/Medical, fleet `OperatorCode`); `enabled` includes them; `OperatorCode = INDIVIDUAL` is skipped for fleet drivers. Legacy callers unchanged.

## 4. statusHandler computes `verified` + `enabled` generically (`Status.hs`)

Clean top-level split — `enableBotFlow`, `requiresOnboardingInspection`, `requiresDriverOnboardingInspection` always move together (same flow), so the BOT logic lives in one branch and the legacy logic (untouched) in the other:

```haskell
let enableBotFlow = transporterConfig.enableBotFlow == Just True
vehicleDocuments <-
  if enableBotFlow
    then do                                   -- BOT flow: purely doc-driven, ignores separateDriverVehicleEnablement
      when (isFleetRole role) $ recomputeFleetVerifiedAndEnabled …
      when (role == DRIVER)   $ recomputeDriverVerifiedAndEnabled …
      getVehicleDocuments … True …            -- builds status list + vehicle verified write
    else <legacy enablement, byte-identical to pre-BOT: the separate/combined branches>
```

The per-entity logic is factored into helpers (defined near `enableDriver`):

### `recomputeDriverVerifiedAndEnabled` (DCO + fleet driver, `role = DRIVER`)
```haskell
isFleetDriver <- isJust <$> QFDA.findByDriverId person.id True
allMandatoryDocsValid = checkAllDriverDocsVerified' ForVerified (Just isFleetDriver) …  -- isMandatory
allEnablingDocsValid  = checkAllDriverDocsVerified' ForEnabling (Just isFleetDriver) …  -- isMandatoryForEnabling
-- verified BOTH ways
when (allMandatoryDocsValid /= driverInfo.verified) $ updateVerified allMandatoryDocsValid driverId
let shouldEnable = allMandatoryDocsValid && allEnablingDocsValid && driverInfo.approved == Just True
-- enabled BOTH ways
if shouldEnable && not driverInfo.enabled
  then enableDriver … allMandatoryDocsValid                       -- enable (alerts/SMS/LTS)
  else when (not shouldEnable && driverInfo.enabled) $
         SDO.disableDriverWithAnalytics merchantOpCityId person.id Nothing   -- disable
```

- **Bidirectional**: both `verified` and `enabled` are written to whatever the docs currently imply (`/=` guard), so a later doc rejection/expiry retracts them on the next poll.
- **Explicit conjuncts** — `isMandatory VALID` **and** `isMandatoryForEnabling VALID` **and** `approved == Just True` — not relying on `ForEnabling ⊇ ForVerified`.
- MSDS/Medical (`isMandatory=false, isMandatoryForEnabling=true`) gate `enabled` but not `verified`.
- Enable routes through `enableDriver` (alerts/SMS/LTS); disable through `disableDriverWithAnalytics` (analytics + LTS sync).

### Vehicle (`getVehicleDocuments`)
```haskell
when enableBotFlow $ do
  let allVehicleMandatoryDocsValid = checkAllVehicleMandatoryDocsValid …   -- ForVerified
  rcHash <- getDbHash vehicleDoc.registrationNo
  RCQuery.updateVerifiedByCertificateNumberHash (Just allVehicleMandatoryDocsValid) rcHash  -- both ways
-- RC auto-activation + nested enable are suppressed when enableBotFlow (guarded by `not enableBotFlow`)
```
Writes `VRC.verified` both ways (`Just True` / `Just False`). Vehicle has no `enabled` flag; `approved` is BOT-owned. Vehicle docs ignore `applicableTo` (predicate passes `Nothing`).

### Fleet owner (`recomputeFleetVerifiedAndEnabled`)
Runs only for fleet **owners** (`isFleetRole role`). Fleet **drivers** have `role = DRIVER` and go through `recomputeDriverVerifiedAndEnabled` instead.
```haskell
allFleetMandatoryDocsValid = checkAllDriverMandatoryDocsValid …  -- isMandatory (excludes OperatorCode)
allFleetEnablingDocsValid  = checkAllDriverDocsVerified …            -- isMandatoryForEnabling (incl. OperatorCode)
-- both flags written DIRECTLY, both ways, NO cascade (fleet enable/disable is decoupled from its drivers)
when (allFleetMandatoryDocsValid /= foi.verified) $ updateFleetOwnerVerifiedStatus allFleetMandatoryDocsValid personId
when (allFleetEnablingDocsValid  /= foi.enabled)  $ updateFleetOwnerEnabledStatus  allFleetEnablingDocsValid  personId
```
- No `approved` conjunct for fleet — the BOT-controlled `OperatorCode` (isMandatoryForEnabling) going VALID is the gate.
- **Decoupled from drivers under `enableBotFlow`:** the fleet owner's enable/disable does **not** cascade to its drivers (no `enableDriver`, no `cascadeFleet*ToDrivers`). Each fleet driver's enablement is computed independently by `recomputeDriverVerifiedAndEnabled` (own docs + own `approved`). Legacy flow still cascades via `enableDriver`.

### Doc-fetch consistency (fleet)
`getDriverDocTypes` (fleet `Left` branch) filters the mandatory fetch by `fromMaybe isMandatory isMandatoryForEnabling` (matching the driver side) — so enabling-only fleet docs (`OperatorCode`) are fetched and seen by the fleet `enabled` computation. (Pre-existing fleet-role-drift fallback from an earlier merged fix remains; the target cities seed configs for both `FLEET_OWNER` and `FLEET_BUSINESS`, so it does not fire.)

**Why:** statusHandler is the single, generic computer of `verified` and `enabled` for driver **and** fleet owner. BOT only flips driver `approved` / makes fleet `OperatorCode` VALID / sets vehicle `approved`; the next statusHandler run derives the rest. (Vehicle has no `enabled`; fleet drivers go through the driver path.)

## 5. `enableDriver` gains a `verifiedToSet` arg (`Status.hs`)

```haskell
enableDriver :: Id MerchantOperatingCity -> Id Person -> Role -> Maybe Text -> TransporterConfig -> Id Merchant -> Bool -> Flow ()
--                                                                                                            ^^^^ verifiedToSet
```

- Threads `verifiedToSet` into `enableAndTriggerOnboardingAlertsAndMessages … verifiedToSet` (driver) and `updateFleetOwnerEnabledAndVerifiedStatus True verifiedToSet` (fleet branch, which still cascades — used by **legacy** only).
- **All legacy call sites pass `True`** (enable implies verified — unchanged). The BOT **driver** path passes `allMandatoryDocsValid`. The BOT **fleet** path does **not** call `enableDriver` at all — it writes `fleet.enabled` directly (no cascade; see §4).
- **Why:** driver enable fires onboarding alerts / SMS / LTS pool sync / analytics — must be preserved; and the enableBotFlow path must not clobber the independently-computed `verified`.

## 6. Ops inspection approval marks the inspection doc VALID only (`Operator/Driver.hs`)

`handleVehicleInspectionApproval` / `handleDriverInspectionApproval` (in `postDriverOperatorRespondHubRequest`) are now flag-aware:

- Under `enableBotFlow`: ops-approve writes **no flags**. It only sets the `OperationHubRequest` to APPROVED (via `updateStatusWithDetails`) — which makes `InspectionHub` / `DriverInspectionHub` read VALID through the request-status → status mapping — plus bookkeeping (reminders cancel, completion record, analytics). `approved`, RC activation, `verified`, `enabled` are all derived by statusHandler.
- Legacy (`enableBotFlow` off): unchanged — `QVRC.updateApproved` / `QDIExtra.updateApproved` + `activateRCAutomatically` / `postDriverEnable` still fire.
- **Why:** statusHandler is the single source of truth in the BOT flow; ops-approve's only job is to make the inspection doc VALID (one input to the doc-driven verified/enabled computation).

## 7. Bidirectional writes + race-free firstVerifiedAt

- **Bidirectional** (all three recompute paths, see §4): flags are written to match current doc validity via a `x /= current` guard, so a BOT doc-rejection (doc → INVALID) retracts `verified`/`enabled` on the next poll. Driver disable → `disableDriverWithAnalytics`; fleet disable → `updateFleetOwnerEnabledStatus False` (no cascade under `enableBotFlow`); vehicle → `updateVerifiedByCertificateNumberHash (Just False)`.
- **Race-free stamp**: `updateVerified` stamps `firstVerifiedAt` via `stampFirstVerifiedAtIfNull` — atomic `UPDATE … WHERE first_verified_at IS NULL`.

---

## 8. Driver `OperatorCode` status derivation (`Status.hs`)

`OperatorCode` has no stored doc/image — its status is **derived** from the active driver↔operator
association (which `postOperatorConsent` activates), the same pattern as the inspection hubs:

```haskell
getOperatorCodeStatusForDriver driverId = do
  mbAssoc <- QDOA.findByDriverId driverId True   -- active driver-operator association
  pure $ VALID <$ mbAssoc                         -- active ⇒ VALID; none ⇒ NO_DOC_AVAILABLE
```

- Cases added in both driver status functions (`getProcessedDriverDocuments`, `getInProgressDriverDocuments`).
  Previously `OperatorCode` fell to the default `NO_DOC_AVAILABLE` — it could never become VALID, so a DCO
  could never verify/enable. Now: operator consent → association active → next status poll reads VALID.
- No write/trigger needed at consent time (statusHandler picks it up on the next poll).
- Fleet drivers unaffected (`applicableTo=INDIVIDUAL` excludes the doc); the fleet owner's `OperatorCode`
  (FleetOwnerDVC, BOT-input) is a separate path.

## 9. Dependency-doc gate at ops-approve (`Operator/Driver.hs`, `Status.hs`)

The operator can approve an inspection request only if **all `dependencyDocumentType` docs of the
inspection-hub config are VALID** — otherwise the approval **throws, naming the invalid docs**:
`InvalidRequest "Cannot approve inspection; dependency documents not valid: [VehicleInsurance,…]"`.

- Gated under `enableBotFlow` in both `handleVehicleInspectionApproval` (deps of `InspectionHub`) and
  `handleDriverInspectionApproval` (deps of `DriverInspectionHub`). Legacy cities keep the existing
  silent-PENDING behaviour.
- Per-doc statuses come from new helpers `fetchVehicleDocStatusesForRC` / `fetchDriverDocStatusesForPerson`
  (extracted from the existing Bool checks, which now call them with `onlyMandatoryDocs = Just True` —
  behaviour-preserving). The dep gate fetches with `Nothing` (full doc list) because deps like
  Insurance/Fitness/PUC are excluded from the mandatory-only fetch in this city's configs.
- Config-driven: checks whatever `dependencyDocumentType` is seeded; missing doc counts as invalid.

## 10. Go-online gates (`setActivity`, `UI/Driver.hs`)

After the existing `enabled` + `subscribed` checks, under `enableBotFlow`:

- **Active vehicle** (all drivers): `isJust mbVehicle`, else `"Cannot go online: no active vehicle linked"`.
- **Fleet driver only** (active fleet association via `QFDA.findByDriverId`):
  - fleet owner `enabled` (`QFOI.findByPrimaryKey`), else `"Cannot go online: fleet is not enabled"`;
  - active fleet `PREPAID_SUBSCRIPTION` purchase via `QSPE.findLatestActiveByOwnerAndServiceName`
    (no-op expiry handler — read-only gate), else `"Cannot go online: fleet subscription is not active"`.
- **Why the purchase lookup, not `fleetOwnerInfo.subscribed`:** that flag is never written anywhere
  (created `Nothing`, untouched by payment/webhook flows) — a fleet's real subscription state lives in
  the `SubscriptionPurchase` table (activated on payment, ended by the `ExpireSubscriptionPurchase` job).
- Net: DCO go-online = `enabled && subscribed && active vehicle`; fleet driver additionally requires
  `fleet.enabled && fleet subscription active` — complements the no-cascade decoupling (§4).

---

## Pending (later parts)

- **BOT API** (both checks): owned by another dev.

### Done (was pending)
- **Operator-code entry** → §8. **Dependency-doc validation** → §9 (ops-approve gate, per clarified requirement). **Go-online gate** → §10.
- **Seeding** — shipped in `dev/feature-migrations/0014-bot-flow-onboarding-doc-config-seed.sql`: mandatory driver docs `applicableTo=FLEET_AND_INDIVIDUAL`; MSDS (`DrivingSchoolCertificate`, re-enabled) + Medical (`MedicalCertificate`) enable-only; `InspectionHub`/`DriverInspectionHub` `isMandatory=true`; **driver** `OperatorCode` `applicableTo=INDIVIDUAL`; **fleet** `OperatorCode` in `FleetOwnerDocumentVerificationConfig` for `FLEET_OWNER` + `FLEET_BUSINESS`.

## Backward compatibility

- All behaviour changes are inside `enableBotFlow` branches; flag off ⇒ identical to before.
- Predicate split: legacy names resolve to the original `fromMaybe isMandatory isMandatoryForEnabling` logic.
- `enableDriver`’s new arg is `True` for every legacy caller.
- Enum widenings are append-only; no migration backfill (except `firstVerifiedAt` for already-verified drivers — separate).
