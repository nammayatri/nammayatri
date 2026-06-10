# Onboarding Flows — BOT Flow (`enableBotFlow = Just True`)

Complete end-to-end onboarding flows under the BOT-approval flow, gated per city by
`TransporterConfig.enableBotFlow`. When the flag is **off** (NULL) the legacy / Operations-Hub flow
applies **unchanged** — the Ops-Hub flow and the BOT flow **coexist** (a BOT city still runs ops
inspections; "BOT on" means "ops flow **plus** the doc-driven engine + BOT approval", not "ops replaced").

Four entities: **DCO driver**, **fleet driver**, **vehicle (RC)**, **fleet owner**.
Code-level design + rationale: `onboarding-revamp-design.md`.

---

## Core rule (all entities)

```
verified = ALL isMandatory docs VALID                       ← statusHandler (both directions)
enabled  = ALL isMandatory docs VALID
           AND ALL isMandatoryForEnabling docs VALID         ← statusHandler (both directions)
           AND approved == Just True                          ← BOT API sets approved (driver/vehicle)
```

- **statusHandler is the single source of truth** for `verified`/`enabled`. It writes them
  **bidirectionally** (`x /= current` guard) — a rejected/expired doc retracts them on the next poll.
- **Config source:** driver / fleet driver → `DocumentVerificationConfig`;
  fleet owner → `FleetOwnerDocumentVerificationConfig` (keyed by `role`).
- **Predicate:** `isDocRequiredFor mode config` — `ForVerified ⇒ isMandatory`,
  `ForEnabling ⇒ fromMaybe isMandatory isMandatoryForEnabling`. `docAppliesToDriver` filters by
  `applicableTo` + fleet-linkage.

### Who sets what

| Actor | Sets |
|---|---|
| **Driver** | KYC docs, training, operator code (DCO only), subscription |
| **Ops partner** (inspection approve) | inspection-hub doc → VALID (writes **no** flags under enableBotFlow) — can approve **only if** the inspection-hub config's `dependencyDocumentType` docs are all VALID |
| **BOT API** | driver `approved` (#1), vehicle `approved`, fleet `OperatorCode` input, MSDS + Medical upload (#2) |
| **statusHandler** | `verified` (driver/vehicle/fleet), `enabled` (driver/fleet) — bidirectional, from current doc validity |

### Doc-status sources (how each doc becomes VALID)

| Doc | Source |
|---|---|
| DL / RC / PAN / Aadhaar | own table + verification (Idfy/HyperVerge) |
| PVC / LocalResidenceProof / MSDS (`DrivingSchoolCertificate`) / Medical (`MedicalCertificate`) | Image row `verificationStatus` |
| `InspectionHub` / `DriverInspectionHub` | **derived** from the ops `OperationHubRequest` status (APPROVED ⇒ VALID) |
| `TrainingForm` | LMS / Plasma completion |
| `OperatorCode` (driver) | **derived** from the active driver↔operator association (`getOperatorCodeStatusForDriver`) |
| `OperatorCode` (fleet) | BOT inputs it (FleetOwnerDVC row) |

---

## 1. DCO DRIVER (`role = DRIVER`, no fleet association)

```
 LOGIN (phone + OTP, T&C)
   │
   ▼
 DRIVER PROFILE — upload KYC docs
   Selfie/ProfilePhoto → DriverLicense → AadhaarCard → PanCard
   → PAN-Aadhaar linkage → LocalResidenceProof → PoliceVerificationCertificate
   │
   ▼
 OPERATIONS HUB → DRIVER inspection request created (DriverInspectionHub)
   │
   ▼
 ┌───────────────────────────────────────────────────────────────────┐
 │ OPS-HUB INSPECTION APPROVED  (handleDriverInspectionApproval)      │
 │  • dependency gate: all DriverInspectionHub deps must be VALID,    │
 │    else throw "dependency documents not valid: […]"               │
 │  • OperationHubRequest → APPROVED ⇒ DriverInspectionHub doc VALID  │
 │  • writes NO flags (BOT owns approved; statusHandler owns enable)  │
 └───────────────────────────────────────────────────────────────────┘
   │
   ▼
 TRAINING → videos → MCQ (Plasma) ⇒ TrainingForm doc VALID
   │
   ▼
 OPERATOR LINK (consent) ⇒ active driver↔operator association
   ⇒ OperatorCode doc VALID   (derived; applicableTo = INDIVIDUAL)
   │
   ▼
 ╔═══════════════════════════════════════════════════════════════════╗
 ║ statusHandler · recomputeDriverVerifiedAndEnabled                 ║
 ║   checkAllDriverDocsValidForVerified (isMandatory) ⇒ verified = TRUE ║
 ║   (DL+Aadhaar+PAN+PVC+LocalRes+Inspection+Training+OperatorCode)   ║
 ╚═══════════════════════════════════════════════════════════════════╝
   │
   ▼
 ┌──────────────────────────────────────────────┐
 │ BOT CHECK #1 (external API) → approved = TRUE │
 └──────────────────────────────────────────────┘
   │
   ▼
 SUBSCRIPTION PURCHASE (PREPAID_SUBSCRIPTION) ⇒ subscribed = TRUE   (gate UI-handled)
   │
   ▼
 ┌───────────────────────────────────────────────────────────────────┐
 │ BOT CHECK #2 (external) → uploads MSDS + Medical (Image VALID)     │
 │   → triggers statusHandler                                         │
 └───────────────────────────────────────────────────────────────────┘
   │
   ▼
 ╔═══════════════════════════════════════════════════════════════════╗
 ║ statusHandler · recomputeDriverVerifiedAndEnabled                 ║
 ║   isMandatory VALID                                               ║
 ║   && isMandatoryForEnabling VALID (incl. MSDS + Medical)          ║
 ║   && approved == Just True                                        ║
 ║   ⇒ enabled = TRUE  (via enableDriver — alerts/SMS/LTS sync)      ║
 ╚═══════════════════════════════════════════════════════════════════╝
   │
   ▼
 GO ONLINE  (setActivity)
   requires: enabled && subscribed && active vehicle
             (+ existing legacy gates: plan selected, bank charges, not blocked)

 ── any mandatory doc later REJECTED/expired ──
    next statusHandler poll → verified = FALSE  (allMandatory /= verified)
                            → enabled  = FALSE  (disableDriverWithAnalytics)
```

---

## 2. FLEET DRIVER (`role = DRIVER`, **active fleet association**)

Same code path as DCO (`recomputeDriverVerifiedAndEnabled`, same config). **Only difference:**
`OperatorCode` is skipped — `applicableTo = INDIVIDUAL` doesn't apply to fleet drivers (the fleet
provides the operator linkage).

```
 Fleet adds a driver  →  person.role = DRIVER, active fleet association
   │  isFleetDriver = isJust (QFDA.findByDriverId driverId True) = TRUE
   ▼
 DRIVER PROFILE — KYC docs (same as DCO)
   │
   ▼
 OPS-HUB INSPECTION APPROVED ⇒ DriverInspectionHub VALID   (deps gated, no flags)
   │
   ▼
 TRAINING ⇒ TrainingForm VALID
   │
   ▼
 ┌───────────────────────────────────────────────────────────────────┐
 │  OPERATOR CODE — NOT required for fleet drivers                    │
 │  applicableTo=INDIVIDUAL ⇒ docAppliesToDriver (Just True) = False  │
 │  ⇒ excluded from both verified and enabled checks                 │
 └───────────────────────────────────────────────────────────────────┘
   │
   ▼
 ╔═══════════════════════════════════════════════════════════════════╗
 ║ statusHandler · recomputeDriverVerifiedAndEnabled (isFleetDriver) ║
 ║   isMandatory VALID (OperatorCode EXCLUDED) ⇒ verified = TRUE      ║
 ╚═══════════════════════════════════════════════════════════════════╝
   │
   ▼
 BOT CHECK #1 → approved = TRUE
   │
   ▼
 SUBSCRIPTION (or fleet-sponsored) ⇒ subscribed = TRUE
   │
   ▼
 BOT CHECK #2 → MSDS + Medical (Image VALID) → triggers statusHandler
   │
   ▼
 ╔═══════════════════════════════════════════════════════════════════╗
 ║ statusHandler · recomputeDriverVerifiedAndEnabled                 ║
 ║   isMandatory VALID && isMandatoryForEnabling VALID               ║
 ║   (incl. MSDS+Medical; OperatorCode still EXCLUDED)               ║
 ║   && approved == Just True  ⇒ enabled = TRUE                      ║
 ╚═══════════════════════════════════════════════════════════════════╝
   │
   ▼
 GO ONLINE  (setActivity)
   requires: enabled && subscribed && active vehicle
             && fleet.enabled && active fleet subscription
   (a fleet's drivers cannot go online until the fleet owner is enabled
    and has an active fleet PREPAID_SUBSCRIPTION purchase)
```

> **Note — no cascade under enableBotFlow.** A fleet driver's enablement is computed **independently**
> from the fleet owner's. The owner enabling does NOT auto-enable its drivers (and disabling does NOT
> auto-disable them). Instead, the go-online gate enforces `fleet.enabled` directly at the point of
> going online. (Legacy flow still cascades owner→drivers.)

### DCO vs Fleet driver — the only difference

```haskell
isFleetDriver <- isJust <$> QFDA.findByDriverId person.id True   -- active fleet assoc?

docAppliesToDriver (Just isFleetDriver) applicableTo = case applicableTo of
  FLEET_AND_INDIVIDUAL -> True              -- everyone
  FLEET                -> isFleetDriver      -- fleet drivers only
  INDIVIDUAL           -> not isFleetDriver  -- DCO only
```

| | DCO (`isFleetDriver=False`) | Fleet driver (`isFleetDriver=True`) |
|---|---|---|
| OperatorCode (`applicableTo=INDIVIDUAL`) | ✅ required | ❌ skipped |
| Other driver docs (`FLEET_AND_INDIVIDUAL`) | ✅ required | ✅ required |
| Inspection / Training / MSDS / Medical | same | same |
| Extra go-online gates | — | fleet.enabled + fleet subscription |

> If a fleet driver is later unlinked → `isFleetDriver` flips to False → `OperatorCode` becomes
> required → they effectively become a DCO. (Unlink handling owned separately.)

---

## 3. VEHICLE (RC) — has `verified` + `approved`, **no `enabled` flag**

```
 RC entered → eligibility checks → auto-verified
   │
   ▼
 VEHICLE PROFILE — RC → Insurance → Fitness → Permit → PUC → Vehicle Photos
   │
   ▼
 VEHICLE OPS HUB → VEHICLE inspection request created (InspectionHub)
   │
   ▼
 ┌───────────────────────────────────────────────────────────────────┐
 │ OPS-HUB INSPECTION APPROVED  (handleVehicleInspectionApproval)     │
 │  • dependency gate: all InspectionHub deps must be VALID          │
 │    (RC, Insurance, Fitness, PUC, InspectionForm), else throw      │
 │  • OperationHubRequest → APPROVED ⇒ InspectionHub doc VALID       │
 │  • writes NO flags — does NOT set approved, does NOT activate RC   │
 │    (only the BOT approves; statusHandler/BOT activates the RC)     │
 └───────────────────────────────────────────────────────────────────┘
   │
   ▼
 ╔═══════════════════════════════════════════════════════════════════╗
 ║ statusHandler · getVehicleDocuments (enableBotFlow)               ║
 ║   checkAllVehicleDocsValidForVerified (isMandatory, incl. Inspection)║
 ║   ⇒ VRC.verified = updateVerifiedByCertificateNumberHash          ║
 ║     (writes Just True / Just False — bidirectional)               ║
 ╚═══════════════════════════════════════════════════════════════════╝
   │
   ▼
 ┌────────────────────────────────────────────────────┐
 │ BOT review (dashboard doc-update) → VRC.approved=TRUE│  (BOT-owned)
 │ RC activation also BOT / statusHandler-owned         │
 └────────────────────────────────────────────────────┘

 NOTE: vehicle has NO `enabled` flag. Flags = { verified (statusHandler), approved (BOT) }.
 NOTE: vehicle docs ignore applicableTo (predicate passes Nothing).

 ── a mandatory vehicle doc later REJECTED ──
    statusHandler → VRC.verified = Just False
```

---

## 4. FLEET OWNER (`role = FLEET_OWNER | FLEET_BUSINESS`) — **no inspection, no `approved` flag**

```
 FLEET OWNER PROFILE — fleet docs
   AadhaarCard → PanCard → BusinessLicense → GSTCertificate → (UDYAM/TAN/LDC optional)
   │  config = FleetOwnerDocumentVerificationConfig, matched by person.role
   ▼
 ╔═══════════════════════════════════════════════════════════════════╗
 ║ statusHandler · recomputeFleetVerifiedAndEnabled                  ║
 ║   ALL isMandatory fleet docs VALID (excludes OperatorCode)        ║
 ║   ⇒ fleet.verified = TRUE  (written directly, both ways)          ║
 ╚═══════════════════════════════════════════════════════════════════╝
   │
   ▼
 ┌───────────────────────────────────────────────────────────────────┐
 │ BOT inputs fleet OperatorCode ⇒ OperatorCode doc VALID            │
 │  FleetOwnerDVC row, per role: isMandatory=false,                  │
 │  isMandatoryForEnabling=true, no applicableTo. BOT-controlled.    │
 └───────────────────────────────────────────────────────────────────┘
   │
   ▼
 ╔═══════════════════════════════════════════════════════════════════╗
 ║ statusHandler · recomputeFleetVerifiedAndEnabled                  ║
 ║   ALL isMandatoryForEnabling fleet docs VALID                     ║
 ║   (incl. BOT-set OperatorCode)                                    ║
 ║   ⇒ fleet.enabled = TRUE  (written directly — NO cascade)         ║
 ╚═══════════════════════════════════════════════════════════════════╝

 NOTE: fleet owner has NO inspection and NO `approved` flag — the BOT-controlled
   OperatorCode going VALID IS the enable gate.
 NOTE: enable/disable is DECOUPLED from drivers under enableBotFlow — no cascade.
   Each fleet driver enables independently (§2). (Legacy flow still cascades.)

 ── a mandatory fleet doc later REJECTED ──
    statusHandler → fleet.verified = FALSE
                  → fleet.enabled  = FALSE  (updateFleetOwnerEnabledStatus False, no cascade)
```

---

## Side-by-side comparison

| | **DCO driver** | **Fleet driver** | **Vehicle (RC)** | **Fleet owner** |
|---|---|---|---|---|
| `person.role` | DRIVER | DRIVER | — | FLEET_OWNER / FLEET_BUSINESS |
| Fleet association | none | **active** | — | — |
| Config table | DVC | DVC | DVC (vehicle docs) | FleetOwnerDVC (by role) |
| Recompute fn | `recomputeDriverVerifiedAndEnabled` | `recomputeDriverVerifiedAndEnabled` | `getVehicleDocuments` | `recomputeFleetVerifiedAndEnabled` |
| `verified` | isMandatory VALID | isMandatory VALID (OperatorCode excl.) | isMandatory VALID (incl. InspectionHub) | isMandatory VALID (OperatorCode excl.) |
| `approved` | BOT #1 | BOT #1 | BOT (doc-update) | — (no flag) |
| `enabled` | isMandatory + isMandatoryForEnabling + approved | same | — (no flag) | isMandatoryForEnabling VALID (incl. BOT OperatorCode); **no cascade** |
| Inspection | DriverInspectionHub | DriverInspectionHub | InspectionHub | none |
| Enable-only docs | MSDS + Medical (BOT) | MSDS + Medical (BOT) | — | OperatorCode (BOT input) |
| OperatorCode | ✅ required (DVC, `INDIVIDUAL`, derived from operator link) | ❌ skipped (`INDIVIDUAL`) | n/a | required (FleetOwnerDVC, by role, BOT-input) |
| applicableTo split | applies (`Just False`) | applies (`Just True`) | ignored (`Nothing`) | n/a (matched by `config.role`) |
| Disable on reject | `disableDriverWithAnalytics` | `disableDriverWithAnalytics` | `verified = Just False` | `updateFleetOwnerEnabledStatus False` (no cascade) |
| Extra go-online gate | active vehicle | active vehicle + fleet.enabled + fleet subscription | n/a | n/a |

---

## Cross-cutting checks (under `enableBotFlow`)

| Check | Where | Effect |
|---|---|---|
| **Dependency gate** | `handle{Vehicle,Driver}InspectionApproval` (`Operator/Driver.hs`) | ops can approve only if every `dependencyDocumentType` doc of the inspection-hub config is VALID, else throws naming the invalid docs |
| **Go-online: active vehicle** | `setActivity` (`UI/Driver.hs`) | all drivers: `isJust mbVehicle`, else "Cannot go online: no active vehicle linked" |
| **Go-online: fleet enabled** | `setActivity` | fleet drivers: fleet owner `enabled`, else "Cannot go online: fleet is not enabled" |
| **Go-online: fleet subscription** | `setActivity` | fleet drivers: active fleet `PREPAID_SUBSCRIPTION` purchase (via `SubscriptionPurchase` table — `fleetOwnerInfo.subscribed` is never written), else "Cannot go online: fleet subscription is not active" |

---

## The single invariant

**statusHandler derives `verified` / `enabled` from current document validity every time it runs**,
in both directions. Ops makes inspection docs VALID (gated on their dependencies); the BOT sets
`approved` and uploads enable-only docs; statusHandler reconciles the flags up and down. No flag is
set anywhere else under `enableBotFlow`. With the flag off, the legacy / Ops-Hub flow is untouched.
