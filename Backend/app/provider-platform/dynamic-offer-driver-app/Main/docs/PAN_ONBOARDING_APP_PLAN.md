# PAN Onboarding (App and Dashboard) – Full Plan

This document is the **complete** plan for PAN onboarding: **App** (HyperVerge) and **Dashboard** (Idfy `pan_aadhaar_link`), plus shared storage and tests. The main [ONBOARDING_DOCUMENTS_AI_PLAN.md](./ONBOARDING_DOCUMENTS_AI_PLAN.md) points here for all PAN onboarding details.

---

## 1. PAN Onboarding (App and Dashboard)

### Goal

Persist **aadhaar linkage**, **pan status**, and **pan category** in the Driver PAN card table. **App** uses HyperVerge; **Dashboard** uses Idfy `pan_aadhaar_link` for aadhaar linkage.

### Steps (overview)

1. **App** – Verify HyperVerge response and persist aadhaar linkage, pan status, pan category. *(Detailed below.)*
2. **Dashboard** – Call Idfy **pan_aadhaar_link** API; consume `result.details.message` and create mapping to stored value; persist in `driver_pan_card`.
3. **Extend DriverPanCard storage** – Add `aadhaarLinkage`, `panStatus`, `panCategory` in `spec/Storage/DriverOnboarding.yaml` (DriverPanCard); run generator and add migration.
4. **Create/update entries** – App: parse HyperVerge response and persist; Dashboard: map Idfy message and persist. Use `Storage.Queries.DriverPanCard` patterns.
5. **Tests** – Response parsing, DB write, and read-back for all 3 values.

### Dashboard – Idfy `pan_aadhaar_link` API and aadhaar linkage mapping

- Call Idfy **pan_aadhaar_link** API from the dashboard PAN flow.
- Consume **`result.details.message`** from the response (valid string).
- **Create a mapping** from the message to a stored enum/value. Possible values:
  - `"PAN & Aadhaar are linked"`
  - `"Aadhaar linked to some other PAN"`
  - `"Aadhaar PAN linking failed due to name mismatch"`
  - `"This PAN number does not exist"`
- Map these strings to a domain enum or `aadhaarLinkage` values in `driver_pan_card` (e.g. `PAN_AADHAAR_LINKED`, `AADHAAR_LINKED_TO_OTHER_PAN`, `NAME_MISMATCH`, `PAN_DOES_NOT_EXIST`) and persist in the Driver PAN card table.

---

## 2. App (HyperVerge) – Goal

Persist HyperVerge response fields (**aadhaar linkage**, **PAN status**, **PAN category**) in the Driver PAN card table when the driver completes PAN verification from the **App** (HyperVerge flow).

---

## 3. App – Verify HyperVerge Response Shape *(aadhaar linkage – to be confirmed)*

### 3.1 Check whether HyperVerge returns aadhaar linkage

- **Status:** We are not sure yet; this needs to be verified.
- **Actions:**
  1. **Review HyperVerge API documentation** (partner docs or API spec) for PAN verification / PAN–Aadhaar linking response fields.
  2. **Inspect live/sandbox response:** Trigger a PAN verification from the App (or use a test harness) and capture the full HyperVerge response JSON.
  3. **Search codebase** for existing HyperVerge response types and where the response is parsed:
     - `Domain/Action/UI/DriverOnboarding/PanVerification.hs` – PAN verification flow.
     - `Domain/Action/UI/DriverOnboarding/HyperVergeWebhook.hs` – webhook handler (if verification is async).
     - Any HyperVerge type modules in `Kernel.External.*` (e.g. `Kernel.External.HyperVerge.*` or similar).
  4. **Decide:** If aadhaar linkage is **not** in the response, document it and skip persisting it for App; rely on Dashboard (Idfy) for aadhaar linkage only.

### 3.2 Identify exact field names and values (once confirmed)

- **Aadhaar linkage** (if present):
  - Field name in response (e.g. `aadhaar_link_status`, `panAadhaarLink`, etc.).
  - Possible values (e.g. `PAN_AADHAAR_LINKED`, `AADHAAR_LINKED_TO_OTHER_PAN`, `PAN_AADHAAR_NOT_LINKED`,  `PAN_DOES_NOT_EXIST`). Create an enum or mapping for storage.
- **PAN status:**
  - Field name (e.g. `pan_status`, `status`).
  - Values: e.g. VALID / INVALID `driver_pan_card.panStatus`.
- **PAN category:**
  - Field name (e.g. `category`, `pan_category`).
  - Values: e.g. INDIVIDUAL / COMPANY / TRUST . Map to a domain enum or `Maybe Text` for `driver_pan_card.panCategory`.

### 3.3 Document response shape

- Add a short comment or table in this file (or in code) documenting:
  - HyperVerge endpoint(s) used for PAN verification (sync vs async/webhook).
  - Sample response (redacted) or a link to internal doc.
  - Mapping: `HyperVerge field → our domain/storage field`.

---

## 4. Code Locations (App – HyperVerge)

| Purpose                    | Module / path |
|---------------------------|--------------------------------------|
| PAN verification (App)    | `Domain/Action/UI/DriverOnboarding/PanVerification.hs` |
| HyperVerge webhook        | `Domain/Action/UI/DriverOnboarding/HyperVergeWebhook.hs` |
| HyperVerge types / client  | `Kernel.External.*` (search for HyperVerge) |
| Driver PAN card storage   | `Storage/Queries/DriverPanCard.hs` (read-only), `Storage/Queries/DriverPanCardExtra.hs` |
| Beam / domain types       | `Storage/Beam/DriverPanCard.hs`, `Domain/Types/DriverPanCard.hs` (read-only; extend via YAML) |

---

## 5. Implementation Steps (App flow)

### 5.1 Extend storage (after response shape is known)

- In `spec/Storage/DriverOnboarding.yaml` (DriverPanCard entity), add:
  - `aadhaarLinkage :: Maybe Text` (or enum) – **only if** HyperVerge is confirmed to return it.
  - `panStatus :: Maybe Text` (or enum).
  - `panCategory :: Maybe Text` (or enum).
- Run `, run-generator --apply-hint` and add migration for new columns.

### 5.2 Parse HyperVerge response in App flow

- In the code path that receives the HyperVerge PAN result (e.g. webhook handler or sync callback in `PanVerification.hs` or `HyperVergeWebhook.hs`):
  1. Parse the JSON/response into the existing (or new) HyperVerge domain type.
  2. Extract:
     - `aadhaarLinkage` (if present) → map to storage value.
     - `panStatus` → map to storage value.
     - `panCategory` → map to storage value.
  3. Handle missing or null fields (store `Nothing` or a default if appropriate).

### 5.3 Persist to driver_pan_card

- After successful PAN verification:
  - **If creating a new row:** use `Storage.Queries.DriverPanCard.create` with the 3 new fields (plus existing required fields).
  - **If updating an existing row:** use the appropriate update query (e.g. from DriverPanCard Queries or Extra) to set `aadhaarLinkage`, `panStatus`, `panCategory`.
- Reuse existing patterns: `QDPC.create`, error handling from `Kernel.Types.Error`, and transaction boundaries as in similar flows.

### 5.4 Edge cases

- **Webhook vs sync:** If verification is async (webhook), ensure the webhook handler has access to `driverId` / `personId` (e.g. from request ID or callback payload) to update the correct `driver_pan_card` row.
- **Idempotency:** If the same HyperVerge result can be delivered more than once, make updates idempotent (e.g. update only if status is still PENDING or if timestamp is newer).

---

## 6. Verification Checklist

- [ ] HyperVerge response (live or sandbox) captured and documented.
- [ ] Confirmed whether aadhaar linkage is present; if not, documented and App storage for it skipped or left as `Nothing`.
- [ ] Field names and value sets for `panStatus` and `panCategory` documented and mapped.
- [ ] DriverPanCard storage extended via YAML; migration added and applied.
- [ ] Parse logic added in PanVerification.hs and/or HyperVergeWebhook.hs.
- [ ] Persist logic (create/update) added and tested.
- [ ] Unit or flow tests for: response parsing, DB write, and reading back the 3 values.

---

## 7. References

- Main plan: [ONBOARDING_DOCUMENTS_AI_PLAN.md](./ONBOARDING_DOCUMENTS_AI_PLAN.md) – PAN section (Dashboard Idfy flow, shared storage, execution order).
- NammaDSL: change only YAML under `spec/Storage/DriverOnboarding.yaml`; run `, run-generator --apply-hint`; do not edit `src-read-only`.
