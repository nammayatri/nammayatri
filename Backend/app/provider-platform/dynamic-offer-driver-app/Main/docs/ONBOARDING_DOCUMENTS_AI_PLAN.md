# AI Plan: Driver Onboarding Document Flows

Structured plan for implementing Pan onboarding, DriverGSTIN changes, and UDYAM certificate. Use this with the NammaDSL generator (YAML → `src-read-only`), and implement business logic in `src/`.

---

## 1. PAN Onboarding (App and Dashboard)

**→ Full plan (Goal, App/HyperVerge, Dashboard/Idfy, storage, steps, checklist) is in [PAN_ONBOARDING_APP_PLAN.md](./PAN_ONBOARDING_APP_PLAN.md).**

---

## 2. DriverGSTIN (0nly dashboard)

### Goal
- Keep OCR data in `driver_gstin` only.
- Remove GSTIN from `fleet_owner_information` and `driver_information`.
- Add verification status `MANUAL_VERIFICATION_REQUIRED` initially; update after verifyGstAsync is integrated

### Steps

1. **Confirm current GSTIN storage**
   - **driver_gstin** (already has OCR-related fields): `gstinEncrypted`, `gstinHash`, `documentImageId1/2`, `verificationStatus`, `verifiedBy`, etc. (`Storage/Beam/DriverGstin.hs`).
   - **fleet_owner_information**: `gstNumber`, `gstNumberDec`, `gstImageId` (`spec/Storage/FleetOwnerInformation.yaml`).
   - **driver_information**: check for any `gst*` / `driverBankAccountDetails`-style JSON that might hold GSTIN; confirm no dedicated GSTIN column.

2. **Add verification status for GSTIN**
   - Ensure `Kernel.Types.Documents.VerificationStatus` (or equivalent) includes `MANUAL_VERIFICATION_REQUIRED`.
   - In `driver_gstin` flow:
     - When creating/updating a row after OCR: set `verificationStatus = MANUAL_VERIFICATION_REQUIRED`.
     - After verifyGstAsync call (post-OCR): update same row to the final status (e.g. `VERIFIED` / `REJECTED`).
   - Code: `Domain/Action/UI/DriverOnboarding/GstVerification.hs`, and any shared GSTIN verification module.

3. **Remove GSTIN from fleet_owner_information**
   - In `spec/Storage/FleetOwnerInformation.yaml`:
     - Remove (or deprecate) fields: `gstNumber`, `gstNumberDec`, `gstImageId` and their `beamFields` / `fromTType` / `toTType` mappings.
   - Add migration to drop columns: `gst_number_encrypted`, `gst_number_hash`, `gst_number_dec`, `gst_image_id` (exact names from current schema).
   - Run generator; fix all references to these fields (e.g. `Storage/Queries/FleetOwnerInformation`, `Domain/Action/**/Fleet*`, dashboard APIs).
   - If fleet owners still need GSTIN, consider reading from `driver_gstin` (or a dedicated fleet GSTIN table) instead.

4. **Remove GSTIN from driver_information**
   - In `spec/Storage/DriverInformation.yaml`: remove any GSTIN-related field (e.g. if present in `fields` or `beamType`).
   - Add migration to drop corresponding column(s) from `atlas_driver_offer_bpp.driver_information`.
   - Run generator; update all usages to use `driver_gstin` table only.

5. ** verifyGstAsync APIs integration**
   - Identify where OCR completes (e.g. after Idfy/OCR callback or internal step).
   - Call erifyGstAsync with OCR output (e.g. GSTIN number, image IDs).
   - On success/failure: update `driver_gstin.verificationStatus` (and optionally `verifiedBy`, timestamps).
   - Ensure this runs “after OCR” and before marking onboarding step as complete (if applicable).

6. **Tests**
   - Verify: OCR → row in `driver_gstin` with `MANUAL_VERIFICATION_REQUIRED`; after verifyGstAsync call → status update; no GSTIN reads from `fleet_owner_information` or `driver_information`.

---


## 3. UDYAM Certificate (App and Dashboard)

### Goal
New document type “UDYAM Certificate”: extraction and verification both via Idfy; new table `udhyam_certificate` (or `udyam_certificate`); same high-level process as other verified documents (e.g. PAN, GSTIN).

### Steps

1. **NammaDSL – new table**
   - In `spec/Storage/DriverOnboarding.yaml` (or a new YAML if preferred), define:
     - **UdyamCertificate** (or **UdhyamCertificate** to match “udhyam” in spec):
       - Fields: `id`, `driverId`, `merchantId`, `merchantOperatingCityId`, `documentImageId1` (and optionally `documentImageId2`), `verificationStatus`, `verifiedBy`, Idfy request/response identifiers if needed, `createdAt`, `updatedAt`.
       - Optional: encrypted/hashed fields for Udyam number if stored (similar to `driver_gstin` / `driver_pan_card`).
   - Add `enableKVPG` and primary/secondary keys (e.g. `id`; secondary `driverId`).
   - Run generator to get: `Storage/Beam/UdyamCertificate.hs`, `Storage/Queries/UdyamCertificate.hs`, `Domain/Types/UdyamCertificate.hs`, migration in `dev/migrations-read-only/dynamic-offer-driver-app/`.

2. **Document type**
   - Add `UDYAMCertificate` to DocumentType enums (already present in `DriverOnboarding.yaml` line 546; confirm API/dashboard enums).
   - Add to document verification config so this type uses “extraction + verification via Idfy”.

3. **Idfy integration**
   - **Extraction:** On image upload, call Idfy extraction API for Udyam certificate; map response to domain type (e.g. Udyam number, name, validity).
   - **Verification:** Call Idfy verification API (or same API if it does both); on callback/webhook, update `udhyam_certificate.verificationStatus` and `verifiedBy`.
   - Reuse patterns from: `Domain/Action/UI/DriverOnboarding/GstVerification.hs`, `AadhaarVerification.hs`, `PanVerification.hs`, and Idfy modules in `Kernel.External.Verification` / `Tools.Verification`.

4. **Domain flow**
   - **Upload:** Accept image + metadata → create row in `udhyam_certificate` with status e.g. `PENDING` / `MANUAL_VERIFICATION_REQUIRED` → trigger Idfy extraction.
   - **Callback/Webhook:** On Idfy response → update row (extracted data if stored, verification status).
   - **APIs:** Expose “upload Udyam certificate” and “get Udyam certificate status” in DriverOnboarding (and dashboard if required).

5. **Migration and cabal**
   - Ensure new migration is applied (e.g. `udyam_certificate.sql` or `udhyam_certificate.sql`).
   - Add new modules to `dynamic-offer-driver-app.cabal` if the generator does not add them (e.g. `Storage.Queries.UdyamCertificate`, `Storage.Beam.UdyamCertificate`).

6. **Tests**
   - Upload Udyam image → row created; Idfy extraction/verification called; status updated on callback; no regression in other document flows.

### Dependencies

1. **Idfy ** – Idfy currently exposes only the **ind_Udyog_Aadhaar** OCR API. We need to upload a Udyam certificate and verify whether this API works for extraction/verification.
2. **Sample Udyam certificate** – Waiting for Janani to provide a Udyam certificate so we can verify point 1 (ind_Udyog_Aadhaar OCR) end-to-end.
3. **Fallback if OCR API does not work** – If **ind_Udyog_Aadhaar** OCR is not suitable:
   - Store the Udyam certificate **image** in **Common Driver Onboarding Documents** (same as other common docs).
   - Collect **udyam_number** (user-entered or from another source).
   - Verify using Idfy **udyam_aadhaar** async API (async verification flow).

---

## Execution Order (Suggested)

1. **DriverGSTIN** – Verification status + remove GSTIN from fleet_owner_information and driver_information; then verifyGstAsync APIs.
2. **UDYAM** – New table + Idfy extraction/verification + APIs.
3. **PAN onboarding** – HyperVerge response handling and DriverPanCard (or new table) changes.

---

## Conventions (from project)

- **Do not edit `src-read-only`** – change only YAML specs and run `, run-generator --apply-hint`.
- **Migrations:** Prefer `dev/migrations-read-only/` for generator output; use `dev/migrations/` for hand-written migrations if allowed.
- **Build:** From repo root, `nix develop .#backend` then `cd Backend && cabal build all`.
- **Document types:** Keep in sync across `DriverOnboarding.yaml`, dashboard API YAMLs, and any document verification config.

---

## File / Module Quick Reference

| Area              | Spec / Config                                         | Read-only modules (generated)                    | Editable logic (examples)                          |
|-------------------|--------------------------------------------------------|--------------------------------------------------|----------------------------------------------------|
| PAN               | DriverOnboarding.yaml (DriverPanCard)                  | Storage/Beam/DriverPanCard, Queries/DriverPanCard| PanVerification.hs, HyperVergeWebhook.hs           |
| GSTIN             | DriverOnboarding.yaml (DriverGstin), DriverInformation, FleetOwnerInformation | Beam/DriverGstin, Queries/DriverGstin   | GstVerification.hs                                 |
| UDYAM             | New table in DriverOnboarding.yaml (or new YAML)        | Beam/UdyamCertificate, Queries/UdyamCertificate  | New module mirroring GstVerification + Idfy         |
