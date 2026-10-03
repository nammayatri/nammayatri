-- Corrections on top of 0014-bot-flow-onboarding-doc-config-seed.sql for MSIL_PARTNER.
--
-- 0014 is already applied everywhere, so it is left untouched and the deltas land here instead
-- (migrations are append-only — editing an applied file changes its checksum and it will not re-run).
-- Everything below is a plain UPDATE/DELETE, so re-applying is a no-op.
--
-- Scope matches 0014: every operating city of merchant short_id = 'MSIL_PARTNER',
-- vehicle_category = 'CAR' on the driver/vehicle table.
--
-- NOTE: document_verification_config is cached by driver-app. With inMemConfig.enableInMem = True it
-- needs a restart; with it disabled the cache is the Redis hash
-- "CacheHash:DRIVER-CONFIG_DocumentVerificationConfig-MerchantOperatingCityId:<opCityId>", which must
-- be deleted for this to take effect without one.

-- ============================================================
-- atlas_driver_offer_bpp
-- ============================================================

-- RUN IN MASTER

------------------------------------------------------------------------------------------------------
-- A) Driver OperatorPartnerCode also gates `verified`, not just `enabled`.
--    0014 set is_mandatory = false, which makes the document invisible to the ForVerified pass:
--    checkIfDocumentValid' evaluates `isDocRequiredFor mode cfg && docAppliesToDriver ...`, and
--    isDocRequiredFor ForVerified reads is_mandatory (Common.hs) — so with false it short-circuits
--    and applicable_to is never even consulted. A DCO driver with no operator association could
--    therefore reach verified. is_mandatory = true closes that.
--
--    applicable_to stays 'INDIVIDUAL', so docAppliesToDriver returns False for a fleet driver and
--    their verified path is unaffected.
--
--    is_hidden = true: the status is derived from the driver<->operator association, never uploaded,
--    so it should not appear as an actionable item in the driver app.
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_mandatory = true,
    is_hidden = true,
    updated_at = CURRENT_TIMESTAMP
WHERE document_type = 'OperatorPartnerCode'
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- B) MSDS (DrivingSchoolCertificate) + MedicalCertificate: no image validation.
--    0014 set is_image_validation_required = true on the assumption that no provider backs these, so
--    the upload should land VALID by itself. These are ops-supplied scans with nothing to extract,
--    and running validation on them only produces spurious INVALID images.
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_image_validation_required = false, updated_at = CURRENT_TIMESTAMP
WHERE document_type IN ('DrivingSchoolCertificate', 'MedicalCertificate')
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- C) MedicalCertificate upload roles: widen '{OPERATOR,ADMIN}' to the same set the other
--    ops-supplied documents use, so a fleet owner / fleet business / the driver can supply it too
--    rather than it being operator-or-admin only.
------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.document_verification_config
SET roles_allowed_to_upload_document_text = '{OPERATOR,FLEET_OWNER,FLEET_BUSINESS,DRIVER}'::text[],
    updated_at = CURRENT_TIMESTAMP
WHERE document_type = 'MedicalCertificate'
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

------------------------------------------------------------------------------------------------------
-- D) Rating is not part of the MSIL document set — drop the row 0014 seeded.
--    Safe: it gated nothing (is_mandatory = false AND is_mandatory_for_enabling = false), it only
--    appeared in the document list.
------------------------------------------------------------------------------------------------------
DELETE FROM atlas_driver_offer_bpp.document_verification_config
WHERE document_type = 'Rating'
  AND vehicle_category = 'CAR'
  AND merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');
