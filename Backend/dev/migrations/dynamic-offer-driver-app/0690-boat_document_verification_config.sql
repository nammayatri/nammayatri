--------------------------------------------------------------------------------------------------------
--------------------------- LOCAL TESTING, DO NOT RUN IN MASTER OR PROD --------------------------------
--------------------------------------------------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
  check_expiry, check_extraction, dependency_document_type, description, disable_warning,
  document_type, is_disabled, is_hidden, is_mandatory, max_retry_count,
  merchant_id, merchant_operating_city_id, rc_number_prefix_list, supported_vehicle_classes_json,
  title, vehicle_category, vehicle_class_check_type, created_at, updated_at,
  is_default_enabled_on_manual_verification, is_image_validation_required
)
SELECT
  check_expiry,
  check_extraction,
  dependency_document_type,
  description,
  disable_warning,
  document_type,
  is_disabled,
  is_hidden,
  is_mandatory,
  max_retry_count,
  merchant_id,
  (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI_PARTNER') AS merchant_operating_city_id,
  '{OD}',                             -- Odisha RC prefix
  supported_vehicle_classes_json,
  title,
  'BOAT' AS vehicle_category,
  vehicle_class_check_type,
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP,
  is_default_enabled_on_manual_verification,
  is_image_validation_required
FROM atlas_driver_offer_bpp.document_verification_config
WHERE vehicle_category = 'CAR'
  AND merchant_operating_city_id = (
    SELECT merchant_operating_city_id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI_PARTNER'
  )
  AND document_type IN ('ProfilePhoto','DriverLicense','Permissions','AadhaarCard')
ON CONFLICT DO NOTHING;

-- BOAT DVC: VehicleRegistrationCertificate (copy from CAR → BOAT)
INSERT INTO atlas_driver_offer_bpp.document_verification_config (
  check_expiry, check_extraction, dependency_document_type, description, disable_warning,
  document_type, is_disabled, is_hidden, is_mandatory, max_retry_count,
  merchant_id, merchant_operating_city_id, rc_number_prefix_list, supported_vehicle_classes_json,
  title, vehicle_category, vehicle_class_check_type, created_at, updated_at,
  is_default_enabled_on_manual_verification, is_image_validation_required
)
SELECT
  false AS check_expiry,
  false AS check_extraction,
  dependency_document_type,
  description,
  disable_warning,
  document_type,
  is_disabled,
  true  AS is_hidden,
  false AS is_mandatory,
  max_retry_count,
  (SELECT merchant_id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI_PARTNER') AS merchant_id,
  (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI_PARTNER') AS merchant_operating_city_id,
  '{OD}',                             -- Odisha RC prefix
  supported_vehicle_classes_json,
  title,
  'BOAT' AS vehicle_category,
  vehicle_class_check_type,
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP,
  is_default_enabled_on_manual_verification,
  is_image_validation_required
FROM atlas_driver_offer_bpp.document_verification_config
WHERE vehicle_category = 'CAR'
  AND merchant_operating_city_id = (
    SELECT merchant_operating_city_id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI_PARTNER'
  )
  AND document_type IN ('VehicleRegistrationCertificate')
ON CONFLICT DO NOTHING;

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = json_build_array()
WHERE document_type = 'DriverLicense'
  AND vehicle_category = 'BOAT'
  AND merchant_operating_city_id = (
    SELECT merchant_operating_city_id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI_PARTNER'
  );