-- Purpose: Backfilling Aadhaar Verification data to Aadhaar Card table.
INSERT INTO atlas_driver_offer_bpp.aadhaar_card  (
  aadhaar_back_image_id,
  aadhaar_front_image_id,
  address,
  consent,
  consent_timestamp,
  created_at,
  date_of_birth,
  driver_id,
  masked_aadhaar_number,
  merchant_id,
  merchant_operating_city_id,
  name_on_card,
  updated_at,
  verification_status,
  driver_image_path,
  driver_image,
  driver_gender,
  aadhaar_number_hash
)
SELECT
  null as aadhaar_back_image_id,
  null as aadhaar_front_image_id,
  null as address,
  true as consent,
  CURRENT_TIMESTAMP as consent_timestamp,
  created_at,
  driver_dob,
  driver_id,
  null as masked_aadhaar_number,
  merchant_id,
  id,
  driver_name,
  updated_at,
  CASE
    WHEN is_verified = true THEN 'VALID'
    ELSE 'INVALID'
  END as verification_status,
  driver_image_path,
  driver_image,
  driver_gender,
  aadhaar_number_hash
FROM atlas_driver_offer_bpp.aadhaar_verification
  CROSS JOIN
    (SELECT merchant_id, id
     FROM atlas_driver_offer_bpp.merchant_operating_city
     WHERE merchant_short_id = 'JATRI_SAATHI_PARTNER' AND city = 'Kolkata') moc;
