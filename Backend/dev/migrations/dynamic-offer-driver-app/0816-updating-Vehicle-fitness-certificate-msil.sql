UPDATE atlas_driver_offer_bpp.document_verification_config
SET only_image_verification_status_lookup_required = true
WHERE document_type IN (
  'VehicleFitnessCertificate',
  'VehicleInspectionForm'
)
AND merchant_operating_city_id IN (
  SELECT id
  FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);

UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_hidden = true WHERE document_type = 'DrivingSchoolCertificate' AND merchant_operating_city_id IN (
  SELECT id
  FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);