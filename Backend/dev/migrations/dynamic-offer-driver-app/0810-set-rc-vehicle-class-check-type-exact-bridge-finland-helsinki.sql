UPDATE atlas_driver_offer_bpp.document_verification_config
SET vehicle_class_check_type = 'Exact'
WHERE document_type = 'VehicleRegistrationCertificate'
  AND merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'BRIDGE_FINLAND_PARTNER'
      AND city = 'Helsinki'
  );
