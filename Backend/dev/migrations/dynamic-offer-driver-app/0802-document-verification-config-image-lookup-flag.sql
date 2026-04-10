UPDATE atlas_driver_offer_bpp.document_verification_config
SET only_image_verification_status_lookup_required = true
WHERE document_type IN (
  'VehiclePermit',
  'VehicleInsurance',
  'VehiclePUC',
  'VehicleLeft',
  'VehicleRight',
  'VehicleFrontInterior',
  'VehicleBackInterior',
  'VehicleFront',
  'VehicleBack',
  'VehicleNOC',
  'InspectionHub'
)
AND merchant_operating_city_id IN (
  SELECT id
  FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);