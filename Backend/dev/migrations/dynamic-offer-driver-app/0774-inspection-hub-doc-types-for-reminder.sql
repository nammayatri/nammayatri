UPDATE atlas_driver_offer_bpp.reminder_config
SET document_type = 'DriverInspectionHub'
WHERE document_type = 'DriverInspectionForm'
AND merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);

UPDATE atlas_driver_offer_bpp.reminder_config
SET document_type = 'InspectionHub'
WHERE document_type = 'VehicleInspectionForm'
AND merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);
