UPDATE atlas_driver_offer_bpp.transporter_config SET default_popup_delay = 2;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN default_popup_delay SET NOT NULL;

-- Run this whole query in the master db
UPDATE atlas_driver_offer_bpp.transporter_config
SET
    allowed_referral_entities = '{OPERATOR, FLEET_OWNER}',
    generate_referral_code_for_operator = true,
    generate_referral_code_for_fleet = true,
    enable_existing_vehicle_in_bulk_upload = true,
    require_route_mapping_in_vehicle = false,
    deactivate_rc_on_unlink = false,
    disable_driver_when_unlinking_vehicle = false,
    allow_duplicate_pan = false,
    allow_duplicate_gst = false,
    allow_duplicate_aadhaar = false,
    valid_name_compare_percentage = 90,
    enable_mobile_number_validation = true
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);

UPDATE atlas_driver_offer_bpp.transporter_config
SET
    allow_rc_unlink_when_driver_offline = true
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city = 'Helsinki'
);