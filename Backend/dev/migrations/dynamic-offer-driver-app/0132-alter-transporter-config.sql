UPDATE atlas_driver_offer_bpp.transporter_config SET default_popup_delay = 2;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN default_popup_delay SET NOT NULL;

UPDATE atlas_driver_offer_bpp.transporter_config
SET
    allowed_referral_entities = '{OPERATOR, FLEET_OWNER}',
    generate_referral_code_for_operator = true,
    generate_referral_code_for_fleet = true,
    enable_existing_vehicle_in_bulk_upload = true,
    require_route_mapping_in_vehicle = false
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);