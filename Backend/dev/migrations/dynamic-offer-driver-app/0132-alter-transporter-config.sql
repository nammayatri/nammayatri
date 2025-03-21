UPDATE atlas_driver_offer_bpp.transporter_config SET default_popup_delay = 2;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN default_popup_delay SET NOT NULL;

UPDATE atlas_driver_offer_bpp.transporter_config SET allowed_referral_entities = '{OPERATOR, FLEET_OWNER}' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');
