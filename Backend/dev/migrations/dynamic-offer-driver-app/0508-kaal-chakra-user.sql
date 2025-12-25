GRANT USAGE ON SCHEMA atlas_driver_offer_bpp TO kaal_chakra_user;

GRANT SELECT ON TABLE atlas_driver_offer_bpp.app_dynamic_logic TO kaal_chakra_user;
GRANT SELECT ON atlas_driver_offer_bpp.namma_tag TO kaal_chakra_user;
GRANT SELECT ON atlas_driver_offer_bpp.chakra_queries TO kaal_chakra_user;
GRANT ALL ON atlas_driver_offer_bpp.user_data TO kaal_chakra_user;
GRANT SELECT, UPDATE ON atlas_driver_offer_bpp.person TO kaal_chakra_user;
GRANT SELECT ON atlas_driver_offer_bpp.system_configs TO kaal_chakra_user;
GRANT SELECT ON atlas_driver_offer_bpp.merchant_operating_city TO kaal_chakra_user;
GRANT SELECT ON atlas_driver_offer_bpp.merchant TO kaal_chakra_user;


-- required only for local testing
UPDATE atlas_driver_offer_bpp.person SET merchant_id = 'favorit0-0000-0000-0000-00000favorit' WHERE id = 'favorit-auto1-0000000000000000000000';
UPDATE atlas_driver_offer_bpp.registration_token SET merchant_id = 'favorit0-0000-0000-0000-00000favorit' WHERE id = 'favorit-auto1-0000000000000000000000';
UPDATE atlas_driver_offer_bpp.person SET merchant_id = 'favorit0-0000-0000-0000-00000favorit' WHERE id = 'favorit-bike-00000000000000000000000';
UPDATE atlas_driver_offer_bpp.registration_token SET merchant_id = 'favorit0-0000-0000-0000-00000favorit' WHERE id = 'favorit-bike-00000000000000000000000';
