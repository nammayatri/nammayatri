------- driver_quote -------
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN backend_app_version text ;


--- search_request_for_driver ---
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN backend_app_version text ;


--- person ---

ALTER TABLE atlas_driver_offer_bpp.person RENAME bundle_version TO client_bundle_version;
ALTER TABLE atlas_driver_offer_bpp.person RENAME client_version TO client_sdk_version;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN backend_app_version text ;

--- quote_special_zone ---

ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN backend_app_version text ;

--- booking ---
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN backend_app_version text ;
