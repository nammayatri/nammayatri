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

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_bundle_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_sdk_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_os_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_os_type text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN client_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN backend_config_version text ;
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN backend_app_version text ;

