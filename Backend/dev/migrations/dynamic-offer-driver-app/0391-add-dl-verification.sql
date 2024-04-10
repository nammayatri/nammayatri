ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN dl_number_verification bool ;



---------- DROP QUERY  -----------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN safety_webhook_auth_token ;