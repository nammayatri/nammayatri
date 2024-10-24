CREATE TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ();

ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN callback_response text ;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN doc_type text ;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN failure_reason text ;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN hv_flow_id text ;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN status text ;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN txn_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.hyperverge_sdk_logs ADD PRIMARY KEY ( txn_id);