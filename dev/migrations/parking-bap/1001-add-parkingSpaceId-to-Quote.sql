ALTER TABLE atlas_parking.booking ADD COLUMN bpp_order_id character varying(255);
ALTER TABLE atlas_parking.booking ADD COLUMN requestor_name character varying(255);
ALTER TABLE atlas_parking.booking DROP COLUMN additional_info;

ALTER TABLE atlas_parking.payment_transaction DROP COLUMN bkn_txn_id;
ALTER TABLE atlas_parking.payment_transaction ADD COLUMN payment_gateway_txn_status character(36) NOT NULL;

ALTER TABLE atlas_parking.parking_location ALTER COLUMN id_from_bpp TYPE character varying(255);
ALTER TABLE atlas_parking.parking_location ALTER COLUMN name TYPE character varying(255);
ALTER TABLE atlas_parking.parking_location ALTER COLUMN street_address TYPE character varying(255);
ALTER TABLE atlas_parking.parking_location ALTER COLUMN locality TYPE character varying(255);
ALTER TABLE atlas_parking.parking_location ALTER COLUMN city TYPE character varying(255);
ALTER TABLE atlas_parking.parking_location ALTER COLUMN state TYPE character varying(255);
ALTER TABLE atlas_parking.parking_location ALTER COLUMN country TYPE character varying(255);
ALTER TABLE atlas_parking.parking_location ALTER COLUMN area_code TYPE character varying(255);

ALTER TABLE atlas_parking.quote ALTER COLUMN bpp_id TYPE character varying(255);
ALTER TABLE atlas_parking.quote ALTER COLUMN bpp_item_id TYPE character varying(255);
ALTER TABLE atlas_parking.quote ALTER COLUMN parking_location_id_from_bpp TYPE character varying(255);

ALTER TABLE atlas_parking.booking ALTER COLUMN bpp_id TYPE character varying(255);
ALTER TABLE atlas_parking.booking ALTER COLUMN bpp_item_id TYPE character varying(255);