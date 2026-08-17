CREATE TABLE atlas_app.frfs_booking_group ();

ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN payment_order_short_id character varying(36) ;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN currency text ;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN total_slots integer NOT NULL;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_booking_group ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_booking_group ADD PRIMARY KEY ( id);
