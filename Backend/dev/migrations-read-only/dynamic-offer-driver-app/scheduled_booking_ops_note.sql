CREATE TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ();

ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN booking_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN content text ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN created_by_dashboard_user_id text ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN note_type character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN status character varying (36) NOT NULL default 'PENDING';
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN transaction_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.scheduled_booking_ops_note ADD PRIMARY KEY ( id);
