CREATE TABLE atlas_app.frfs_ticket_booking_feedback ();

ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN is_fare_accepted boolean;
ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_booking_feedback ADD COLUMN feedback_details text ;