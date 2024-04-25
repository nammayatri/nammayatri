CREATE TABLE atlas_driver_offer_bpp.cancellation_charges ();

ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN cancellation_charges_: double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN cancellation_charges double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges DROP COLUMN cancellation_charges:;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN maybe_utc_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges DROP COLUMN created_at;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ALTER COLUMN updated_at DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_charges DROP COLUMN maybe_utc_time;