CREATE TABLE atlas_driver_offer_bpp.cancellation_fare_policy ();

ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN free_cancellation_time_seconds integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN max_cancellation_charge double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN max_waiting_time_at_pickup_seconds integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN min_cancellation_charge double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN per_metre_cancellation_charge double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN per_minute_cancellation_charge double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN percentage_of_ride_fare_to_be_charged double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_fare_policy ALTER COLUMN percentage_of_ride_fare_to_be_charged TYPE text;