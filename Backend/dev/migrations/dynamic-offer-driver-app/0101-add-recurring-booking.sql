CREATE TABLE atlas_driver_offer_bpp.recurring_booking (
  id character varying(255) PRIMARY KEY,
  scheduled_days jsonb not null,
  start_date date not null,
  end_date date,
  pickup_time time not null,
  status character varying(255) not null,
  provider_id character varying(255) not null,
  provider_url character varying(255) not null,
  provider_name character varying(255) not null,
  provider_mobile_number character varying(255) not null,
  fare_policy_id character varying(255),
  rider_id character varying(255),
  from_location_id character varying(255),
  to_location_id character varying(255)
);

ALTER TABLE atlas_driver_offer_bpp.recurring_booking OWNER TO atlas_driver_offer_bpp_user;
