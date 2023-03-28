ALTER TABLE atlas_driver_offer_bpp.fare_parameters 
  ADD column waiting_or_pickup_charges integer,
  ADD column service_charge integer,
  ADD column fare_policy_type character varying(255) NOT NULL DEFAULT 'NORMAL';
