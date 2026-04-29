ALTER TABLE atlas_driver_offer_bpp.merchant
  ADD COLUMN fare_policy_type character varying(255) NOT NULL DEFAULT 'NORMAL';
