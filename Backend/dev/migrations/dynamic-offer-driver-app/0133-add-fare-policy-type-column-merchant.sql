ALTER TABLE atlas_driver_offer_bpp.merchant
  ADD COLUMN fare_policy_type character varying(255) NOT NULL DEFAULT 'NORMAL';

UPDATE atlas_driver_offer_bpp.merchant SET
  fare_policy_type = 'NORMAL'
  WHERE short_id = 'NAMMA_YATRI_PARTNER';

UPDATE atlas_driver_offer_bpp.merchant SET
  fare_policy_type = 'SLAB'
  WHERE short_id = 'JATRI_SAATHI_PARTNER';
