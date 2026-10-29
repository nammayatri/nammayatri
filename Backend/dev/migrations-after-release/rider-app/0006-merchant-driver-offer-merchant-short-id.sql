-- Fill merchant.driver_offer_merchant_short_id with the short id of the driver
-- (BPP) merchant behind each rider merchant. The rider side only stores the
-- driver merchant's id; the Xyne support bot context needs its short id so the
-- workflow can address driver-side records (e.g. NAMMA_YATRI -> NAMMA_YATRI_PARTNER).
-- Mapping taken from atlas_driver_offer_bpp.merchant on prod and master.
-- Only rows still unset are touched, so re-running is harmless.
UPDATE atlas_app.merchant SET driver_offer_merchant_short_id = 'NAMMA_YATRI_PARTNER'
  WHERE driver_offer_merchant_short_id IS NULL
    AND short_id IN ('NAMMA_YATRI', 'ANNA_APP', 'MOBILITY_PAYTM', 'MOBILITY_REDBUS', 'ODISHA_YATRI');
UPDATE atlas_app.merchant SET driver_offer_merchant_short_id = 'YATRI_PARTNER'
  WHERE driver_offer_merchant_short_id IS NULL AND short_id = 'YATRI';
UPDATE atlas_app.merchant SET driver_offer_merchant_short_id = 'JATRI_SAATHI_PARTNER'
  WHERE driver_offer_merchant_short_id IS NULL AND short_id = 'JATRI_SAATHI';
UPDATE atlas_app.merchant SET driver_offer_merchant_short_id = 'BHARAT_TAXI_PARTNER'
  WHERE driver_offer_merchant_short_id IS NULL AND short_id = 'BHARAT_TAXI';
UPDATE atlas_app.merchant SET driver_offer_merchant_short_id = 'BRIDGE_CABS_PARTNER'
  WHERE driver_offer_merchant_short_id IS NULL AND short_id = 'BRIDGE_CABS';
UPDATE atlas_app.merchant SET driver_offer_merchant_short_id = 'BRIDGE_FINLAND_PARTNER'
  WHERE driver_offer_merchant_short_id IS NULL AND short_id = 'BRIDGE_FINLAND';
