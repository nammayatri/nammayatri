ALTER TABLE
   atlas_driver_offer_bpp.beckn_request
ALTER COLUMN
   signature_header TYPE text USING (signature_header :: text);
