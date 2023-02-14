ALTER TABLE
   atlas_app.beckn_request
ALTER COLUMN
   signature_header TYPE text USING (signature_header :: text);
