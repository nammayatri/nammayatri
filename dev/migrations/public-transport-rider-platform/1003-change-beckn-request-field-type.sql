ALTER TABLE
   atlas_public_transport.beckn_request
ALTER COLUMN
   signature_header TYPE text USING (signature_header :: text);
