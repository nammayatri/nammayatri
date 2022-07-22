ALTER TABLE
   atlas_transporter.beckn_request
ALTER COLUMN
   signature_header TYPE text USING (signature_header :: text);
