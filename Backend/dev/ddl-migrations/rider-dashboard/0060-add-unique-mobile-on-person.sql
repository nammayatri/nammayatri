-- Closes the cross-request race in PT employee bulkCreate: two admins at
-- different merchants uploading a CSV with the same phone can't both succeed.
-- Partial-scope unique key on the login composite key.
ALTER TABLE atlas_bap_dashboard.person
  ADD CONSTRAINT person_unique_mobile
  UNIQUE (mobile_country_code, mobile_number_hash);
