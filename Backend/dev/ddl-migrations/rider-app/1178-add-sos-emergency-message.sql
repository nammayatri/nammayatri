

ALTER TABLE atlas_app.person
  ADD COLUMN trigger_support boolean NOT NULL default true;

ALTER TABLE atlas_app.merchant ADD COLUMN tracking_short_url_pattern text DEFAULT 'nammayatri.in/t/' NOT NULL;