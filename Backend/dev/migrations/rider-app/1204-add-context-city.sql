ALTER TABLE  atlas_app.merchant_operating_city ADD COLUMN context_city character varying(255);
UPDATE atlas_app.merchant_operating_city
SET context_city = city;
ALTER TABLE atlas_app.merchant_operating_city
ALTER COLUMN context_city SET NOT NULL;
UPDATE atlas_app.merchant_operating_city
SET context_city = city where city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI';