-- Allow the same offer code in different cities/merchants; keep it unique within a city
ALTER TABLE atlas_app.offer DROP CONSTRAINT IF EXISTS unique_offer_code;
ALTER TABLE atlas_app.offer ADD CONSTRAINT unique_offer_code_per_city UNIQUE (merchant_operating_city_id, offer_code);
