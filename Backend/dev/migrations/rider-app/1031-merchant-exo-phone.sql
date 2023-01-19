ALTER TABLE atlas_app.merchant
    ADD COLUMN exo_phone character varying(255),
    ADD COLUMN exo_phone_country_code character varying(255);

UPDATE atlas_app.merchant
    SET exo_phone = '8047108594', exo_phone_country_code = '+91';