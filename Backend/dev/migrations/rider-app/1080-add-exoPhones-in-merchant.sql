ALTER TABLE atlas_app.merchant ADD COLUMN exo_phones text[] ;
UPDATE atlas_app.merchant
    SET exo_phones = '{"8069457995","8035272983"}', exo_phone_country_code = '+91'
    WHERE short_id = 'NAMMA_YATRI';
UPDATE atlas_app.merchant
    SET exo_phones = '{"8069457995","8035272983"}', exo_phone_country_code = '+91'
    WHERE short_id = 'YATRI';