INSERT INTO atlas_app.exophone (id, merchant_id, merchant_operating_city_id, primary_phone, backup_phone, is_primary_down)
    (SELECT
        atlas_app.uuid_generate_v4(),
        T1.id,
        'mOpCityId',
        unnest (T1.exo_phones),
        unnest (T1.exo_phones),
        false
    FROM atlas_app.merchant AS T1)
    ON CONFLICT DO NOTHING;

ALTER TABLE atlas_app.merchant DROP COLUMN exo_phones;


-- updating after dsl generated query
ALTER TABLE atlas_app.booking ALTER COLUMN primary_exophone DROP NOT NULL;
UPDATE atlas_app.booking
    SET primary_exophone = 'UNKNOWN';
ALTER TABLE atlas_app.booking ALTER COLUMN primary_exophone SET NOT NULL;

