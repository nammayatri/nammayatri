INSERT INTO atlas_driver_offer_bpp.exophone (id, merchant_id, merchant_operating_city_id, primary_phone, backup_phone, is_primary_down)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.id,
        'merchantOpCId',
        unnest (T1.exo_phones),
        unnest (T1.exo_phones),
        false
    FROM atlas_driver_offer_bpp.merchant AS T1)
    ON CONFLICT DO NOTHING;

ALTER TABLE atlas_driver_offer_bpp.merchant DROP COLUMN exo_phones;