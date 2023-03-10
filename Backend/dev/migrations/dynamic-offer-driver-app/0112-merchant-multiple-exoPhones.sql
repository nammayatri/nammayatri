ALTER TABLE atlas_driver_offer_bpp.merchant
    ADD COLUMN exo_phones character varying(255) [];

UPDATE atlas_driver_offer_bpp.merchant
    SET exo_phones = '{ExoPhone}'
    WHERE exo_phones IS NULL;

UPDATE atlas_driver_offer_bpp.merchant
    SET exo_phones = '{+918047108594}'
    WHERE short_id = 'YATRI';

ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN exo_phones SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.booking
    ADD COLUMN provider_exo_phone character varying(255);

UPDATE atlas_driver_offer_bpp.booking
    SET provider_exo_phone = 'UNKNOWN';

ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN provider_exo_phone SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.call_status
    RENAME COLUMN exotel_call_sid TO call_id;