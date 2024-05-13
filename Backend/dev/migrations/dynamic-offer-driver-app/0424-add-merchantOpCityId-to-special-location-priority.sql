ALTER TABLE atlas_driver_offer_bpp.special_location_priority
ADD COLUMN merchant_operating_city_id character(36) NOT NULL DEFAULT 'temp';

INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, merchant_operating_city_id, category, pickup_priority, drop_priority)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    moc.merchant_id,
    moc.id AS merchant_operating_city_id,
    slp.category,
    slp.pickup_priority,
    slp.drop_priority
FROM
    atlas_driver_offer_bpp.merchant_operating_city as moc
JOIN
    atlas_driver_offer_bpp.special_location_priority as slp
ON
    slp.merchant_id = moc.merchant_id;