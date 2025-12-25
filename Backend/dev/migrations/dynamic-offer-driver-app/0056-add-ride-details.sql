INSERT INTO atlas_driver_offer_bpp.ride_details (
    id,
    driver_name,
    driver_number_encrypted,
    driver_number_hash,
    driver_country_code,
    vehicle_number,
    vehicle_color,
    vehicle_variant,
    vehicle_model,
    vehicle_class,
    created_at  -- Add the createdAt column to the column list
)
SELECT
    r.id,
    COALESCE(p.first_name, '[Driver deleted]'),
    p.mobile_number_encrypted,
    p.mobile_number_hash,
    p.mobile_country_code,
    COALESCE(v.registration_no, '[Vehicle deleted]'),
    v.color,
    v.variant,
    v.model,
    v.vehicle_class,
    now()  -- Provide now() for the createdAt column value
FROM
    atlas_driver_offer_bpp.ride AS r
LEFT JOIN
    atlas_driver_offer_bpp.person AS p ON r.driver_id = p.id
LEFT JOIN
    atlas_driver_offer_bpp.vehicle AS v ON r.driver_id = v.driver_id
ON CONFLICT DO NOTHING;
