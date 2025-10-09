-- Run in Master
INSERT INTO atlas_driver_offer_bpp.volunteer (
    id,
    created_at,
    updated_at,
    place,
    vendor_id,
    is_active
)
VALUES (
    '0ad46579-e881-4576-bc1e-c9a5b38fe785',
    NOW(),
    NOW(),
    'Kolkata Station',
    'KOLKATA_STATION_CHITPUR',
    TRUE
)
ON CONFLICT (id,vendor_id) DO NOTHING;

------ PLEASE CONNECT WITH DUSHYANT OR UTKARSH BEFORE RELEASE --------------------------
------ ALERT : Disable this Table in KV First then Run this update query and then run read only migration as we are adding new column to Primary Key------
------ Run in Prod Even if Commented

UPDATE atlas_driver_offer_bpp.volunteer
SET vendor_id = CASE place
    WHEN 'Airport' THEN 'KOLKATA_AIRPORT'
    WHEN 'Santragachi Station' THEN 'SANTRAGACHI_STATION'
    WHEN 'Kolkata Station' THEN 'KOLKATA_STATION_CHITPUR'
    WHEN 'Sealdah Station' THEN 'SEALDAH_STATION'
    WHEN 'Howrah Station' THEN 'HOWRAH_STATION'
END
WHERE place IN (
    'Airport',
    'Santragachi Station',
    'Kolkata Station',
    'Sealdah Station',
    'Howrah Station'
);