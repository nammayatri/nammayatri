UPDATE atlas_app.rider_config rc
SET kapture_config = jsonb_build_object(
    'queue', rc.kapture_queue,
    'sosQueue', 'SOS_BLR',
    'disposition', m.kapture_disposition
)
FROM atlas_app.merchant m
WHERE rc.merchant_id = m.id;