UPDATE atlas_app.rider_config
SET kapture_config = (kapture_config::jsonb || '{"l0FeedbackQueue": "SOS_BLR"}'::jsonb)::json
WHERE kapture_config IS NOT NULL;