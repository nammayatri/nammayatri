ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS always_allowed_notification_categories text[] DEFAULT '{RIDE_RELATED,SAFETY}';
