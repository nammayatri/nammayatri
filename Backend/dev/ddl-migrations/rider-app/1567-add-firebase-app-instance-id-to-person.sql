-- GA4 app instance id (Analytics SDK getAppInstanceId, not the Firebase installation id) sent by the
-- client via POST /profile; used for S2S Firebase Analytics events.
ALTER TABLE atlas_app.person ADD COLUMN IF NOT EXISTS firebase_app_instance_id text;
