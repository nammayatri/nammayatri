-- Add "Driver asked me to cancel" cancellation reason on rider side.
INSERT INTO atlas_app.cancellation_reason
  (reason_code, enabled, on_assign, on_confirm, on_init, on_search, priority, description, created_at, updated_at)
VALUES
  ('DRIVER_ASKED_TO_CANCEL', true, true, true, false, false, 1,
   'Driver asked me to cancel', now(), now())
ON CONFLICT (reason_code) DO NOTHING;
