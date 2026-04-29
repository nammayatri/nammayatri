-- This index was previously defined in 0205-add-feedback-badges.sql (now deleted).
-- It likely already exists in production environments.

CREATE INDEX IF NOT EXISTS idx_driver_feedback ON atlas_driver_offer_bpp.feedback_badge USING btree (driver_id);
