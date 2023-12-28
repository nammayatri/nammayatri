ALTER TABLE atlas_app.payment_order ADD COLUMN is_retried boolean DEFAULT false NOT NULL;
ALTER TABLE atlas_app.payment_order ADD COLUMN is_retargeted boolean DEFAULT false NOT NULL;
ALTER TABLE atlas_app.payment_order ADD COLUMN retarget_link Text;