ALTER TABLE atlas_app.booking ADD COLUMN transaction_id character(36);
UPDATE atlas_app.booking AS T1 SET transaction_id = 'UNKNOWN';
ALTER TABLE atlas_app.booking ALTER COLUMN transaction_id SET NOT NULL;