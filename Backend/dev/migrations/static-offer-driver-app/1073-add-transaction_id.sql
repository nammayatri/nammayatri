ALTER TABLE atlas_transporter.search_request ADD COLUMN transaction_id character(36);
UPDATE atlas_transporter.search_request AS T1 SET transaction_id = 'UNKNOWN';
ALTER TABLE atlas_transporter.search_request ALTER COLUMN transaction_id SET NOT NULL;

ALTER TABLE atlas_transporter.booking ADD COLUMN transaction_id character(36);
UPDATE atlas_transporter.booking AS T1 SET transaction_id = 'UNKNOWN';
ALTER TABLE atlas_transporter.booking ALTER COLUMN transaction_id SET NOT NULL;