CREATE TABLE atlas_driver_offer_bpp.driver_block_reason (
    reason_code text NOT NULL PRIMARY KEY,
    block_reason text,
    block_time_in_hours int
);

INSERT INTO atlas_driver_offer_bpp.driver_block_reason ( reason_code, block_reason, block_time_in_hours) VALUES
    ('101', 'Repeated driver cancellations', 24),
    ('102', 'Sexual harassment', NULL),
    ('103', 'Rash driving', 168),
    ('others', ' ', NULL);

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN blocked_reason Text;
ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN block_expiry_time timestamp with time zone;