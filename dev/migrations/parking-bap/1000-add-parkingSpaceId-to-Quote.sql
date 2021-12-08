ALTER TABLE atlas_parking.quote ADD COLUMN bpp_item_id character(36) NOT NULL;
UPDATE atlas_parking.quote SET bpp_item_id = 'UNKNOWN';
ALTER TABLE atlas_parking.quote ALTER COLUMN bpp_item_id SET NOT NULL;

ALTER TABLE atlas_parking.booking ADD COLUMN bpp_item_id character(36) NOT NULL;
UPDATE atlas_parking.quote SET bpp_item_id = 'UNKNOWN';
ALTER TABLE atlas_parking.booking ALTER COLUMN bpp_item_id SET NOT NULL;

ALTER TABLE atlas_parking.booking DROP COLUMN parking_support_number;