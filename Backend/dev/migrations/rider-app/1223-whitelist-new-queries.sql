ALTER TABLE atlas_app.white_list_org
ADD COLUMN domain character varying(255) NULL;

UPDATE atlas_app.white_list_org
SET domain = 'MOBILITY';

ALTER TABLE atlas_app.white_list_org
ALTER COLUMN domain SET NOT NULL;

-- Run these queries in prod after successful release to maintain backward compatibility

ALTER TABLE atlas_app.white_list_org
DROP COLUMN type;
