ALTER TABLE atlas_app.white_list_org
ADD COLUMN domain character varying(255) NULL;

UPDATE atlas_app.white_list_org
SET domain = 'MOBILITY';

ALTER TABLE atlas_app.white_list_org
ALTER COLUMN domain SET NOT NULL;

ALTER TABLE atlas_app.black_list_org
ADD COLUMN domain character varying(255) NOT NULL;

-- Run these queries in prod after successful release to maintain backward compatibility

ALTER TABLE atlas_app.white_list_org
DROP COLUMN type;

ALTER TABLE atlas_app.black_list_org
DROP COLUMN type;
