ALTER TABLE atlas_app.white_list_org
ADD COLUMN domain character varying(255) default NULL;

update atlas_app.white_list_org set type = 'BAP' where type = 'APP';

update atlas_app.white_list_org set domain = 'MOBILITY' where type = 'BAP';

ALTER TABLE atlas_app.black_list_org
ADD COLUMN domain character varying(255) default NULL;
