ALTER TABLE atlas_driver_offer_bpp.white_list_org
ADD COLUMN domain character varying(255) default NULL;

update atlas_driver_offer_bpp.white_list_org set type = 'BPP' where type = 'APP';

update atlas_driver_offer_bpp.white_list_org set domain = 'MOBILITY' where type = 'BPP';


insert into atlas_driver_offer_bpp.white_list_org (id,subscriber_id,type,domain) values ('45a16bb9-69e1-4dc6-a751-65b82a84dbd9', 'localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'BPP', 'MOBILITY');

ALTER TABLE atlas_driver_offer_bpp.black_list_org
ADD COLUMN domain character varying(255) default NULL;