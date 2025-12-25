-- Please Run this Queries in master release for testing

ALTER TABLE atlas_driver_offer_bpp.issue_category ADD igm_category text;
ALTER TABLE atlas_driver_offer_bpp.issue_option ADD igm_sub_category text;

update atlas_driver_offer_bpp.issue_category set igm_category = 'ORDER' where id = '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6';
update atlas_driver_offer_bpp.issue_option set igm_sub_category = 'ORD111' where id = '169ae361-ab6e-4c77-b5ff-c898114d6b3c';

update atlas_driver_offer_bpp.issue_category set igm_category = 'PAYMENT' where id = '5ca814d9-66e2-4ccc-b236-40b73b705e88';
update atlas_driver_offer_bpp.issue_option set igm_sub_category = 'PMT112' where id = '3d68dbe3-bc3a-44a0-939a-7ad166c4818b';
update atlas_driver_offer_bpp.issue_option set igm_sub_category = 'PMT113' where id = '16bf2a0d-229e-4afc-be9d-a06fb1bd0581';