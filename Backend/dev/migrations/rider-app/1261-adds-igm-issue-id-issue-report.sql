ALTER TABLE atlas_app.issue_report ADD beckn_issue_id character varying(36);
ALTER TABLE atlas_app.issue_option ADD igm_category text;
ALTER TABLE atlas_app.issue_option ADD igm_sub_category text;

UPDATE atlas_app.issue_option SET igm_category = "FULFILLMENT" WHERE id = "2d1c22eb-f576-4c9b-9d92-985d29149254";
UPDATE atlas_app.issue_option SET igm_sub_category = "FLM112" WHERE id = "2d1c22eb-f576-4c9b-9d92-985d29149254";

update atlas_app.issue_option set igm_category = 'ORDER' where id = 'pxz7sc8s-2uaq-s00y-1ii8-4luatg4bz58g';
update atlas_app.issue_option set igm_sub_category = 'ORD111' where id = 'pxz7sc8s-2uaq-s00y-1ii8-4luatg4bz58g';

update atlas_app.issue_option set igm_category = 'PAYMENT' where id = 'lpab2oxv-14td-jubb-i5df-83rq3sh55iyu';
update atlas_app.issue_option set igm_sub_category = 'PMT112' where id = 'lpab2oxv-14td-jubb-i5df-83rq3sh55iyu';

update atlas_app.issue_option set igm_category = 'PAYMENT' where id = 'asdfghjk-2345-6789-0poi-1q2w3e4r5t6y';
update atlas_app.issue_option set igm_sub_category = 'PMT113' where id = 'asdfghjk-2345-6789-0poi-1q2w3e4r5t6y';

update atlas_app.issue_option set igm_category = 'PAYMENT' where id = 'b9m8ztgh-x9p1-eepm-p4pu-8tada97ngo1x';
update atlas_app.issue_option set igm_sub_category = 'PMT113' where id = 'b9m8ztgh-x9p1-eepm-p4pu-8tada97ngo1x';