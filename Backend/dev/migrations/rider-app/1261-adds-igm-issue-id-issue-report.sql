ALTER TABLE atlas_app.issue_report ADD beckn_issue_id character varying(36);
ALTER TABLE atlas_app.issue_category ADD igm_category text;
ALTER TABLE atlas_app.issue_option ADD igm_sub_category text;

update atlas_app.issue_category set igm_category = 'ORDER' where id = 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w';
update atlas_app.issue_option set igm_sub_category = 'ORD111' where id = 'pxz7sc8s-2uaq-s00y-1ii8-4luatg4bz58g';

update atlas_app.issue_category set igm_category = 'PAYMENT' where id = 'jdir4kkp-49fb-4x75-mqgf-3ig3mm2d7ecy';
update atlas_app.issue_option set igm_sub_category = 'PMT112' where id = 'k7zgdgit-8jwq-uudw-xanw-ptegr1nwrxe6';
update atlas_app.issue_option set igm_sub_category = 'PMT113' where id = 'b9m8ztgh-x9p1-eepm-p4pu-8tada97ngo1x';