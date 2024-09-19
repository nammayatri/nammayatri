ALTER TABLE atlas_app.igm_issue ADD COLUMN customer_email character varying(36);

ALTER TABLE atlas_app.igm_issue ADD COLUMN customer_name character varying(36);

ALTER TABLE atlas_app.igm_issue ADD COLUMN customer_phone character varying(36);

ALTER TABLE atlas_app.igm_issue ADD COLUMN issue_raised_by_merchant character varying(36);

ALTER TABLE atlas_app.igm_issue ADD COLUMN merchant_id character varying(36);

ALTER TABLE atlas_app.igm_issue ADD COLUMN resolution_action character varying(36);

ALTER TABLE atlas_app.igm_issue ADD COLUMN domain character varying(36) not null;