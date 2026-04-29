-- only run these queries in master release for testing

ALTER TABLE atlas_app.issue_report ADD beckn_issue_id character varying(36);
ALTER TABLE atlas_app.issue_category ADD igm_category text;
ALTER TABLE atlas_app.issue_option ADD igm_sub_category text;