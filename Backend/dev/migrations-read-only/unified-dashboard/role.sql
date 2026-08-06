CREATE TABLE atlas_dashboard.role ();

ALTER TABLE atlas_dashboard.role ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.role ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_dashboard.role ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.role ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_dashboard.role ADD COLUMN needs_bpp_account_creation boolean NOT NULL;
ALTER TABLE atlas_dashboard.role ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.role ADD PRIMARY KEY ( id);
