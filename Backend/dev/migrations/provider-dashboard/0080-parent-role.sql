ALTER TABLE atlas_bpp_dashboard.role ADD COLUMN parent_role_id character(36) REFERENCES atlas_bpp_dashboard.role (id);
