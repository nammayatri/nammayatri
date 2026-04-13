ALTER TABLE atlas_bpp_dashboard.role ADD COLUMN parent_role_id character(36) REFERENCES atlas_bpp_dashboard.role (id);

ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN user_action_types_for_descendants_check text[];
