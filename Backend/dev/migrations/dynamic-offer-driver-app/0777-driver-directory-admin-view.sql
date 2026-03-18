-- Driver Directory Admin View: schema additions for admin management of directory entries
-- Depends on driver_directory_entry table from P3 Driver Directory foundation

-- Add admin management columns to driver_directory_entry
ALTER TABLE atlas_driver_offer_bpp.driver_directory_entry
    ADD COLUMN IF NOT EXISTS admin_status VARCHAR(20) NOT NULL DEFAULT 'pending'
        CHECK (admin_status IN ('pending', 'approved', 'suspended', 'flagged')),
    ADD COLUMN IF NOT EXISTS admin_reviewed_by VARCHAR(36),
    ADD COLUMN IF NOT EXISTS admin_reviewed_at TIMESTAMP WITH TIME ZONE,
    ADD COLUMN IF NOT EXISTS admin_notes TEXT,
    ADD COLUMN IF NOT EXISTS profile_completeness_score INT NOT NULL DEFAULT 0
        CHECK (profile_completeness_score BETWEEN 0 AND 100),
    ADD COLUMN IF NOT EXISTS suspension_reason TEXT;

-- Indexes for admin directory queries
CREATE INDEX IF NOT EXISTS idx_dde_admin_status
    ON atlas_driver_offer_bpp.driver_directory_entry(admin_status);

CREATE INDEX IF NOT EXISTS idx_dde_merchant_city_status
    ON atlas_driver_offer_bpp.driver_directory_entry(merchant_id, city_id, admin_status);

-- Audit log table for admin actions on directory listings
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_directory_audit_log (
    id VARCHAR(36) PRIMARY KEY DEFAULT gen_random_uuid(),
    driver_id VARCHAR(36) NOT NULL,
    action VARCHAR(50) NOT NULL
        CHECK (action IN ('enrolled', 'approved', 'suspended', 'flagged', 'reinstated', 'profile_updated')),
    performed_by VARCHAR(36) NOT NULL,
    reason TEXT,
    previous_status VARCHAR(20),
    new_status VARCHAR(20),
    metadata JSONB,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_ddal_driver_id
    ON atlas_driver_offer_bpp.driver_directory_audit_log(driver_id, created_at);

CREATE INDEX IF NOT EXISTS idx_ddal_performed_by
    ON atlas_driver_offer_bpp.driver_directory_audit_log(performed_by);

-- Consolidated view for directory listing queries
CREATE OR REPLACE VIEW atlas_driver_offer_bpp.driver_directory_view AS
SELECT
    p.id AS driver_id,
    p.first_name,
    p.last_name,
    p.mobile_number,
    p.mobile_country_code,
    p.email,
    p.rating,
    p.enabled,
    p.blocked,
    p.created_at AS joined_at,
    p.merchant_id,
    p.merchant_operating_city_id AS city_id,
    fa.fleet_owner_id,
    fo.first_name AS fleet_owner_name,
    v.vehicle_number,
    v.variant AS vehicle_variant,
    v.model AS vehicle_model,
    di.verification_status AS onboarding_status,
    dde.admin_status,
    dde.profile_completeness_score,
    dde.enrolled_at,
    dde.admin_reviewed_at,
    COALESCE(tc.modules_completed, 0) AS training_modules_completed,
    COALESCE(tc.total_modules, 0) AS training_total_modules,
    COALESCE(rs.total_rides, 0) AS total_rides_completed
FROM atlas_driver_offer_bpp.person p
INNER JOIN atlas_driver_offer_bpp.driver_directory_entry dde ON dde.driver_id = p.id
LEFT JOIN atlas_driver_offer_bpp.fleet_driver_association fa ON fa.driver_id = p.id AND fa.is_active = true
LEFT JOIN atlas_driver_offer_bpp.person fo ON fo.id = fa.fleet_owner_id
LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
LEFT JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
LEFT JOIN atlas_driver_offer_bpp.driver_training_completion tc ON tc.driver_id = p.id
LEFT JOIN atlas_driver_offer_bpp.driver_ride_stats rs ON rs.driver_id = p.id;

-- Additional indexes to support efficient joins
CREATE INDEX IF NOT EXISTS idx_person_merchant_city
    ON atlas_driver_offer_bpp.person(merchant_id, merchant_operating_city_id);

CREATE INDEX IF NOT EXISTS idx_fda_driver_active
    ON atlas_driver_offer_bpp.fleet_driver_association(driver_id) WHERE is_active = true;
