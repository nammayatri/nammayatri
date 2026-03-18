-- Ride Allocation Framework tables
-- Stores ride allocation policies, rules, weights, and audit history
-- for the fleet marketplace ride distribution system.

-- ============================================
-- ride_allocation_policy: Core policy entity
-- ============================================
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.ride_allocation_policy (
    id VARCHAR(36) PRIMARY KEY DEFAULT gen_random_uuid(),
    name TEXT NOT NULL,
    description TEXT,
    merchant_id VARCHAR(36) NOT NULL,
    city_id VARCHAR(36) NOT NULL,
    status VARCHAR(20) NOT NULL DEFAULT 'DRAFT'
        CHECK (status IN ('DRAFT', 'ACTIVE', 'INACTIVE', 'ARCHIVED')),
    priority INT NOT NULL DEFAULT 0,
    effective_from TIMESTAMP WITH TIME ZONE,
    effective_to TIMESTAMP WITH TIME ZONE,
    created_by VARCHAR(36) NOT NULL,
    updated_by VARCHAR(36) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    version INT NOT NULL DEFAULT 1
);

CREATE INDEX IF NOT EXISTS idx_rap_merchant_city_status
    ON atlas_driver_offer_bpp.ride_allocation_policy(merchant_id, city_id, status);
CREATE INDEX IF NOT EXISTS idx_rap_active_priority
    ON atlas_driver_offer_bpp.ride_allocation_policy(city_id, status, priority)
    WHERE status = 'ACTIVE';

-- ============================================
-- ride_allocation_rule: Conditions within a policy
-- ============================================
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.ride_allocation_rule (
    id VARCHAR(36) PRIMARY KEY DEFAULT gen_random_uuid(),
    policy_id VARCHAR(36) NOT NULL
        REFERENCES atlas_driver_offer_bpp.ride_allocation_policy(id) ON DELETE CASCADE,
    rule_order INT NOT NULL,
    condition_type VARCHAR(50) NOT NULL
        CHECK (condition_type IN (
            'ZONE', 'VEHICLE_VARIANT', 'TIME_WINDOW',
            'FLEET_PERFORMANCE_TIER', 'CAPACITY_UTILIZATION',
            'DRIVER_RATING_MIN', 'FLEET_SIZE_MIN'
        )),
    operator VARCHAR(20) NOT NULL
        CHECK (operator IN ('EQUALS', 'NOT_EQUALS', 'IN', 'NOT_IN',
                            'GREATER_THAN', 'LESS_THAN', 'BETWEEN')),
    value JSONB NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_rar_policy_id
    ON atlas_driver_offer_bpp.ride_allocation_rule(policy_id);

-- ============================================
-- ride_allocation_weight: Fleet owner/tier weight assignments
-- ============================================
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.ride_allocation_weight (
    id VARCHAR(36) PRIMARY KEY DEFAULT gen_random_uuid(),
    policy_id VARCHAR(36) NOT NULL
        REFERENCES atlas_driver_offer_bpp.ride_allocation_policy(id) ON DELETE CASCADE,
    fleet_owner_id VARCHAR(36),
    fleet_tier VARCHAR(20),
    weight NUMERIC(5,2) NOT NULL CHECK (weight >= 0 AND weight <= 100),
    max_rides_per_hour INT,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    CONSTRAINT chk_fleet_or_tier CHECK (
        (fleet_owner_id IS NOT NULL AND fleet_tier IS NULL) OR
        (fleet_owner_id IS NULL AND fleet_tier IS NOT NULL)
    )
);

CREATE INDEX IF NOT EXISTS idx_raw_policy_id
    ON atlas_driver_offer_bpp.ride_allocation_weight(policy_id);

-- ============================================
-- ride_allocation_policy_history: Audit trail
-- ============================================
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.ride_allocation_policy_history (
    id VARCHAR(36) PRIMARY KEY DEFAULT gen_random_uuid(),
    policy_id VARCHAR(36) NOT NULL
        REFERENCES atlas_driver_offer_bpp.ride_allocation_policy(id),
    action VARCHAR(30) NOT NULL
        CHECK (action IN ('CREATED', 'UPDATED', 'ACTIVATED', 'DEACTIVATED', 'ARCHIVED')),
    changed_by VARCHAR(36) NOT NULL,
    previous_state JSONB,
    new_state JSONB,
    change_summary TEXT,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_raph_policy_id
    ON atlas_driver_offer_bpp.ride_allocation_policy_history(policy_id, created_at);
