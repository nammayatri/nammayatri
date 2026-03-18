-- Training Module Feedback table and materialized view
-- Stores driver feedback/ratings on training modules for admin dashboard visibility

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.training_module_feedback (
    id VARCHAR(36) PRIMARY KEY DEFAULT gen_random_uuid(),
    driver_id VARCHAR(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person(id),
    training_module_id VARCHAR(36) NOT NULL,
    training_module_name TEXT NOT NULL,
    rating SMALLINT NOT NULL CHECK (rating BETWEEN 1 AND 5),
    feedback_text TEXT,
    city_id VARCHAR(36) NOT NULL,
    merchant_id VARCHAR(36) NOT NULL,
    fleet_owner_id VARCHAR(36),
    completed_at TIMESTAMP WITH TIME ZONE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_tmf_module_id ON atlas_driver_offer_bpp.training_module_feedback(training_module_id);
CREATE INDEX IF NOT EXISTS idx_tmf_city_created ON atlas_driver_offer_bpp.training_module_feedback(city_id, created_at);
CREATE INDEX IF NOT EXISTS idx_tmf_driver_id ON atlas_driver_offer_bpp.training_module_feedback(driver_id);
CREATE INDEX IF NOT EXISTS idx_tmf_merchant_city ON atlas_driver_offer_bpp.training_module_feedback(merchant_id, city_id);

-- Materialized view for aggregated feedback per module/city/merchant
CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.training_module_feedback_aggregated AS
SELECT
    training_module_id,
    training_module_name,
    city_id,
    merchant_id,
    COUNT(*) AS total_responses,
    AVG(rating)::NUMERIC(3,2) AS avg_rating,
    COUNT(CASE WHEN rating >= 4 THEN 1 END) AS promoters,
    COUNT(CASE WHEN rating <= 2 THEN 1 END) AS detractors,
    MAX(created_at) AS last_feedback_at
FROM atlas_driver_offer_bpp.training_module_feedback
GROUP BY training_module_id, training_module_name, city_id, merchant_id;

CREATE UNIQUE INDEX IF NOT EXISTS idx_tmfa_module_city_merchant
    ON atlas_driver_offer_bpp.training_module_feedback_aggregated(training_module_id, city_id, merchant_id);
