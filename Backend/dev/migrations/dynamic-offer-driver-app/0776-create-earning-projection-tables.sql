-- Earning Projection feature tables

CREATE TABLE atlas_driver_offer_bpp.driver_earning_goal (
    driver_id              VARCHAR(36) PRIMARY KEY REFERENCES atlas_driver_offer_bpp.person(id),
    daily_goal_amount      INT,
    daily_goal_currency    VARCHAR(5) DEFAULT 'INR',
    weekly_goal_amount     INT,
    weekly_goal_currency   VARCHAR(5) DEFAULT 'INR',
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    created_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE atlas_driver_offer_bpp.earning_projection_log (
    id                     VARCHAR(36) PRIMARY KEY,
    driver_id              VARCHAR(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person(id),
    projection_date        DATE NOT NULL,
    projection_type        VARCHAR(10) NOT NULL,
    projected_low          INT NOT NULL,
    projected_expected     INT NOT NULL,
    projected_high         INT NOT NULL,
    actual_earnings        INT,
    accuracy_pct           DOUBLE PRECISION,
    factors_json           TEXT,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    created_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_earning_projection_driver_date ON atlas_driver_offer_bpp.earning_projection_log(driver_id, projection_date);
CREATE INDEX idx_earning_projection_accuracy ON atlas_driver_offer_bpp.earning_projection_log(projection_date, merchant_operating_city_id);
