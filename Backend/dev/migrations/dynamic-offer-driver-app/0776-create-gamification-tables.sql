-- Gamification Lifetime Ride Journey feature tables

CREATE TABLE atlas_driver_offer_bpp.badge_definition (
    id                             VARCHAR(36) PRIMARY KEY,
    name                           TEXT NOT NULL,
    description                    TEXT NOT NULL,
    category                       VARCHAR(30) NOT NULL,
    icon_url                       TEXT NOT NULL,
    criteria_type                  VARCHAR(30) NOT NULL,
    criteria_value                 INT NOT NULL,
    xp_reward                      INT NOT NULL DEFAULT 0,
    coin_reward                    INT NOT NULL DEFAULT 0,
    tier                           VARCHAR(20) NOT NULL DEFAULT 'COMMON',
    is_active                      BOOLEAN NOT NULL DEFAULT TRUE,
    merchant_id                    VARCHAR(36) NOT NULL,
    merchant_operating_city_id     VARCHAR(36),
    created_at                     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at                     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE atlas_driver_offer_bpp.driver_badge (
    id                             VARCHAR(36) PRIMARY KEY,
    driver_id                      VARCHAR(36) NOT NULL,
    badge_definition_id            VARCHAR(36) NOT NULL REFERENCES atlas_driver_offer_bpp.badge_definition(id),
    earned_at                      TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    merchant_operating_city_id     VARCHAR(36) NOT NULL
);

CREATE UNIQUE INDEX idx_driver_badge_unique ON atlas_driver_offer_bpp.driver_badge(driver_id, badge_definition_id);
CREATE INDEX idx_driver_badge_driver ON atlas_driver_offer_bpp.driver_badge(driver_id, earned_at);

CREATE TABLE atlas_driver_offer_bpp.driver_gamification_progress (
    driver_id                      VARCHAR(36) PRIMARY KEY,
    total_xp                       INT NOT NULL DEFAULT 0,
    current_level                  VARCHAR(20) NOT NULL DEFAULT 'BRONZE',
    current_streak_days            INT DEFAULT 0,
    longest_streak_days            INT DEFAULT 0,
    last_ride_date                 DATE,
    badges_earned_count            INT DEFAULT 0,
    merchant_operating_city_id     VARCHAR(36) NOT NULL,
    created_at                     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at                     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE atlas_driver_offer_bpp.gamification_level_config (
    id                             VARCHAR(36) PRIMARY KEY,
    level_name                     VARCHAR(20) NOT NULL,
    min_xp                         INT NOT NULL,
    icon_url                       TEXT,
    perks_description              TEXT,
    display_order                  INT NOT NULL,
    merchant_id                    VARCHAR(36) NOT NULL,
    created_at                     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at                     TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Seed default level configurations (using placeholder merchant_id)
-- These should be updated with actual merchant IDs during deployment
INSERT INTO atlas_driver_offer_bpp.gamification_level_config (id, level_name, min_xp, icon_url, perks_description, display_order, merchant_id) VALUES
    ('gl-bronze-default', 'BRONZE', 0, NULL, 'Starting level - Welcome to the journey!', 1, 'default-merchant'),
    ('gl-silver-default', 'SILVER', 1000, NULL, 'Earn priority support access', 2, 'default-merchant'),
    ('gl-gold-default', 'GOLD', 3000, NULL, 'Earn priority ride allocation', 3, 'default-merchant'),
    ('gl-platinum-default', 'PLATINUM', 7000, NULL, 'Earn premium incentives and priority support', 4, 'default-merchant'),
    ('gl-diamond-default', 'DIAMOND', 15000, NULL, 'Top tier - Maximum rewards and exclusive benefits', 5, 'default-merchant');

-- Seed default badge definitions
INSERT INTO atlas_driver_offer_bpp.badge_definition (id, name, description, category, icon_url, criteria_type, criteria_value, xp_reward, coin_reward, tier, is_active, merchant_id) VALUES
    ('badge-rides-100', 'Century Rider', 'Complete 100 rides', 'RIDES', 'https://cdn.nammayatri.in/badges/century-rider.png', 'TOTAL_RIDES', 100, 100, 50, 'COMMON', TRUE, 'default-merchant'),
    ('badge-rides-500', 'Road Explorer', 'Complete 500 rides', 'RIDES', 'https://cdn.nammayatri.in/badges/road-explorer.png', 'TOTAL_RIDES', 500, 250, 125, 'RARE', TRUE, 'default-merchant'),
    ('badge-rides-1000', 'Thousand Miler', 'Complete 1000 rides', 'RIDES', 'https://cdn.nammayatri.in/badges/thousand-miler.png', 'TOTAL_RIDES', 1000, 500, 250, 'RARE', TRUE, 'default-merchant'),
    ('badge-rides-5000', 'Road Warrior', 'Complete 5000 rides', 'RIDES', 'https://cdn.nammayatri.in/badges/road-warrior.png', 'TOTAL_RIDES', 5000, 1000, 500, 'EPIC', TRUE, 'default-merchant'),
    ('badge-rides-10000', 'Legend Driver', 'Complete 10000 rides', 'RIDES', 'https://cdn.nammayatri.in/badges/legend-driver.png', 'TOTAL_RIDES', 10000, 2000, 1000, 'LEGENDARY', TRUE, 'default-merchant'),
    ('badge-night-50', 'Night Owl', 'Complete 50 late night trips', 'SPECIAL', 'https://cdn.nammayatri.in/badges/night-owl.png', 'LATE_NIGHT_TRIPS', 50, 200, 100, 'RARE', TRUE, 'default-merchant'),
    ('badge-night-200', 'Midnight Legend', 'Complete 200 late night trips', 'SPECIAL', 'https://cdn.nammayatri.in/badges/midnight-legend.png', 'LATE_NIGHT_TRIPS', 200, 500, 250, 'EPIC', TRUE, 'default-merchant'),
    ('badge-streak-7', 'Weekly Warrior', 'Maintain a 7-day ride streak', 'STREAK', 'https://cdn.nammayatri.in/badges/weekly-warrior.png', 'STREAK_DAYS', 7, 150, 75, 'COMMON', TRUE, 'default-merchant'),
    ('badge-streak-30', 'Monthly Master', 'Maintain a 30-day ride streak', 'STREAK', 'https://cdn.nammayatri.in/badges/monthly-master.png', 'STREAK_DAYS', 30, 500, 250, 'EPIC', TRUE, 'default-merchant'),
    ('badge-referral-10', 'Community Builder', 'Refer 10 drivers', 'SPECIAL', 'https://cdn.nammayatri.in/badges/community-builder.png', 'REFERRALS', 10, 300, 150, 'RARE', TRUE, 'default-merchant');
