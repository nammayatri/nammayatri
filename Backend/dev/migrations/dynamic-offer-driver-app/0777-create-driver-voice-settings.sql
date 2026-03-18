-- AI Voice Update feature: Driver voice notification settings
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_voice_settings (
    driver_id              VARCHAR(36) PRIMARY KEY REFERENCES atlas_driver_offer_bpp.person(id),
    is_enabled             BOOLEAN NOT NULL DEFAULT FALSE,
    language               VARCHAR(10) NOT NULL DEFAULT 'en',
    volume_level           INT NOT NULL DEFAULT 80,
    ride_request_enabled   BOOLEAN NOT NULL DEFAULT TRUE,
    navigation_enabled     BOOLEAN NOT NULL DEFAULT FALSE,
    earnings_enabled       BOOLEAN NOT NULL DEFAULT TRUE,
    safety_alerts_enabled  BOOLEAN NOT NULL DEFAULT TRUE,
    contextual_updates_enabled BOOLEAN NOT NULL DEFAULT FALSE,
    merchant_id            VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    created_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);
