-- Create driver_directory_profile table for MSIL Driver Directory feature
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_directory_profile (
    driver_id              VARCHAR(36) PRIMARY KEY REFERENCES atlas_driver_offer_bpp.person(id),
    is_listed              BOOLEAN NOT NULL DEFAULT FALSE,
    listed_at              TIMESTAMP WITH TIME ZONE,
    preferred_vehicle_type VARCHAR(50),
    preferred_cities       TEXT[] DEFAULT '{}',
    years_of_experience    INT,
    bio                    TEXT,
    show_phone_number      BOOLEAN NOT NULL DEFAULT FALSE,
    show_rating            BOOLEAN NOT NULL DEFAULT TRUE,
    show_total_rides       BOOLEAN NOT NULL DEFAULT TRUE,
    show_vehicle_info      BOOLEAN NOT NULL DEFAULT TRUE,
    availability_status    VARCHAR(20) DEFAULT 'AVAILABLE',
    merchant_id            VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    created_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at             TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_driver_directory_listed ON atlas_driver_offer_bpp.driver_directory_profile(is_listed, merchant_operating_city_id);
CREATE INDEX IF NOT EXISTS idx_driver_directory_city ON atlas_driver_offer_bpp.driver_directory_profile(merchant_operating_city_id, availability_status);
