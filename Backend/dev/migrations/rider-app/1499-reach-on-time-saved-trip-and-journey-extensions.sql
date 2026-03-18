-- Reach on Time feature: SavedTrip table and Journey extensions

-- SavedTrip table for recurring/saved trip planning
CREATE TABLE atlas_app.saved_trip (
    id CHARACTER VARYING(36) PRIMARY KEY,
    rider_id CHARACTER VARYING(36) NOT NULL REFERENCES atlas_app.person(id),
    name TEXT NOT NULL,
    origin_lat DOUBLE PRECISION NOT NULL,
    origin_lon DOUBLE PRECISION NOT NULL,
    origin_address TEXT,
    destination_lat DOUBLE PRECISION NOT NULL,
    destination_lon DOUBLE PRECISION NOT NULL,
    destination_address TEXT,
    time_mode CHARACTER VARYING(20) NOT NULL DEFAULT 'LeaveNow',
    target_time TIMESTAMP WITH TIME ZONE,
    target_time_of_day_seconds INT,
    buffer_minutes INT NOT NULL DEFAULT 10,
    recurrence CHARACTER VARYING(20) NOT NULL DEFAULT 'NoRecurrence',
    custom_days TEXT,
    notify_before_minutes INT NOT NULL DEFAULT 5,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    last_computed_departure TIMESTAMP WITH TIME ZONE,
    last_notified_at TIMESTAMP WITH TIME ZONE,
    merchant_id CHARACTER VARYING(36) NOT NULL,
    merchant_operating_city_id CHARACTER VARYING(36) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_saved_trip_rider ON atlas_app.saved_trip(rider_id);
CREATE INDEX idx_saved_trip_active_recurring ON atlas_app.saved_trip(is_active, recurrence)
    WHERE is_active = TRUE AND recurrence != 'NoRecurrence';

-- Journey table extensions for time-aware planning
ALTER TABLE atlas_app.journey ADD COLUMN IF NOT EXISTS time_mode CHARACTER VARYING(20) DEFAULT 'LeaveNow';
ALTER TABLE atlas_app.journey ADD COLUMN IF NOT EXISTS target_arrival_time TIMESTAMP WITH TIME ZONE;
ALTER TABLE atlas_app.journey ADD COLUMN IF NOT EXISTS target_departure_time TIMESTAMP WITH TIME ZONE;
ALTER TABLE atlas_app.journey ADD COLUMN IF NOT EXISTS buffer_minutes INT DEFAULT 0;
ALTER TABLE atlas_app.journey ADD COLUMN IF NOT EXISTS risk_level CHARACTER VARYING(20);
ALTER TABLE atlas_app.journey ADD COLUMN IF NOT EXISTS recommended_departure TIMESTAMP WITH TIME ZONE;

-- Search request extensions for time constraints
ALTER TABLE atlas_app.search_request ADD COLUMN IF NOT EXISTS time_mode CHARACTER VARYING(20) DEFAULT 'LeaveNow';
ALTER TABLE atlas_app.search_request ADD COLUMN IF NOT EXISTS target_arrival_time TIMESTAMP WITH TIME ZONE;
ALTER TABLE atlas_app.search_request ADD COLUMN IF NOT EXISTS target_departure_time TIMESTAMP WITH TIME ZONE;
ALTER TABLE atlas_app.search_request ADD COLUMN IF NOT EXISTS buffer_minutes INT;
