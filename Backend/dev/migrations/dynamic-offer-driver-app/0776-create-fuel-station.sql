-- Create fuel_station table for the Fuel Station Locator feature.
-- Stores fuel station data (petrol, diesel, CNG, EV) for driver-facing map view.
-- Uses PostGIS GEOGRAPHY type for efficient geospatial queries via ST_DWithin.

CREATE TABLE atlas_driver_offer_bpp.fuel_station (
    id                          VARCHAR(36) PRIMARY KEY,
    name                        TEXT NOT NULL,
    address                     TEXT NOT NULL,
    lat                         DOUBLE PRECISION NOT NULL,
    lon                         DOUBLE PRECISION NOT NULL,
    point                       GEOGRAPHY(POINT, 4326),
    fuel_types                  TEXT[] NOT NULL DEFAULT '{}',
    brand                       TEXT,
    operating_hours_start       TEXT,
    operating_hours_end         TEXT,
    is_open_24h                 BOOLEAN NOT NULL DEFAULT FALSE,
    phone_number                TEXT,
    is_active                   BOOLEAN NOT NULL DEFAULT TRUE,
    city                        TEXT NOT NULL,
    merchant_id                 VARCHAR(36) NOT NULL,
    merchant_operating_city_id  VARCHAR(36) NOT NULL,
    created_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Geospatial index for ST_DWithin queries (find stations within radius)
CREATE INDEX idx_fuel_station_geo ON atlas_driver_offer_bpp.fuel_station USING GIST(point);

-- Composite index for city + active status filtering
CREATE INDEX idx_fuel_station_city ON atlas_driver_offer_bpp.fuel_station(city, is_active);

-- GIN index for fuel_types array containment queries
CREATE INDEX idx_fuel_station_fuel_type ON atlas_driver_offer_bpp.fuel_station USING GIN(fuel_types);

-- Index for merchant operating city lookups
CREATE INDEX idx_fuel_station_merchant_op_city ON atlas_driver_offer_bpp.fuel_station(merchant_operating_city_id, is_active);

-- Trigger to auto-populate PostGIS point column from lat/lon on insert/update
CREATE OR REPLACE FUNCTION atlas_driver_offer_bpp.fuel_station_update_point()
RETURNS TRIGGER AS $$
BEGIN
    NEW.point = ST_SetSRID(ST_MakePoint(NEW.lon, NEW.lat), 4326)::GEOGRAPHY;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER fuel_station_point_trigger
    BEFORE INSERT OR UPDATE OF lat, lon ON atlas_driver_offer_bpp.fuel_station
    FOR EACH ROW
    EXECUTE FUNCTION atlas_driver_offer_bpp.fuel_station_update_point();
