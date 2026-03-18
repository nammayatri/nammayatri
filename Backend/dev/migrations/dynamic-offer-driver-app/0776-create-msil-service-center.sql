-- Create MSIL service center table for storing workshop and bodyshop data
CREATE TABLE atlas_driver_offer_bpp.msil_service_center (
    id                          CHARACTER VARYING(36) PRIMARY KEY,
    name                        TEXT NOT NULL,
    center_type                 CHARACTER VARYING(20) NOT NULL,
    address                     TEXT NOT NULL,
    lat                         DOUBLE PRECISION NOT NULL,
    lon                         DOUBLE PRECISION NOT NULL,
    point                       GEOGRAPHY(POINT, 4326),
    services_offered            TEXT[] NOT NULL DEFAULT '{}',
    brand                       TEXT DEFAULT 'MSIL',
    dealer_code                 TEXT,
    phone_number                TEXT,
    email                       TEXT,
    operating_hours_start       TEXT,
    operating_hours_end         TEXT,
    is_open_sunday              BOOLEAN NOT NULL DEFAULT FALSE,
    is_active                   BOOLEAN NOT NULL DEFAULT TRUE,
    city                        TEXT NOT NULL,
    state                       TEXT,
    pincode                     TEXT,
    merchant_id                 CHARACTER VARYING(36) NOT NULL,
    merchant_operating_city_id  CHARACTER VARYING(36) NOT NULL,
    created_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Geospatial index for nearby queries using PostGIS
CREATE INDEX idx_msil_service_center_geo ON atlas_driver_offer_bpp.msil_service_center USING GIST(point);

-- Composite index for filtered queries by city, type, and active status
CREATE INDEX idx_msil_service_center_city_type ON atlas_driver_offer_bpp.msil_service_center(city, center_type, is_active);

-- GIN index for array-based service type filtering
CREATE INDEX idx_msil_service_center_services ON atlas_driver_offer_bpp.msil_service_center USING GIN(services_offered);

-- Index on merchant operating city for admin list queries
CREATE INDEX idx_msil_service_center_moc ON atlas_driver_offer_bpp.msil_service_center(merchant_operating_city_id, is_active);

-- Trigger to auto-populate PostGIS point column from lat/lon
CREATE OR REPLACE FUNCTION atlas_driver_offer_bpp.msil_service_center_update_point()
RETURNS TRIGGER AS $$
BEGIN
    NEW.point = ST_SetSRID(ST_MakePoint(NEW.lon, NEW.lat), 4326)::GEOGRAPHY;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_msil_service_center_point
    BEFORE INSERT OR UPDATE OF lat, lon ON atlas_driver_offer_bpp.msil_service_center
    FOR EACH ROW
    EXECUTE FUNCTION atlas_driver_offer_bpp.msil_service_center_update_point();
