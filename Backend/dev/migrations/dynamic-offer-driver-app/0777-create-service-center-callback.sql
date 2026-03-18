-- Create service center callback request table for tracking driver callback requests
CREATE TABLE atlas_driver_offer_bpp.service_center_callback_request (
    id                          CHARACTER VARYING(36) PRIMARY KEY,
    driver_id                   CHARACTER VARYING(36) NOT NULL,
    service_center_id           CHARACTER VARYING(36) NOT NULL,
    vehicle_number              TEXT,
    issue_description           TEXT,
    preferred_date              TEXT,
    preferred_time_slot         CHARACTER VARYING(20),
    status                      CHARACTER VARYING(20) NOT NULL DEFAULT 'PENDING',
    admin_notes                 TEXT,
    merchant_operating_city_id  CHARACTER VARYING(36) NOT NULL,
    created_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Index for looking up callbacks by driver
CREATE INDEX idx_callback_request_driver ON atlas_driver_offer_bpp.service_center_callback_request(driver_id, created_at DESC);

-- Index for looking up callbacks by service center and status
CREATE INDEX idx_callback_request_center ON atlas_driver_offer_bpp.service_center_callback_request(service_center_id, status);

-- Index for admin status-based filtering
CREATE INDEX idx_callback_request_status ON atlas_driver_offer_bpp.service_center_callback_request(status, created_at DESC);

-- Index for city-based admin queries
CREATE INDEX idx_callback_request_city ON atlas_driver_offer_bpp.service_center_callback_request(merchant_operating_city_id, status);
