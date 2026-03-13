-- Corporate Commute driver-side tables
-- These tables track driver-corporate assignments and corporate ride assignments on the BPP side

-- =========================
-- corporate_driver_assignment
-- =========================
-- Maps drivers to corporate shifts/routes they are assigned to serve
CREATE TABLE atlas_driver_offer_bpp.corporate_driver_assignment (
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL,
    corporate_entity_id character(36) NOT NULL,
    shift_id character(36),
    route_id character(36),
    assignment_type text NOT NULL,
    effective_from timestamp with time zone NOT NULL,
    effective_to timestamp with time zone,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_driver_assignment_driver_id ON atlas_driver_offer_bpp.corporate_driver_assignment USING btree (driver_id);
CREATE INDEX idx_corporate_driver_assignment_shift_id ON atlas_driver_offer_bpp.corporate_driver_assignment USING btree (shift_id);
CREATE INDEX idx_corporate_driver_assignment_status ON atlas_driver_offer_bpp.corporate_driver_assignment USING btree (status);

-- =========================
-- corporate_ride_assignment
-- =========================
-- Individual ride assignments for corporate commute trips on the driver side
CREATE TABLE atlas_driver_offer_bpp.corporate_ride_assignment (
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL,
    roster_id character(36) NOT NULL,
    booking_id character(36),
    ride_id character(36),
    corporate_entity_id character(36) NOT NULL,
    shift_id character(36),
    route_id character(36),
    scheduled_pickup_time timestamp with time zone NOT NULL,
    actual_pickup_time timestamp with time zone,
    actual_drop_time timestamp with time zone,
    status text NOT NULL,
    fare_amount double precision,
    currency text,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_ride_assignment_driver_id ON atlas_driver_offer_bpp.corporate_ride_assignment USING btree (driver_id);
CREATE INDEX idx_corporate_ride_assignment_roster_id ON atlas_driver_offer_bpp.corporate_ride_assignment USING btree (roster_id);
CREATE INDEX idx_corporate_ride_assignment_status ON atlas_driver_offer_bpp.corporate_ride_assignment USING btree (status);
CREATE INDEX idx_corporate_ride_assignment_scheduled_pickup_time ON atlas_driver_offer_bpp.corporate_ride_assignment USING btree (scheduled_pickup_time);
