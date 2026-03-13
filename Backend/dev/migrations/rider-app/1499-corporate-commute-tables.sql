-- Corporate Commute feature tables

-- =========================
-- corporate_entity
-- =========================
CREATE TABLE atlas_app.corporate_entity (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL,
    merchant_operating_city_id character(36) NOT NULL,
    partner_org_id character(36),
    name text NOT NULL,
    registered_name text NOT NULL,
    gstin text,
    industry text NOT NULL,
    contact_person_name text NOT NULL,
    contact_email text NOT NULL,
    contact_phone text NOT NULL,
    billing_address text NOT NULL,
    billing_model text NOT NULL,
    billing_cycle_type text NOT NULL,
    credit_limit double precision NOT NULL,
    currency text NOT NULL,
    status text NOT NULL,
    contract_start_date timestamp with time zone NOT NULL,
    contract_end_date timestamp with time zone,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_entity_merchant_id ON atlas_app.corporate_entity USING btree (merchant_id);
CREATE INDEX idx_corporate_entity_partner_org_id ON atlas_app.corporate_entity USING btree (partner_org_id);
CREATE INDEX idx_corporate_entity_status ON atlas_app.corporate_entity USING btree (status);

-- =========================
-- corporate_employee
-- =========================
CREATE TABLE atlas_app.corporate_employee (
    id character(36) NOT NULL PRIMARY KEY,
    corporate_entity_id character(36) NOT NULL REFERENCES atlas_app.corporate_entity(id),
    person_id character(36),
    employee_code text NOT NULL,
    name text NOT NULL,
    email text NOT NULL,
    phone text NOT NULL,
    department text NOT NULL,
    cost_center text,
    gender text NOT NULL,
    office_location_id text,
    default_pickup_lat double precision NOT NULL,
    default_pickup_lon double precision NOT NULL,
    default_pickup_address text NOT NULL,
    reporting_manager_email text,
    status text NOT NULL,
    linked_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_employee_corporate_entity_id ON atlas_app.corporate_employee USING btree (corporate_entity_id);
CREATE INDEX idx_corporate_employee_person_id ON atlas_app.corporate_employee USING btree (person_id);
CREATE INDEX idx_corporate_employee_phone ON atlas_app.corporate_employee USING btree (phone);
CREATE INDEX idx_corporate_employee_status ON atlas_app.corporate_employee USING btree (status);

-- =========================
-- corporate_shift
-- =========================
CREATE TABLE atlas_app.corporate_shift (
    id character(36) NOT NULL PRIMARY KEY,
    corporate_entity_id character(36) NOT NULL REFERENCES atlas_app.corporate_entity(id),
    merchant_operating_city_id character(36) NOT NULL,
    name text NOT NULL,
    pickup_window_start time without time zone NOT NULL,
    pickup_window_end time without time zone NOT NULL,
    drop_window_start time without time zone NOT NULL,
    drop_window_end time without time zone NOT NULL,
    active_days text NOT NULL,
    is_night_shift boolean NOT NULL DEFAULT false,
    max_occupancy integer NOT NULL,
    allowed_vehicle_tiers text NOT NULL,
    confirmation_deadline_minutes integer NOT NULL,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_shift_corporate_entity_id ON atlas_app.corporate_shift USING btree (corporate_entity_id);
CREATE INDEX idx_corporate_shift_status ON atlas_app.corporate_shift USING btree (status);

-- =========================
-- corporate_route
-- =========================
CREATE TABLE atlas_app.corporate_route (
    id character(36) NOT NULL PRIMARY KEY,
    corporate_entity_id character(36) NOT NULL REFERENCES atlas_app.corporate_entity(id),
    shift_id character(36) NOT NULL REFERENCES atlas_app.corporate_shift(id),
    route_code text NOT NULL,
    direction text NOT NULL,
    estimated_duration_minutes integer NOT NULL,
    estimated_distance_meters integer NOT NULL,
    vehicle_tier text NOT NULL,
    max_capacity integer NOT NULL,
    polyline text,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_route_corporate_entity_id ON atlas_app.corporate_route USING btree (corporate_entity_id);
CREATE INDEX idx_corporate_route_shift_id ON atlas_app.corporate_route USING btree (shift_id);
CREATE INDEX idx_corporate_route_status ON atlas_app.corporate_route USING btree (status);

-- =========================
-- corporate_route_stop
-- =========================
CREATE TABLE atlas_app.corporate_route_stop (
    id character(36) NOT NULL PRIMARY KEY,
    route_id character(36) NOT NULL REFERENCES atlas_app.corporate_route(id),
    sequence integer NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    address text NOT NULL,
    estimated_arrival_offset integer NOT NULL,
    created_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_route_stop_route_id ON atlas_app.corporate_route_stop USING btree (route_id);

-- =========================
-- corporate_route_stop_assignment
-- =========================
CREATE TABLE atlas_app.corporate_route_stop_assignment (
    id character(36) NOT NULL PRIMARY KEY,
    route_stop_id character(36) NOT NULL REFERENCES atlas_app.corporate_route_stop(id),
    employee_id character(36) NOT NULL REFERENCES atlas_app.corporate_employee(id),
    effective_from timestamp with time zone NOT NULL,
    effective_to timestamp with time zone,
    created_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_route_stop_assignment_route_stop_id ON atlas_app.corporate_route_stop_assignment USING btree (route_stop_id);
CREATE INDEX idx_corporate_route_stop_assignment_employee_id ON atlas_app.corporate_route_stop_assignment USING btree (employee_id);

-- =========================
-- corporate_roster
-- =========================
CREATE TABLE atlas_app.corporate_roster (
    id character(36) NOT NULL PRIMARY KEY,
    corporate_entity_id character(36) NOT NULL REFERENCES atlas_app.corporate_entity(id),
    corporate_employee_id character(36) NOT NULL REFERENCES atlas_app.corporate_employee(id),
    corporate_shift_id character(36) NOT NULL REFERENCES atlas_app.corporate_shift(id),
    corporate_route_id character(36),
    corporate_route_stop_id character(36),
    roster_date date NOT NULL,
    attendance_status text NOT NULL,
    booking_id character(36),
    confirmed_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_roster_corporate_entity_id ON atlas_app.corporate_roster USING btree (corporate_entity_id);
CREATE INDEX idx_corporate_roster_corporate_employee_id ON atlas_app.corporate_roster USING btree (corporate_employee_id);
CREATE INDEX idx_corporate_roster_corporate_shift_id ON atlas_app.corporate_roster USING btree (corporate_shift_id);
CREATE INDEX idx_corporate_roster_roster_date ON atlas_app.corporate_roster USING btree (roster_date);
CREATE INDEX idx_corporate_roster_shift_date ON atlas_app.corporate_roster USING btree (corporate_shift_id, roster_date);

-- =========================
-- corporate_policy
-- =========================
CREATE TABLE atlas_app.corporate_policy (
    id character(36) NOT NULL PRIMARY KEY,
    corporate_entity_id character(36) NOT NULL REFERENCES atlas_app.corporate_entity(id),
    name text NOT NULL,
    policy_type text NOT NULL,
    max_fare_per_trip double precision,
    max_monthly_budget_per_employee double precision,
    allowed_service_tiers text NOT NULL,
    requires_approval boolean NOT NULL DEFAULT false,
    night_shift_safety_enabled boolean NOT NULL DEFAULT false,
    women_safety_rules_enabled boolean NOT NULL DEFAULT false,
    surge_cap double precision,
    is_active boolean NOT NULL DEFAULT true,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_policy_corporate_entity_id ON atlas_app.corporate_policy USING btree (corporate_entity_id);
CREATE INDEX idx_corporate_policy_is_active ON atlas_app.corporate_policy USING btree (is_active);

-- =========================
-- corporate_wallet
-- =========================
CREATE TABLE atlas_app.corporate_wallet (
    id character(36) NOT NULL PRIMARY KEY,
    corporate_entity_id character(36) NOT NULL REFERENCES atlas_app.corporate_entity(id),
    balance double precision NOT NULL DEFAULT 0,
    currency text NOT NULL,
    status text NOT NULL,
    grace_started_at timestamp with time zone,
    last_top_up_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_wallet_corporate_entity_id ON atlas_app.corporate_wallet USING btree (corporate_entity_id);

-- =========================
-- corporate_invoice
-- =========================
CREATE TABLE atlas_app.corporate_invoice (
    id character(36) NOT NULL PRIMARY KEY,
    corporate_entity_id character(36) NOT NULL REFERENCES atlas_app.corporate_entity(id),
    invoice_number text NOT NULL,
    period_start timestamp with time zone NOT NULL,
    period_end timestamp with time zone NOT NULL,
    total_trips integer NOT NULL DEFAULT 0,
    base_amount double precision NOT NULL,
    cgst_rate double precision,
    cgst_amount double precision,
    sgst_rate double precision,
    sgst_amount double precision,
    igst_rate double precision,
    igst_amount double precision,
    total_tax_amount double precision NOT NULL,
    net_amount double precision NOT NULL,
    currency text NOT NULL,
    sac_code text NOT NULL,
    place_of_supply text NOT NULL,
    supplier_gstin text NOT NULL,
    recipient_gstin text,
    e_invoice_irn text,
    status text NOT NULL,
    pdf_url text,
    generated_at timestamp with time zone NOT NULL,
    paid_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL DEFAULT now(),
    updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX idx_corporate_invoice_corporate_entity_id ON atlas_app.corporate_invoice USING btree (corporate_entity_id);
CREATE UNIQUE INDEX idx_corporate_invoice_invoice_number ON atlas_app.corporate_invoice USING btree (invoice_number);
CREATE INDEX idx_corporate_invoice_status ON atlas_app.corporate_invoice USING btree (status);
CREATE INDEX idx_corporate_invoice_generated_at_brin ON atlas_app.corporate_invoice USING brin (generated_at);
