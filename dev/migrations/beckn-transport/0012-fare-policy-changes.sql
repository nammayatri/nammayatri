
CREATE TABLE atlas_transporter.fare_policy_per_extra_km_rate (
    id character(36) PRIMARY KEY NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    organization_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id) ON DELETE CASCADE,
    distance_range_start double precision NOT NULL,
    fare double precision NOT NULL,
    CONSTRAINT fare_policy_extra_km_rate_unique_extra_distance_range_start UNIQUE (vehicle_variant, organization_id, distance_range_start)
);

INSERT INTO atlas_transporter.fare_policy_per_extra_km_rate (
    SELECT T1.id, 
           T1.vehicle_variant,
           T1.organization_id,
           T1.base_distance,
           T1.per_extra_km_rate
    FROM atlas_transporter.fare_policy AS T1);

ALTER TABLE atlas_transporter.fare_policy DROP COLUMN per_extra_km_rate;
ALTER TABLE atlas_transporter.fare_policy DROP COLUMN base_distance;

CREATE FUNCTION atlas_transporter.uuid_generate_v4() RETURNS character (36)
    AS 'SELECT uuid_in(overlay(overlay(md5(random()::text || '':'' || clock_timestamp()::text) placing ''4'' from 13) placing to_hex(floor(random()*(11-8+1) + 8)::int)::text from 17)::cstring)::character (36);'
    LANGUAGE SQL
    IMMUTABLE;

CREATE OR REPLACE FUNCTION atlas_transporter.create_fare_policy_on_org_creation() RETURNS TRIGGER AS $create_fare_policy_on_org_creation$
    BEGIN
        INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, night_shift_start, night_shift_end, night_shift_rate) VALUES
            (atlas_transporter.uuid_generate_v4(), 'SUV', NEW.id, 120.0, NULL, NULL, 1.0),
            (atlas_transporter.uuid_generate_v4(), 'SEDAN', NEW.id, 120.0, NULL, NULL, 1.0),
            (atlas_transporter.uuid_generate_v4(), 'HATCHBACK', NEW.id, 120.0, NULL, NULL, 1.0);
        INSERT INTO atlas_transporter.fare_policy_per_extra_km_rate VALUES 
            (atlas_transporter.uuid_generate_v4(), 'SUV', NEW.id, 5000, 12),
            (atlas_transporter.uuid_generate_v4(), 'SEDAN', NEW.id, 5000, 12),
            (atlas_transporter.uuid_generate_v4(), 'HATCHBACK', NEW.id, 5000, 12);
        RETURN NULL;
    END;
$create_fare_policy_on_org_creation$ LANGUAGE plpgsql;

CREATE TRIGGER create_fare_policy_on_org_creation AFTER INSERT ON atlas_transporter.organization
    FOR EACH ROW EXECUTE FUNCTION atlas_transporter.create_fare_policy_on_org_creation();

ALTER TABLE atlas_transporter.fare_policy
   ADD  CONSTRAINT fare_policy_org_id_fkey FOREIGN KEY (organization_id)
      REFERENCES atlas_transporter.organization (id) on delete cascade;