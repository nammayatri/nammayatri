ALTER TABLE atlas_transporter.ride DROP CONSTRAINT IF EXISTS ride_driver_id_fkey;
ALTER TABLE atlas_transporter.ride DROP CONSTRAINT IF EXISTS ride_vehicle_id_fkey;

CREATE TABLE IF NOT EXISTS atlas_transporter.ride_details (
    id character(36) PRIMARY KEY NOT NULL,
    driver_name varchar (255),
    driver_number_encrypted varchar (255),
    driver_number_hash bytea,
    driver_country_code varchar (255),
    vehicle_number varchar (255) NOT NULL,
    vehicle_color varchar (255),
    vehicle_variant varchar (255),
    vehicle_model varchar (255),
    vehicle_class varchar (255)
);

INSERT INTO atlas_transporter.ride_details
  SELECT r.id,
         COALESCE (p.first_name, '[Driver deleted]'),
         p.mobile_number_encrypted,
         p.mobile_number_hash,
         p.mobile_country_code,
         COALESCE (v.registration_no, '[Vehicle deleted]'),
         v.color,
         v.variant,
         v.model,
         v.vehicle_class

    FROM atlas_transporter.ride AS r
    LEFT JOIN atlas_transporter.person AS p ON r.driver_id = p.id
    LEFT JOIN atlas_transporter.vehicle AS v ON r.driver_id = v.driver_id
ON CONFLICT DO NOTHING;