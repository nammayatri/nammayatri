CREATE TABLE atlas_driver_offer_bpp.driver_fee(
    `id` String,
    `merchant_id` String,
    `driver_id` Nullable (String),
    `status` Nullable (String),
    `num_rides` Nullable(Int64),
    `special_zone_amount` Nullable(Float64),
    `platform_fee` Nullable(Float64),
    `cgst` Nullable(Float64),
    `sgst` Nullable(Float64),
    `govt_charges` Nullable(Int64),
    `collected_at` DateTime DEFAULT now(),
    `collected_by` Nullable(String)
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.ride_details (
    `id` String,
    `vehicle_number` Nullable(String),
    `fleet_owner_id` Nullable(String)
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.ride (
    `id` String,
    `status` Nullable(String),
    `fare` Nullable(Int),
    `driver_id` Nullable(String)
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.govt_data(
    `id` String,
    `merchant_operating_city_id` String,
    `owner_serial_number` Nullable(String),
    `registration_number` Nullable(String),
    `manufacturer_model` Nullable(String),
    `permit_validity_from` Nullable(String),
    `permit_validity_upto` Nullable(String),
    `manufacturer` Nullable(String),
    `body_type` Nullable(String),
    `number_of_cylinder` Nullable(Int),
    `fuel_type` Nullable(String),
    `seating_capacity` Nullable(Int),
    `from_date` Nullable(String),
    `to_date` Nullable(String),
    `created_at`  DateTime DEFAULT now()
) ENGINE = MergeTree() PRIMARY KEY (id);