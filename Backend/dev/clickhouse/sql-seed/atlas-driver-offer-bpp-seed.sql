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
    `collected_by` Nullable(String),
    `updated_at` DateTime DEFAULT now()
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.ride_details (
    `id` String,
    `vehicle_number` Nullable(String),
    `fleet_owner_id` Nullable(String),
    `created_at` DateTime DEFAULT now()
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.ride (
    `id` String,
    `status` Nullable(String),
    `fare` Nullable(Int),
    `driver_id` Nullable(String),
    `chargeable_distance` Nullable(Int),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (id);

CREATE TABLE atlas_driver_offer_bpp.daily_stats (
    `id` String,
    `driver_id` String,
    `merchant_local_date` DateTime DEFAULT now(),
    `total_earnings` Float64,
    `total_distance` Float64,
    `num_rides` Int64,
    `cancellation_charges` Float64,
    `bonus_earnings` Float64,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (merchant_local_date, driver_id, id);

CREATE TABLE atlas_driver_offer_bpp.driver_information (
    `driver_id` String,
    `driver_flow_status` Nullable(String),
    `enabled` Boolean,
    `enabled_at` Nullable(DateTime),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (driver_id);

create table atlas_driver_offer_bpp.driver_operator_association (
    `id` String,
    `driver_id` String,
    `operator_id` String,
    `is_active` Boolean,
    `associated_on` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (operator_id, driver_id);

create table atlas_driver_offer_bpp.fleet_driver_association (
    `id` String,
    `driver_id` String,
    `fleet_owner_id` String,
    `is_active` Boolean,
    `associated_on` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (fleet_owner_id, driver_id);

create table atlas_driver_offer_bpp.fleet_operator_association (
    `id` String,
    `fleet_owner_id` String,
    `operator_id` String,
    `is_active` Boolean,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (operator_id, fleet_owner_id);

create table atlas_driver_offer_bpp.driver_stats (
    `driver_id` String,
    `total_rides` Int64,
    `total_rating_score` Nullable(Int64),
    `valid_driver_cancellation_tag_count` Int64,
    `acceptation_count` Nullable(Int64),
    `total_request_count` Nullable(Int64),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (driver_id);