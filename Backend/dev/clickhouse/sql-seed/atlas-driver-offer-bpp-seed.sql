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
    `merchant_local_date` Date DEFAULT today(),
    `total_earnings` Float64,
    `total_distance` Int64,
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

CREATE TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats (
    `fleet_operator_id` String,
    `fleet_driver_id` String,
    `merchant_local_date` Date DEFAULT today(),
    `rejected_request_count` Nullable(Int64),
    `pulled_request_count` Nullable(Int64),
    `acceptation_request_count` Nullable(Int64),
    `total_request_count` Nullable(Int64),
    `customer_cancellation_count` Nullable(Int64),
    `driver_cancellation_count` Nullable(Int64),
    `total_distance` Nullable(Int64),
    `total_completed_rides` Nullable(Int64),
    `online_total_earning` Nullable(Float64),
    `cash_total_earning` Nullable(Float64),
    `cash_platform_fees` Nullable(Float64),
    `online_platform_fees` Nullable(Float64),
    `online_duration` Nullable(Int64),
    `total_rating_score` Nullable(Int64),
    `ride_duration` Nullable(Int64),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (merchant_local_date, fleet_operator_id);

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

create table atlas_driver_offer_bpp.vehicle (
    `driver_id` String,
    `version` DateTime DEFAULT now(),
    `is_deleted` UInt8 DEFAULT 0
) ENGINE = ReplacingMergeTree(version, is_deleted)
ORDER BY (driver_id);

CREATE TABLE atlas_driver_offer_bpp.search_request_for_driver
(
    `id` String,
    `driver_id` String,
    `from_loc_geohash` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `trip_estimated_distance` Nullable(Int32),
    `trip_estimated_duration` Nullable(Int32),
    `mode` Nullable(String),
    `response` Nullable(String),
    `vehicle_service_tier` Nullable(String),
    `vehicle_category` Nullable(String),
    `search_request_id` String,
    `search_try_id` String,
    `created_at` DateTime DEFAULT now()
)
ENGINE = ReplacingMergeTree()
ORDER BY (created_at, driver_id, id);

create table atlas_driver_offer_bpp.person (
    `id` String,
    `first_name` String,
    `middle_name` Nullable(String),
    `last_name` Nullable(String),
    `mobile_number_hash` Nullable(String),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (id);