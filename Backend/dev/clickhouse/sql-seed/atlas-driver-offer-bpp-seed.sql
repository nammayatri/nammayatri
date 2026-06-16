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
    `default_service_tier_name` Nullable(String),
    `created_at` DateTime DEFAULT now()
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.ride (
    `id` String,
    `status` Nullable(String),
    `fare` Nullable(Int),
    `driver_id` Nullable(String),
    `chargeable_distance` Nullable(Int),
    `trip_start_time` Nullable(DateTime),
    `trip_end_time` Nullable(DateTime),
    `driver_arrival_time` Nullable(DateTime),
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
    `mobile_number` Nullable(String),
    `mobile_number_hash` Nullable(String),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (id);

create table atlas_driver_offer_bpp.vehicle_registration_certificate (
    `id` String,
    `unencrypted_certificate_number` Nullable(String),
    `vehicle_model` Nullable(String),
    `vehicle_manufacturer` Nullable(String),
    `docs_verification_status` Nullable(String),
    `fleet_owner_id` Nullable(String),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (id);

create table atlas_driver_offer_bpp.fleet_rc_daily_stats (
    `fleet_owner_id` String,
    `rc_id` String,
    `merchant_local_date` Date DEFAULT today(),
    `total_completed_rides` Int32,
    `total_earnings` Int32,
    `ride_distance` Int32,
    `ride_duration` Int32,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (merchant_local_date, fleet_owner_id, rc_id);

CREATE TABLE atlas_driver_offer_bpp.fleet_owner_information (
    `fleet_owner_person_id` String,
    `fleet_name` Nullable(String),
    `docs_verification_status` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version)
ORDER BY (fleet_owner_person_id);

-- ============================================================================
-- Kafka -> ClickHouse pipeline
-- Topics are produced by dynamic-offer-driver-drainer (PUSH_TO_KAFKA=true) on
-- adob-sessionizer-<table_name_snake_case>. Each table below has a Kafka
-- staging engine table + a MATERIALIZED VIEW that drains it into the
-- destination ReplacingMergeTree table defined above.
-- ============================================================================

-- fleet_operator_association ------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_operator_association_queue (
    `id` String,
    `fleet_owner_id` String,
    `operator_id` String,
    `is_active` Bool,
    `updated_at` DateTime
) ENGINE = Kafka
SETTINGS
    kafka_broker_list = 'localhost:29092',
    kafka_topic_list  = 'adob-sessionizer-fleet_operator_association',
    kafka_group_name  = 'ch-atlas_driver_offer_bpp-fleet_operator_association',
    kafka_format      = 'JSONEachRow',
    kafka_num_consumers = 1,
    kafka_skip_broken_messages = 100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.fleet_operator_association_mv
TO atlas_driver_offer_bpp.fleet_operator_association AS
SELECT
    id,
    fleet_owner_id,
    operator_id,
    is_active,
    updated_at AS version
FROM atlas_driver_offer_bpp.fleet_operator_association_queue;

-- driver_fee ----------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_fee_queue (
    `id` String,
    `merchant_id` String,
    `driver_id` Nullable(String),
    `status` Nullable(String),
    `num_rides` Nullable(Int64),
    `special_zone_amount` Nullable(Float64),
    `platform_fee` Nullable(Float64),
    `cgst` Nullable(Float64),
    `sgst` Nullable(Float64),
    `govt_charges` Nullable(Int64),
    `collected_at` Nullable(DateTime),
    `collected_by` Nullable(String),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-driver_fee', kafka_group_name='ch-atlas_driver_offer_bpp-driver_fee', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.driver_fee_mv
TO atlas_driver_offer_bpp.driver_fee AS
SELECT id, merchant_id, driver_id, status, num_rides, special_zone_amount, platform_fee, cgst, sgst, govt_charges, collected_at, collected_by, updated_at
FROM atlas_driver_offer_bpp.driver_fee_queue;

-- ride_details --------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.ride_details_queue (
    `id` String,
    `vehicle_number` Nullable(String),
    `fleet_owner_id` Nullable(String),
    `default_service_tier_name` Nullable(String),
    `created_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-ride_details', kafka_group_name='ch-atlas_driver_offer_bpp-ride_details', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.ride_details_mv
TO atlas_driver_offer_bpp.ride_details AS
SELECT id, vehicle_number, fleet_owner_id, default_service_tier_name, created_at
FROM atlas_driver_offer_bpp.ride_details_queue;

-- ride ----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.ride_queue (
    `id` String,
    `status` Nullable(String),
    `fare` Nullable(Int32),
    `driver_id` Nullable(String),
    `chargeable_distance` Nullable(Int32),
    `trip_start_time` Nullable(DateTime),
    `trip_end_time` Nullable(DateTime),
    `driver_arrival_time` Nullable(DateTime),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-ride', kafka_group_name='ch-atlas_driver_offer_bpp-ride', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.ride_mv
TO atlas_driver_offer_bpp.ride AS
SELECT id, status, fare, driver_id, chargeable_distance, trip_start_time, trip_end_time, driver_arrival_time, created_at, updated_at, updated_at AS version
FROM atlas_driver_offer_bpp.ride_queue;

-- daily_stats ---------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.daily_stats_queue (
    `id` String,
    `driver_id` String,
    `merchant_local_date` Date,
    `total_earnings` Float64,
    `total_distance` Int64,
    `num_rides` Int64,
    `cancellation_charges` Float64,
    `bonus_earnings` Float64,
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-daily_stats', kafka_group_name='ch-atlas_driver_offer_bpp-daily_stats', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.daily_stats_mv
TO atlas_driver_offer_bpp.daily_stats AS
SELECT id, driver_id, merchant_local_date, total_earnings, total_distance, num_rides, cancellation_charges, bonus_earnings, updated_at AS version
FROM atlas_driver_offer_bpp.daily_stats_queue;

-- driver_information --------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_information_queue (
    `driver_id` String,
    `driver_flow_status` Nullable(String),
    `enabled` Boolean,
    `enabled_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-driver_information', kafka_group_name='ch-atlas_driver_offer_bpp-driver_information', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.driver_information_mv
TO atlas_driver_offer_bpp.driver_information AS
SELECT driver_id, driver_flow_status, enabled, enabled_at, updated_at AS version
FROM atlas_driver_offer_bpp.driver_information_queue;

-- fleet_operator_daily_stats ------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_operator_daily_stats_queue (
    `fleet_operator_id` String,
    `fleet_driver_id` String,
    `merchant_local_date` Date,
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
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-fleet_operator_daily_stats', kafka_group_name='ch-atlas_driver_offer_bpp-fleet_operator_daily_stats', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.fleet_operator_daily_stats_mv
TO atlas_driver_offer_bpp.fleet_operator_daily_stats AS
SELECT fleet_operator_id, fleet_driver_id, merchant_local_date, rejected_request_count, pulled_request_count, acceptation_request_count, total_request_count, customer_cancellation_count, driver_cancellation_count, total_distance, total_completed_rides, online_total_earning, cash_total_earning, cash_platform_fees, online_platform_fees, online_duration, total_rating_score, ride_duration, updated_at AS version
FROM atlas_driver_offer_bpp.fleet_operator_daily_stats_queue;

-- driver_operator_association -----------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_operator_association_queue (
    `id` String,
    `driver_id` String,
    `operator_id` String,
    `is_active` Boolean,
    `associated_on` DateTime,
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-driver_operator_association', kafka_group_name='ch-atlas_driver_offer_bpp-driver_operator_association', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.driver_operator_association_mv
TO atlas_driver_offer_bpp.driver_operator_association AS
SELECT id, driver_id, operator_id, is_active, associated_on, updated_at AS version
FROM atlas_driver_offer_bpp.driver_operator_association_queue;

-- fleet_driver_association --------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_driver_association_queue (
    `id` String,
    `driver_id` String,
    `fleet_owner_id` String,
    `is_active` Boolean,
    `associated_on` DateTime,
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-fleet_driver_association', kafka_group_name='ch-atlas_driver_offer_bpp-fleet_driver_association', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.fleet_driver_association_mv
TO atlas_driver_offer_bpp.fleet_driver_association AS
SELECT id, driver_id, fleet_owner_id, is_active, associated_on, updated_at AS version
FROM atlas_driver_offer_bpp.fleet_driver_association_queue;

-- driver_stats --------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_stats_queue (
    `driver_id` String,
    `total_rides` Int64,
    `total_rating_score` Nullable(Int64),
    `valid_driver_cancellation_tag_count` Int64,
    `acceptation_count` Nullable(Int64),
    `total_request_count` Nullable(Int64),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-driver_stats', kafka_group_name='ch-atlas_driver_offer_bpp-driver_stats', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.driver_stats_mv
TO atlas_driver_offer_bpp.driver_stats AS
SELECT driver_id, total_rides, total_rating_score, valid_driver_cancellation_tag_count, acceptation_count, total_request_count, updated_at AS version
FROM atlas_driver_offer_bpp.driver_stats_queue;

-- vehicle -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.vehicle_queue (
    `driver_id` String,
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-vehicle', kafka_group_name='ch-atlas_driver_offer_bpp-vehicle', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.vehicle_mv
TO atlas_driver_offer_bpp.vehicle AS
SELECT driver_id, updated_at AS version
FROM atlas_driver_offer_bpp.vehicle_queue;

-- search_request_for_driver -------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.search_request_for_driver_queue (
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
    `created_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-search_request_for_driver', kafka_group_name='ch-atlas_driver_offer_bpp-search_request_for_driver', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.search_request_for_driver_mv
TO atlas_driver_offer_bpp.search_request_for_driver AS
SELECT id, driver_id, from_loc_geohash, merchant_operating_city_id, trip_estimated_distance, trip_estimated_duration, mode, response, vehicle_service_tier, vehicle_category, search_request_id, search_try_id, created_at
FROM atlas_driver_offer_bpp.search_request_for_driver_queue;

-- person --------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.person_queue (
    `id` String,
    `first_name` String,
    `middle_name` Nullable(String),
    `last_name` Nullable(String),
    `mobile_number` Nullable(String),
    `mobile_number_hash` Nullable(String),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-person', kafka_group_name='ch-atlas_driver_offer_bpp-person', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.person_mv
TO atlas_driver_offer_bpp.person AS
SELECT id, first_name, middle_name, last_name, mobile_number, mobile_number_hash, updated_at AS version
FROM atlas_driver_offer_bpp.person_queue;

-- vehicle_registration_certificate ------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.vehicle_registration_certificate_queue (
    `id` String,
    `unencrypted_certificate_number` Nullable(String),
    `vehicle_model` Nullable(String),
    `vehicle_manufacturer` Nullable(String),
    `docs_verification_status` Nullable(String),
    `fleet_owner_id` Nullable(String),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-vehicle_registration_certificate', kafka_group_name='ch-atlas_driver_offer_bpp-vehicle_registration_certificate', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.vehicle_registration_certificate_mv
TO atlas_driver_offer_bpp.vehicle_registration_certificate AS
SELECT id, unencrypted_certificate_number, vehicle_model, vehicle_manufacturer, docs_verification_status, fleet_owner_id, updated_at AS version
FROM atlas_driver_offer_bpp.vehicle_registration_certificate_queue;

-- fleet_rc_daily_stats ------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_rc_daily_stats_queue (
    `fleet_owner_id` String,
    `rc_id` String,
    `merchant_local_date` Date,
    `total_completed_rides` Int32,
    `total_earnings` Int32,
    `ride_distance` Int32,
    `ride_duration` Int32,
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-fleet_rc_daily_stats', kafka_group_name='ch-atlas_driver_offer_bpp-fleet_rc_daily_stats', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.fleet_rc_daily_stats_mv
TO atlas_driver_offer_bpp.fleet_rc_daily_stats AS
SELECT fleet_owner_id, rc_id, merchant_local_date, total_completed_rides, total_earnings, ride_distance, ride_duration, updated_at AS version
FROM atlas_driver_offer_bpp.fleet_rc_daily_stats_queue;

-- fleet_owner_information ---------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_owner_information_queue (
    `fleet_owner_person_id` String,
    `fleet_name` Nullable(String),
    `docs_verification_status` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-fleet_owner_information', kafka_group_name='ch-atlas_driver_offer_bpp-fleet_owner_information', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.fleet_owner_information_mv
TO atlas_driver_offer_bpp.fleet_owner_information AS
SELECT fleet_owner_person_id, fleet_name, docs_verification_status, merchant_operating_city_id, updated_at AS version
FROM atlas_driver_offer_bpp.fleet_owner_information_queue;

-- ============================================================================
-- New destinations + Kafka + MV for tables not previously DDL'd
-- ============================================================================

-- booking -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.booking (
    `id` String,
    `status` Nullable(String),
    `bap_id` Nullable(String),
    `bap_country` Nullable(String),
    `bap_city` Nullable(String),
    `provider_id` Nullable(String),
    `vehicle_service_tier` Nullable(String),
    `trip_category` Nullable(String),
    `estimated_distance` Nullable(Int64),
    `estimated_duration` Nullable(Int64),
    `estimated_fare` Nullable(Float64),
    `from_loc_geohash` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` Nullable(DateTime),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.booking_queue (
    `id` String,
    `status` Nullable(String),
    `bap_id` Nullable(String),
    `bap_country` Nullable(String),
    `bap_city` Nullable(String),
    `provider_id` Nullable(String),
    `vehicle_service_tier` Nullable(String),
    `trip_category` Nullable(String),
    `estimated_distance` Nullable(Int64),
    `estimated_duration` Nullable(Int64),
    `estimated_fare` Nullable(Float64),
    `from_loc_geohash` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-booking', kafka_group_name='ch-atlas_driver_offer_bpp-booking', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.booking_mv
TO atlas_driver_offer_bpp.booking AS
SELECT id, status, bap_id, bap_country, bap_city, provider_id, vehicle_service_tier, trip_category, estimated_distance, estimated_duration, estimated_fare, from_loc_geohash, merchant_id, merchant_operating_city_id, created_at, updated_at, updated_at AS version
FROM atlas_driver_offer_bpp.booking_queue;

-- estimate ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.estimate (
    `id` String,
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `max_fare` Int64,
    `min_fare` Int64,
    `estimated_distance` Nullable(Int64),
    `estimated_duration` Nullable(Int64),
    `vehicle_service_tier` Nullable(String),
    `trip_category` Nullable(String),
    `from_loc_geohash` Nullable(String),
    `is_blocked_route` Nullable(Boolean),
    `is_scheduled` Nullable(Boolean),
    `created_at` Nullable(DateTime),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.estimate_queue (
    `id` String,
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `max_fare` Int64,
    `min_fare` Int64,
    `estimated_distance` Nullable(Int64),
    `estimated_duration` Nullable(Int64),
    `vehicle_service_tier` Nullable(String),
    `trip_category` Nullable(String),
    `from_loc_geohash` Nullable(String),
    `is_blocked_route` Nullable(Boolean),
    `is_scheduled` Nullable(Boolean),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-estimate', kafka_group_name='ch-atlas_driver_offer_bpp-estimate', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.estimate_mv
TO atlas_driver_offer_bpp.estimate AS
SELECT id, merchant_id, merchant_operating_city_id, max_fare, min_fare, estimated_distance, estimated_duration, vehicle_service_tier, trip_category, from_loc_geohash, is_blocked_route, is_scheduled, created_at, updated_at, updated_at AS version
FROM atlas_driver_offer_bpp.estimate_queue;

-- fleet_operator_stats ------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_operator_stats (
    `fleet_operator_id` String,
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `total_completed_rides` Nullable(Int64),
    `total_distance` Nullable(Float64),
    `total_earning` Nullable(Float64),
    `total_rating_score` Nullable(Int64),
    `total_rating_count` Nullable(Int64),
    `total_request_count` Nullable(Int64),
    `acceptation_request_count` Nullable(Int64),
    `customer_cancellation_count` Nullable(Int64),
    `driver_cancellation_count` Nullable(Int64),
    `driver_first_subscription` Nullable(Int64),
    `inspection_completed` Nullable(Int64),
    `created_at` Nullable(DateTime),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (fleet_operator_id);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fleet_operator_stats_queue (
    `fleet_operator_id` String,
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `total_completed_rides` Nullable(Int64),
    `total_distance` Nullable(Float64),
    `total_earning` Nullable(Float64),
    `total_rating_score` Nullable(Int64),
    `total_rating_count` Nullable(Int64),
    `total_request_count` Nullable(Int64),
    `acceptation_request_count` Nullable(Int64),
    `customer_cancellation_count` Nullable(Int64),
    `driver_cancellation_count` Nullable(Int64),
    `driver_first_subscription` Nullable(Int64),
    `inspection_completed` Nullable(Int64),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-fleet_operator_stats', kafka_group_name='ch-atlas_driver_offer_bpp-fleet_operator_stats', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.fleet_operator_stats_mv
TO atlas_driver_offer_bpp.fleet_operator_stats AS
SELECT fleet_operator_id, merchant_id, merchant_operating_city_id, total_completed_rides, total_distance, total_earning, total_rating_score, total_rating_count, total_request_count, acceptation_request_count, customer_cancellation_count, driver_cancellation_count, driver_first_subscription, inspection_completed, created_at, updated_at, updated_at AS version
FROM atlas_driver_offer_bpp.fleet_operator_stats_queue;

-- location ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.location (
    `id` String,
    `lat` Float64,
    `lon` Float64,
    `area` Nullable(String),
    `area_code` Nullable(String),
    `building` Nullable(String),
    `city` Nullable(String),
    `country` Nullable(String),
    `door` Nullable(String),
    `state` Nullable(String),
    `street` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` Nullable(DateTime),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.location_queue (
    `id` String,
    `lat` Float64,
    `lon` Float64,
    `area` Nullable(String),
    `area_code` Nullable(String),
    `building` Nullable(String),
    `city` Nullable(String),
    `country` Nullable(String),
    `door` Nullable(String),
    `state` Nullable(String),
    `street` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-location', kafka_group_name='ch-atlas_driver_offer_bpp-location', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.location_mv
TO atlas_driver_offer_bpp.location AS
SELECT id, lat, lon, area, area_code, building, city, country, door, state, street, merchant_id, merchant_operating_city_id, created_at, updated_at, updated_at AS version
FROM atlas_driver_offer_bpp.location_queue;

-- subscription_purchase -----------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.subscription_purchase (
    `id` String,
    `owner_id` String,
    `owner_type` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `plan_id` String,
    `plan_fee` Float64,
    `plan_frequency` String,
    `payment_order_id` String,
    `service_name` String,
    `status` String,
    `start_date` Nullable(DateTime),
    `expiry_date` Nullable(DateTime),
    `purchase_timestamp` DateTime,
    `enable_service_usage_charge` Boolean,
    `created_at` Nullable(DateTime),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.subscription_purchase_queue (
    `id` String,
    `owner_id` String,
    `owner_type` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `plan_id` String,
    `plan_fee` Float64,
    `plan_frequency` String,
    `payment_order_id` String,
    `service_name` String,
    `status` String,
    `start_date` Nullable(DateTime),
    `expiry_date` Nullable(DateTime),
    `purchase_timestamp` DateTime,
    `enable_service_usage_charge` Boolean,
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='adob-sessionizer-subscription_purchase', kafka_group_name='ch-atlas_driver_offer_bpp-subscription_purchase', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_driver_offer_bpp.subscription_purchase_mv
TO atlas_driver_offer_bpp.subscription_purchase AS
SELECT id, owner_id, owner_type, merchant_id, merchant_operating_city_id, plan_id, plan_fee, plan_frequency, payment_order_id, service_name, status, start_date, expiry_date, purchase_timestamp, enable_service_usage_charge, created_at, updated_at, updated_at AS version
FROM atlas_driver_offer_bpp.subscription_purchase_queue;