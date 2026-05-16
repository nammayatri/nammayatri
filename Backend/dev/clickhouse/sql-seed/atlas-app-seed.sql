-- =============================================================================
-- ClickHouse seed for rider-app analytics database `atlas_app`.
--
-- Schema mirrors the PostgreSQL tables consumed by rider-app via the
-- `Storage/Clickhouse/*.hs` modules. Population is driven by the rider drainer
-- (PUSH_TO_KAFKA=true), which produces row JSON to `aap-sessionizer-<table>`.
-- Each table here has:
--   * a ReplacingMergeTree destination table (queryable by the application)
--   * a Kafka engine staging table reading the corresponding sessionizer topic
--   * a MATERIALIZED VIEW that pipes staging -> destination
--
-- Column projections are intentionally minimal — drainer payloads contain the
-- full row, JSONEachRow ignores extra keys, so adding a column later only
-- requires DDL changes here, no producer changes.
-- =============================================================================

-- alert_incident -------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.alert_incident (
    `id` String,
    `alert_name` String,
    `alert_group` Nullable(String),
    `service_name` String,
    `status` String,
    `severity` Nullable(String),
    `firing_time` DateTime,
    `resolved_time` Nullable(DateTime),
    `downtime_seconds` Nullable(Int64),
    `is_manually_entered` Nullable(Boolean),
    `created_at` Nullable(DateTime),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.alert_incident_queue (
    `id` String,
    `alert_name` String,
    `alert_group` Nullable(String),
    `service_name` String,
    `status` String,
    `severity` Nullable(String),
    `firing_time` DateTime,
    `resolved_time` Nullable(DateTime),
    `downtime_seconds` Nullable(Int64),
    `is_manually_entered` Nullable(Boolean),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-alert_incident', kafka_group_name='ch-atlas_app-alert_incident', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.alert_incident_mv
TO atlas_app.alert_incident AS
SELECT id, alert_name, alert_group, service_name, status, severity, firing_time, resolved_time, downtime_seconds, is_manually_entered, created_at, updated_at, updated_at AS version
FROM atlas_app.alert_incident_queue;

-- booking --------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.booking (
    `id` String,
    `status` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `fare_product_type` Nullable(String),
    `bpp_ride_booking_id` Nullable(String),
    `transaction_id` Nullable(String),
    `estimated_distance` Nullable(Float64),
    `estimated_duration` Nullable(Int64),
    `estimated_fare` Float64,
    `estimated_total_fare` Float64,
    `discount` Nullable(Float64),
    `is_scheduled` Nullable(Boolean),
    `is_pet_ride` Nullable(Boolean),
    `is_air_conditioned` Nullable(Boolean),
    `is_dashboard_request` Nullable(Boolean),
    `created_at` Nullable(DateTime),
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.booking_queue (
    `id` String,
    `status` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `fare_product_type` Nullable(String),
    `bpp_ride_booking_id` Nullable(String),
    `transaction_id` Nullable(String),
    `estimated_distance` Nullable(Float64),
    `estimated_duration` Nullable(Int64),
    `estimated_fare` Float64,
    `estimated_total_fare` Float64,
    `discount` Nullable(Float64),
    `is_scheduled` Nullable(Boolean),
    `is_pet_ride` Nullable(Boolean),
    `is_air_conditioned` Nullable(Boolean),
    `is_dashboard_request` Nullable(Boolean),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-booking', kafka_group_name='ch-atlas_app-booking', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.booking_mv
TO atlas_app.booking AS
SELECT id, status, merchant_id, merchant_operating_city_id, fare_product_type, bpp_ride_booking_id, transaction_id, estimated_distance, estimated_duration, estimated_fare, estimated_total_fare, discount, is_scheduled, is_pet_ride, is_air_conditioned, is_dashboard_request, created_at, updated_at, updated_at AS version
FROM atlas_app.booking_queue;

-- booking_cancellation_reason ------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.booking_cancellation_reason (
    `booking_id` String,
    `ride_id` Nullable(String),
    `rider_id` Nullable(String),
    `merchant_id` Nullable(String),
    `source` String,
    `reason_code` Nullable(String),
    `reason_stage` Nullable(String),
    `additional_info` Nullable(String),
    `driver_dist_to_pickup` Nullable(Int64),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (booking_id);

CREATE TABLE IF NOT EXISTS atlas_app.booking_cancellation_reason_queue (
    `booking_id` String,
    `ride_id` Nullable(String),
    `rider_id` Nullable(String),
    `merchant_id` Nullable(String),
    `source` String,
    `reason_code` Nullable(String),
    `reason_stage` Nullable(String),
    `additional_info` Nullable(String),
    `driver_dist_to_pickup` Nullable(Int64),
    `created_at` Nullable(DateTime),
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-booking_cancellation_reason', kafka_group_name='ch-atlas_app-booking_cancellation_reason', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.booking_cancellation_reason_mv
TO atlas_app.booking_cancellation_reason AS
SELECT booking_id, ride_id, rider_id, merchant_id, source, reason_code, reason_stage, additional_info, driver_dist_to_pickup, created_at, updated_at, COALESCE(updated_at, now()) AS version
FROM atlas_app.booking_cancellation_reason_queue;

-- conductor_stats (analytics-only, no Kafka topic; populated by hourly jobs) ---
CREATE TABLE IF NOT EXISTS atlas_app.conductor_stats (
    `conductor_token_no` String,
    `booking_date` Date,
    `date` DateTime,
    `fleet_no` Nullable(String),
    `depot_no` Nullable(String),
    `number_tickets_booked` Nullable(Int64),
    `total_revenue_in_a_day` Nullable(Float64),
    `number_of_new_customers` Nullable(Int64),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (conductor_token_no, booking_date);

-- estimate_breakup -----------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.estimate_breakup (
    `id` String,
    `estimate_id` String,
    `title` String,
    `price_currency` String,
    `price_value` Float64,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.estimate_breakup_queue (
    `id` String,
    `estimate_id` String,
    `title` String,
    `price_currency` String,
    `price_value` Float64
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-estimate_breakup', kafka_group_name='ch-atlas_app-estimate_breakup', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.estimate_breakup_mv
TO atlas_app.estimate_breakup AS
SELECT id, estimate_id, title, price_currency, price_value, now() AS version
FROM atlas_app.estimate_breakup_queue;

-- fare_breakup ---------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.fare_breakup (
    `id` String,
    `booking_id` String,
    `description` String,
    `entity_type` String,
    `amount` Float64,
    `currency` Nullable(String),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.fare_breakup_queue (
    `id` String,
    `booking_id` String,
    `description` String,
    `entity_type` String,
    `amount` Float64,
    `currency` Nullable(String)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-fare_breakup', kafka_group_name='ch-atlas_app-fare_breakup', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.fare_breakup_mv
TO atlas_app.fare_breakup AS
SELECT id, booking_id, description, entity_type, amount, currency, now() AS version
FROM atlas_app.fare_breakup_queue;

-- frfs_quote_category --------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.frfs_quote_category (
    `id` String,
    `quote_id` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `category` Nullable(String),
    `code` Nullable(String),
    `title` Nullable(String),
    `description` Nullable(String),
    `price` Float64,
    `offered_price` Float64,
    `final_price` Nullable(Float64),
    `currency` Nullable(String),
    `selected_quantity` Nullable(Int64),
    `created_at` DateTime,
    `updated_at` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.frfs_quote_category_queue (
    `id` String,
    `quote_id` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `category` Nullable(String),
    `code` Nullable(String),
    `title` Nullable(String),
    `description` Nullable(String),
    `price` Float64,
    `offered_price` Float64,
    `final_price` Nullable(Float64),
    `currency` Nullable(String),
    `selected_quantity` Nullable(Int64),
    `created_at` DateTime,
    `updated_at` DateTime
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-frfs_quote_category', kafka_group_name='ch-atlas_app-frfs_quote_category', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.frfs_quote_category_mv
TO atlas_app.frfs_quote_category AS
SELECT id, quote_id, merchant_id, merchant_operating_city_id, category, code, title, description, price, offered_price, final_price, currency, selected_quantity, created_at, updated_at, updated_at AS version
FROM atlas_app.frfs_quote_category_queue;

-- frfs_ticket_booking --------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.frfs_ticket_booking (
    `id` String,
    `quote_id` String,
    `provider_id` String,
    `provider_name` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `from_station_id` String,
    `to_station_id` Nullable(String),
    `vehicle_type` Nullable(String),
    `status` Nullable(String),
    `payment_txn_id` Nullable(String),
    `is_booking_cancellable` Nullable(Boolean),
    `customer_cancelled` Boolean,
    `created_at` DateTime,
    `updated_at` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.frfs_ticket_booking_queue (
    `id` String,
    `quote_id` String,
    `provider_id` String,
    `provider_name` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `from_station_id` String,
    `to_station_id` Nullable(String),
    `vehicle_type` Nullable(String),
    `status` Nullable(String),
    `payment_txn_id` Nullable(String),
    `is_booking_cancellable` Nullable(Boolean),
    `customer_cancelled` Boolean,
    `created_at` DateTime,
    `updated_at` DateTime
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-frfs_ticket_booking', kafka_group_name='ch-atlas_app-frfs_ticket_booking', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.frfs_ticket_booking_mv
TO atlas_app.frfs_ticket_booking AS
SELECT id, quote_id, provider_id, provider_name, merchant_id, merchant_operating_city_id, from_station_id, to_station_id, vehicle_type, status, payment_txn_id, is_booking_cancellable, customer_cancelled, created_at, updated_at, updated_at AS version
FROM atlas_app.frfs_ticket_booking_queue;

-- location -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.location (
    `id` String,
    `lat` Float64,
    `lon` Float64,
    `area` Nullable(String),
    `area_code` Nullable(String),
    `city` Nullable(String),
    `country` Nullable(String),
    `state` Nullable(String),
    `street` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` DateTime,
    `updated_at` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.location_queue (
    `id` String,
    `lat` Float64,
    `lon` Float64,
    `area` Nullable(String),
    `area_code` Nullable(String),
    `city` Nullable(String),
    `country` Nullable(String),
    `state` Nullable(String),
    `street` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` DateTime,
    `updated_at` DateTime
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-location', kafka_group_name='ch-atlas_app-location', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.location_mv
TO atlas_app.location AS
SELECT id, lat, lon, area, area_code, city, country, state, street, merchant_id, merchant_operating_city_id, created_at, updated_at, updated_at AS version
FROM atlas_app.location_queue;

-- person ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.person (
    `id` String,
    `first_name` Nullable(String),
    `last_name` Nullable(String),
    `gender` Nullable(String),
    `identifier_type` Nullable(String),
    `language` Nullable(String),
    `enabled` Boolean,
    `blocked` Boolean,
    `has_taken_valid_ride` Boolean,
    `current_city` Nullable(String),
    `aadhaar_verified` Boolean,
    `created_at` DateTime,
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.person_queue (
    `id` String,
    `first_name` Nullable(String),
    `last_name` Nullable(String),
    `gender` Nullable(String),
    `identifier_type` Nullable(String),
    `language` Nullable(String),
    `enabled` Boolean,
    `blocked` Boolean,
    `has_taken_valid_ride` Boolean,
    `current_city` Nullable(String),
    `aadhaar_verified` Boolean,
    `created_at` DateTime,
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-person', kafka_group_name='ch-atlas_app-person', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.person_mv
TO atlas_app.person AS
SELECT id, first_name, last_name, gender, identifier_type, language, enabled, blocked, has_taken_valid_ride, current_city, aadhaar_verified, created_at, updated_at, COALESCE(updated_at, now()) AS version
FROM atlas_app.person_queue;

-- purchased_pass_payment -----------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.purchased_pass_payment (
    `id` String,
    `purchased_pass_id` String,
    `person_id` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `order_id` String,
    `pass_code` String,
    `pass_name` Nullable(String),
    `pass_enum` Nullable(String),
    `amount` Float64,
    `benefit_type` Nullable(String),
    `benefit_value` Nullable(Float64),
    `start_date` Date,
    `end_date` Date,
    `status` String,
    `is_dashboard` Nullable(Boolean),
    `created_at` DateTime,
    `updated_at` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.purchased_pass_payment_queue (
    `id` String,
    `purchased_pass_id` String,
    `person_id` String,
    `merchant_id` String,
    `merchant_operating_city_id` String,
    `order_id` String,
    `pass_code` String,
    `pass_name` Nullable(String),
    `pass_enum` Nullable(String),
    `amount` Float64,
    `benefit_type` Nullable(String),
    `benefit_value` Nullable(Float64),
    `start_date` Date,
    `end_date` Date,
    `status` String,
    `is_dashboard` Nullable(Boolean),
    `created_at` DateTime,
    `updated_at` DateTime
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-purchased_pass_payment', kafka_group_name='ch-atlas_app-purchased_pass_payment', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.purchased_pass_payment_mv
TO atlas_app.purchased_pass_payment AS
SELECT id, purchased_pass_id, person_id, merchant_id, merchant_operating_city_id, order_id, pass_code, pass_name, pass_enum, amount, benefit_type, benefit_value, start_date, end_date, status, is_dashboard, created_at, updated_at, updated_at AS version
FROM atlas_app.purchased_pass_payment_queue;

-- quote_breakup --------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.quote_breakup (
    `id` String,
    `quote_id` String,
    `title` String,
    `price_currency` Nullable(String),
    `price_value` Float64,
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` DateTime,
    `updated_at` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.quote_breakup_queue (
    `id` String,
    `quote_id` String,
    `title` String,
    `price_currency` Nullable(String),
    `price_value` Float64,
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `created_at` DateTime,
    `updated_at` DateTime
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-quote_breakup', kafka_group_name='ch-atlas_app-quote_breakup', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.quote_breakup_mv
TO atlas_app.quote_breakup AS
SELECT id, quote_id, title, price_currency, price_value, merchant_id, merchant_operating_city_id, created_at, updated_at, updated_at AS version
FROM atlas_app.quote_breakup_queue;

-- ride -----------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.ride (
    `id` String,
    `booking_id` String,
    `bpp_ride_id` String,
    `status` Nullable(String),
    `driver_name` String,
    `driver_mobile_number` String,
    `chargeable_distance` Nullable(Float64),
    `total_fare` Nullable(Float64),
    `driver_rating` Nullable(Float64),
    `driver_arrival_status` Nullable(String),
    `driver_arrival_time` Nullable(DateTime),
    `destination_reached_at` Nullable(DateTime),
    `created_at` DateTime,
    `updated_at` DateTime DEFAULT now(),
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.ride_queue (
    `id` String,
    `booking_id` String,
    `bpp_ride_id` String,
    `status` Nullable(String),
    `driver_name` String,
    `driver_mobile_number` String,
    `chargeable_distance` Nullable(Float64),
    `total_fare` Nullable(Float64),
    `driver_rating` Nullable(Float64),
    `driver_arrival_status` Nullable(String),
    `driver_arrival_time` Nullable(DateTime),
    `destination_reached_at` Nullable(DateTime),
    `created_at` DateTime,
    `updated_at` Nullable(DateTime)
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-ride', kafka_group_name='ch-atlas_app-ride', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.ride_mv
TO atlas_app.ride AS
SELECT id, booking_id, bpp_ride_id, status, driver_name, driver_mobile_number, chargeable_distance, total_fare, driver_rating, driver_arrival_status, driver_arrival_time, destination_reached_at, created_at, updated_at, COALESCE(updated_at, now()) AS version
FROM atlas_app.ride_queue;

-- sos ------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS atlas_app.sos (
    `id` String,
    `person_id` String,
    `ride_id` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `flow` String,
    `status` String,
    `sos_state` Nullable(String),
    `entity_type` Nullable(String),
    `ticket_id` Nullable(String),
    `tracking_expires_at` Nullable(DateTime),
    `created_at` DateTime,
    `updated_at` DateTime,
    `version` DateTime DEFAULT now()
) ENGINE = ReplacingMergeTree(version) ORDER BY (id);

CREATE TABLE IF NOT EXISTS atlas_app.sos_queue (
    `id` String,
    `person_id` String,
    `ride_id` Nullable(String),
    `merchant_id` Nullable(String),
    `merchant_operating_city_id` Nullable(String),
    `flow` String,
    `status` String,
    `sos_state` Nullable(String),
    `entity_type` Nullable(String),
    `ticket_id` Nullable(String),
    `tracking_expires_at` Nullable(DateTime),
    `created_at` DateTime,
    `updated_at` DateTime
) ENGINE = Kafka
SETTINGS kafka_broker_list='localhost:29092', kafka_topic_list='aap-sessionizer-sos', kafka_group_name='ch-atlas_app-sos', kafka_format='JSONEachRow', kafka_num_consumers=1, kafka_skip_broken_messages=100;

CREATE MATERIALIZED VIEW IF NOT EXISTS atlas_app.sos_mv
TO atlas_app.sos AS
SELECT id, person_id, ride_id, merchant_id, merchant_operating_city_id, flow, status, sos_state, entity_type, ticket_id, tracking_expires_at, created_at, updated_at, updated_at AS version
FROM atlas_app.sos_queue;
