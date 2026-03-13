-- Corporate Commute ClickHouse event tables

CREATE TABLE IF NOT EXISTS atlas_kafka.corporate_trip_events (
    `event_id` String,
    `corporate_entity_id` String,
    `corporate_employee_id` String,
    `shift_id` Nullable(String),
    `route_id` Nullable(String),
    `booking_id` Nullable(String),
    `ride_id` Nullable(String),
    `event_type` String,
    `pickup_lat` Nullable(Float64),
    `pickup_lon` Nullable(Float64),
    `drop_lat` Nullable(Float64),
    `drop_lon` Nullable(Float64),
    `estimated_fare` Nullable(Float64),
    `actual_fare` Nullable(Float64),
    `currency` Nullable(String),
    `vehicle_tier` Nullable(String),
    `distance_meters` Nullable(Int32),
    `duration_seconds` Nullable(Int32),
    `scheduled_pickup_time` Nullable(DateTime64(3)),
    `actual_pickup_time` Nullable(DateTime64(3)),
    `actual_drop_time` Nullable(DateTime64(3)),
    `status` Nullable(String),
    `merchant_id` String,
    `merchant_operating_city_id` Nullable(String),
    `created_at` DateTime64(3) DEFAULT now(),
    `partition_date` Date DEFAULT toDate(created_at)
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(partition_date)
ORDER BY (corporate_entity_id, corporate_employee_id, created_at)
TTL partition_date + INTERVAL 365 DAY;

CREATE TABLE IF NOT EXISTS atlas_kafka.corporate_billing_events (
    `event_id` String,
    `corporate_entity_id` String,
    `invoice_id` Nullable(String),
    `event_type` String,
    `billing_model` Nullable(String),
    `billing_cycle_type` Nullable(String),
    `amount` Nullable(Float64),
    `tax_amount` Nullable(Float64),
    `net_amount` Nullable(Float64),
    `currency` Nullable(String),
    `trip_count` Nullable(Int32),
    `employee_count` Nullable(Int32),
    `period_start` Nullable(DateTime64(3)),
    `period_end` Nullable(DateTime64(3)),
    `wallet_balance_before` Nullable(Float64),
    `wallet_balance_after` Nullable(Float64),
    `payment_status` Nullable(String),
    `merchant_id` String,
    `created_at` DateTime64(3) DEFAULT now(),
    `partition_date` Date DEFAULT toDate(created_at)
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(partition_date)
ORDER BY (corporate_entity_id, created_at)
TTL partition_date + INTERVAL 365 DAY;

CREATE TABLE IF NOT EXISTS atlas_kafka.corporate_safety_events (
    `event_id` String,
    `corporate_entity_id` String,
    `corporate_employee_id` String,
    `shift_id` Nullable(String),
    `route_id` Nullable(String),
    `booking_id` Nullable(String),
    `ride_id` Nullable(String),
    `event_type` String,
    `severity` Nullable(String),
    `is_night_shift` Nullable(UInt8),
    `is_women_safety` Nullable(UInt8),
    `deviation_meters` Nullable(Int32),
    `sos_triggered` Nullable(UInt8),
    `driver_id` Nullable(String),
    `vehicle_number` Nullable(String),
    `lat` Nullable(Float64),
    `lon` Nullable(Float64),
    `description` Nullable(String),
    `merchant_id` String,
    `merchant_operating_city_id` Nullable(String),
    `created_at` DateTime64(3) DEFAULT now(),
    `partition_date` Date DEFAULT toDate(created_at)
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(partition_date)
ORDER BY (corporate_entity_id, corporate_employee_id, created_at)
TTL partition_date + INTERVAL 365 DAY;
