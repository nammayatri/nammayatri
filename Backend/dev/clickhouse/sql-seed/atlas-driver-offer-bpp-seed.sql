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
    `updated_at` DateTime DEFAULT now()
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.daily_stats (
    `id` String,
    `driver_id` String,
    `merchant_local_date` DateTime DEFAULT now(),
    `total_earnings` Float64,
    `total_distance` Float64,
    `num_rides` Int64,
    `cancellation_charges` Float64,
    `bonus_earnings` Float64
) ENGINE = MergeTree() PRIMARY KEY (id);

CREATE TABLE atlas_driver_offer_bpp.search_request_for_driver (
    `id` String,
    `driver_id` String,
    `search_request_id` Nullable(String),
    `created_at` DateTime DEFAULT now(),
    `updated_at` DateTime DEFAULT now(),
    `status` Nullable(String),
    `trip_estimated_duration` Nullable(Int64),
    `trip_estimated_distance` Nullable(Int64)

) ENGINE = MergeTree() PRIMARY KEY (id);
