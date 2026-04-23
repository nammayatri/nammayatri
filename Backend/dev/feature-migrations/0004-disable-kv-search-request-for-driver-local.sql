-- Append search_request_for_driver to disableForKV locally so findByDriver
-- (filter by driverId — not a SecondaryKey) reads from SQL DB instead of KV.
-- Without this, OnlineRideBookingFlow nearby endpoint returns empty for test
-- drivers whose client_os_type is NULL (skipping the redis-ZSET cache path).

UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = (
    jsonb_set(
        config_value::jsonb,
        '{disableForKV}',
        (
            (config_value::jsonb -> 'disableForKV') || '"search_request_for_driver"'
        )
    )::text
)
WHERE id = 'kv_configs';
