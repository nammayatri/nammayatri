let non_persistent_redis_cfg =
      { redis_host = "0.0.0.0"
      , redis_port = 6379
      , redis_pool_size = 10
      , redis_partition = 0
      , reconnect_max_attempts = 10
      , reconnect_delay = 5000
      , default_ttl = 3600
      , default_hash_ttl = 3600
      , stream_read_count = 100
      }

let persistent_redis_cfg =
      { redis_host = "0.0.0.0"
      , redis_port = 6379
      , redis_pool_size = 10
      , redis_partition = 0
      , reconnect_max_attempts = 10
      , reconnect_delay = 5000
      , default_ttl = 3600
      , default_hash_ttl = 3600
      , stream_read_count = 100
      }

let kafka_cfg =
      { kafka_key = "bootstrap.servers", kafka_host = "0.0.0.0:29092" }

let LogLevel = < TRACE | DEBUG | INFO | WARN | ERROR | OFF >

let logger_cfg = { level = LogLevel.INFO, log_to_file = False }

in  { logger_cfg
    , non_persistent_redis_cfg
    , non_persistent_migration_redis_cfg = non_persistent_redis_cfg
    , persistent_redis_cfg
    , persistent_migration_redis_cfg = persistent_redis_cfg
    , redis_migration_stage = False
    , workers = 1
    , drainer_size = 10
    , drainer_delay = 20
    , new_ride_drainer_delay = 2
    , kafka_cfg
    , port = 8081
    , auth_url = "http://127.0.0.1:8016/internal/auth"
    , auth_api_key = "ae288466-2add-11ee-be56-0242ac120002"
    , bulk_location_callback_url =
        "http://127.0.0.1:8016/internal/bulkLocUpdate"
    , auth_token_expiry = 86400
    , min_location_accuracy = 50.0
    , driver_location_accuracy_buffer = 25.0
    , redis_expiry = 86400
    , last_location_timstamp_expiry = 86400
    , location_update_limit = 6000000000
    , location_update_interval = 60
    , driver_location_update_topic = "location-updates"
    , batch_size = 100
    , bucket_size = 30
    , nearby_bucket_threshold = 4
    }
