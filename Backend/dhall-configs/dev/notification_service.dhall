let redis_cfg =
      { host = "0.0.0.0"
      , port = env:REDIS_CLUSTER_PORT ? 30001
      , cluster_enabled = True
      , cluster_urls = [ "" ]
      , use_legacy_version = False
      , pool_size = 25
      , reconnect_max_attempts = 10
      , reconnect_delay = 5000
      , default_ttl = 3600
      , default_hash_ttl = 3600
      , stream_read_count = 100
      , partition = 0
      , broadcast_channel_capacity = 10000
      }

let LogLevel = < TRACE | DEBUG | INFO | WARN | ERROR | OFF >

let driverAppPort = Natural/show (env:DRIVER_APP_PORT ? 8016)

let logger_cfg = { level = LogLevel.INFO, log_to_file = False }

let driver_internal_auth_config =
      { auth_url = "http://127.0.0.1:${driverAppPort}/internal/auth"
      , auth_api_key = "ae288466-2add-11ee-be56-0242ac120002"
      , auth_token_expiry = 86400
      }

let driver_dashboard_internal_auth_config =
      { auth_url = "http://127.0.0.1:${driverAppPort}/internal/auth"
      , auth_api_key = "ae288466-2add-11ee-be56-0242ac120002"
      , auth_token_expiry = 86400
      }

let tokenOriginInternalAuthMap =
      { DriverApp = driver_internal_auth_config
      , RiderApp = driver_internal_auth_config
      , DriverDashboard = driver_dashboard_internal_auth_config
      , RiderDashboard = driver_dashboard_internal_auth_config
      }

in  { grpc_port = env:GRPC_PORT ? 50051
    , http_server_port = env:HTTP_SERVER_PORT ? 9091
    , internal_auth_cfg = tokenOriginInternalAuthMap
    , driver_api_base_url = "http://127.0.0.1:${driverAppPort}"
    , logger_cfg
    , redis_cfg
    , max_shards = +5
    , channel_buffer = 100000
    , request_timeout_seconds = 60
    , retry_delay_millis = 1000
    , read_all_connected_client_notifications = True
    }
