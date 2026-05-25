-- Mock Payment Server Configuration
let common = ./common.dhall

in  { port = Natural/toInteger (env:SERVICE_PORT ? 8091)
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-payment.log" }
    , graceTerminationPeriod = +90
    , juspayWebhookBaseUrl = "https://api.sandbox.moving.tech/app"
    , esqDBCfg =
      { connectHost = "localhost"
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = "atlas_app_user"
      , connectPassword = "atlas"
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_app"
      , connectionPoolCount = +10
      }
    , hedisCfg =
      { connectHost = "localhost"
      , connectPort = env:REDIS_PORT ? 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = False
      }
    }
