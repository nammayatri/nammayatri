-- Mock Payment Server Configuration
let common = ./common.dhall

in  { port = +8091
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-payment.log" }
    , graceTerminationPeriod = +90
    , juspayWebhookBaseUrl = "https://api.sandbox.moving.tech/app"
    , esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = "atlas_app_user"
      , connectPassword = "atlas"
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_app"
      , connectionPoolCount = +2
      }
    , hedisCfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +5
      , connectMaxIdleTime = +30
      , connectTimeout = Some +1
      , connectReadOnly = False
      }
    }
