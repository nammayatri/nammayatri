let common = ./generic/common.dhall
let sec = ./secrets/mock-app-backend.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5438
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_mock_provider_backend"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_mock_provider_backend"
  }

let gwUri = "http://localhost:8015/v1"

in

{ dbCfg = pgcfg
, port = +8017
, metricsPort = +9995
, xGatewayUri = gwUri
, selfId = "JUSPAY.BPP.MOCK.1"
, nwAddress = "http://localhost:8017/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-provider-backend.log"}
}
