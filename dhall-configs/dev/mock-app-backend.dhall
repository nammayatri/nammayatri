let common = ./common.dhall
let globalCommon = ../generic/common.dhall
let sec = ./secrets/mock-app-backend.dhall

let gwUri = "http://localhost:8015/v1"

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5437
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_mock_app_backend"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = globalCommon.defaultPoolConfig
  , schemaName = "atlas_mock_app_backend"
  }

in

{ dbCfg = pgcfg
, port = +8016
, metricsPort = +9996
, xGatewayUri = gwUri
, selfId = "JUSPAY.BAP.MOCK.1"
, nwAddress = "http://localhost:8016/v1/"
, migrationPath = None Text
, autoMigrate = globalCommon.autoMigrate
, loggerConfig = globalCommon.loggerConfig // {logFilePath = "/tmp/mock-app-backend.log"}
}
