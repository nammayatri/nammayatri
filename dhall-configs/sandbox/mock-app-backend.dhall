let common = ./common.dhall
let globalCommon = ../generic/common.dhall
let sec = ./secrets/mock-app-backend.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
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

let gwUri = "http://beckn-gateway-${common.branchName}.atlas:8015/v1"

in

{ dbCfg = pgcfg
, port = +8016
, metricsPort = +9999
, xGatewayUri = gwUri
, selfId = "JUSPAY.BAP.MOCK.1"
, nwAddress = "https://api.sandbox.beckn.juspay.in/dev/mock/app/v1/"
, migrationPath = None Text
, autoMigrate = globalCommon.autoMigrate
, loggerConfig = globalCommon.loggerConfig // {logFilePath = "/tmp/mock-app-backend.log"}
}
