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

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "localhost"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

in

{ dbCfg = pgcfg
, port = +8017
, metricsPort = +9995
, xGatewayUri = gwUri
, selfId = Some "JUSPAY.BPP.MOCK.1"
, nwAddress = Some "http://localhost:8017/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, logRawSql = True
}
