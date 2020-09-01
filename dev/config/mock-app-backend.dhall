let common = ./generic/common.dhall
let sec = ./secrets/mock-app-backend.dhall

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "localhost"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

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
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_mock_app_backend"
  }

in

{ dbCfg = pgcfg
, port = +8016
, xGatewayUri = gwUri
, selfId = Some "JUSPAY.BAP.MOCK.1"
, nwAddress = Some "https://localhost/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, logRawSql = True
}
