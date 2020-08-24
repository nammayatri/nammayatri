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
  , connectPort = 5433
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  }

in

{ dbCfg = pgcfg
, port = +8016
, xGatewayUri = gwUri
, selfId = Some "JUSPAY.BAP.MOCK.1"
, nwAddress = Some "https://localhost/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
