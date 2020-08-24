let common = ../generic/common.dhall
let sec = ../secrets/mock-app-backend.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app_v2"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_mock_app_backend"
  }

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "beckn-gateway-${common.branchName}.atlas"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

in

{ dbCfg = pgcfg
, port = +8016
, xGatewayUri = gwUri
, selfId = Some "JUSPAY.BAP.MOCK.1"
, nwAddress = Some "Http://api.sandbox.beckn.juspay.in/dev/mock/app/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
