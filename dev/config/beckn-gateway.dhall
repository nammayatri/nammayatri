let common = ./generic/common.dhall
let sec = ./secrets/beckn-gateway.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5435
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_gateway"
  }

let pgcfg =
  { connTag = "gatewayDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_gateway"
  }

in

{ dbCfg = pgcfg
, port = +8015
, metricsPort = +9998
, selfId = Some "JUSPAY.BG.1"
, nwAddress = Some "https://localhost/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
