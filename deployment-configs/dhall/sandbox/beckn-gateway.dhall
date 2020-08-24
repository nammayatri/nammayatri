let common = ../generic/common.dhall
let sec = ../secrets/beckn-gateway.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
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
, metricsPort = +9999
, selfId = Some "JUSPAY.BG.1"
, nwAddress = Some "https://api.sandbox.beckn.juspay.in/dev/gateway/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
