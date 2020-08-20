let common = ../generic/common.dhall
let sec = ../secrets/beckn-gateway.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_gateway_v2"
  }

let pgcfg =
  { connTag = "gatewayDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_gateway"
  }

let rcfg =
  { connectHost = "ec-redis-beta-002.bfw4iw.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, port = +8015
, metricsPort = +9999
, selfId = Some "JUSPAY.BG.1"
, nwAddress = Some "https://api.sandbox.beckn.juspay.in/dev/gateway/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, searchTimeout = None Integer
}
