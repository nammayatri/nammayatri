let common = ../generic/common.dhall
let sec = ../secrets/fmd-wrapper.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_fmd_wrapper_v2"
  }

let pgcfg =
  { connTag = "fmdWrapperDb"
  , pgConfig = postgresConfig
  , poolConfig = (../generic/common.dhall).defaultPoolConfig
  , schemaName = "atlas_fmd_wrapper"
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
, port = +8018
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, logRawSql = True
, coreVersion = "0.8.0"
, domainVersion = "0.8.2"
}
