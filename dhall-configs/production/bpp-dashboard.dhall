let common = ./common.dhall
let sec = ./secrets/bpp-dashboard.dhall

let esqDBCfg =
  { connectHost = "adb.primary.beckn.juspay.net"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_bpp_dashboard"
  , connectSchemaName = "atlas_bpp_dashboard"
  }

let rcfg =
  { connectHost = "cache.primary.beckn.juspay.net"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

let ServerName = < APP_BACKEND_YATRI | APP_BACKEND_ARDU | BECKN_TRANSPORT | DRIVER_OFFER_BPP >

let becknTransport =
  { name = ServerName.BECKN_TRANSPORT
  , url = "https://api.beckn.juspay.in/bpp/dashboard/"
  , token = sec.becknTransportToken
  }

let driverOfferBpp =
  { name = ServerName.DRIVER_OFFER_BPP
  , url = "https://api.beckn.juspay.in/dobpp/dashboard/"
  , token = sec.driverOfferBppToken
  }

in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8018
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/bpp-dashboard.log"}
, graceTerminationPeriod = +90
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = common.httpClientOptions
, authTokenCacheExpiry = +600 --seconds
, registrationTokenExpiry = +365 --days
, encTools = encTools
, dataServers = [becknTransport, driverOfferBpp]
}
