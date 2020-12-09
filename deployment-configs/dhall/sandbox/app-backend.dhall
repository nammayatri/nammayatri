let common = ../generic/common.dhall
let sec = ../secrets/app-backend.dhall

let GeoRestriction = < Unrestricted | Region : Text>

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_app"
  }

let smsConfig =
  { sessionConfig = common.smsSessionConfig
  , credConfig = {
      username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = None Natural
  }

let sesConfig =
  { issuesConfig = {
      from = "support@juspay.in"
    , to = "beckn_mobility@juspay.in"
    , replyTo = "beckn_mobility@juspay.in"
    , region = "eu-west-1"
    }
  }
let geofencingConfig =
{ origin = GeoRestriction.Region "Ernakulam"
, destination = GeoRestriction.Region "Kerala"
}

let gwUri = "http://beckn-gateway-${common.branchName}.atlas:8015/v1"

let providerUri = "http://beckn-transport-${common.branchName}.atlas:8014/v1"

let nsdlUrl = "https://pilot-gateway-1.beckn.nsdl.co.in"

in

{ dbCfg = pgcfg
, smsCfg = smsConfig
, sesCfg = sesConfig
, port = +8013
, metricsPort = +9999
, xGatewayUri = gwUri
, xGatewayApiKey = None Text
, xGatewaySelector = Some "JUSPAY"
, xGatewayNsdlUrl = Some nsdlUrl
, xProviderUri = providerUri
, bapSelfId = Some "JUSPAY.MOBILITY.APP.UAT.1"
, bapNwAddress = Some "https://api.sandbox.beckn.juspay.in/dev/app/v1/"
, searchConfirmExpiry = Some +7200
, searchCaseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None common.ExotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.8.2"
, domainVersion = "0.8.2"
, geofencingConfig = geofencingConfig
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/app-backend.log"}
, signatureExpiry = common.signatureExpiry
}
