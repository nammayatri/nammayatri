let common = ../generic/common.dhall

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "beckn-gateway-${common.branchName}.atlas"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

in

{ port = +8016
, xGatewayUri = gwUri
, selfId = Some "JUSPAY.BAP.MOCK.1"
, nwAddress = Some "Http://api.sandbox.beckn.juspay.in/dev/mock/app/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
