let common = ../generic/common.dhall

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "beckn-gateway-${common.branchName}.atlas"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

in

{ port = +8017
, xGatewayUri = gwUri
, selfId = Some "JUSPAY.BPP.MOCK.1"
, nwAddress = Some "Http://api.sandbox.beckn.juspay.in/dev/mock/provider/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
