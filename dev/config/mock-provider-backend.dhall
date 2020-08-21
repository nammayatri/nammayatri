let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "localhost"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

in

{ port = +8017
, xGatewayUri = gwUri
, selfId = Some "JUSPAY.BPP.MOCK.1"
, nwAddress = Some "https://localhost/v1/"
}
