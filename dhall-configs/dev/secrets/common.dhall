let topSecret = ./top-secret.dhall
let globalCommon = ../../generic/common.dhall

let exotelCfg =
  { apiKey = "xxxxxxx"
  , apiToken = "xxxxxxx"
  , sid = "xxxxxxx"
  , callerId = "xxxxxxx"
  }

let idfyCfg = 
  { accountId :: "xxxxxxxx",
    apiKey :: "xxxxxxxx"
  }

in

{ smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
, exotelCfg = exotelCfg
, googleMapsKey = topSecret.googleMapsKey
, idfyCfg = idfyCfg
}
