let globalCommon = ../../generic/common.dhall

let signingKeys =
[
  -- Gateway
    globalCommon.mkSigningKey "juspay-bg-1-key" "xxxxxxx"

  -- FMD
  -- BAP
  , globalCommon.mkSigningKey "juspay-local-fmd-bap-key-prod" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-dunzo-fmd-bpp-key-prod" "xxxxxxx"

  -- Mobility
  -- BAP
  , globalCommon.mkSigningKey "juspay-mobility-bap-1-key-prod" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mobility-bpp-1-key-prod" "xxxxxxx"
]

let exotelCfg =
  { apiKey = "xxxxxxx"
  , apiToken = "xxxxxxx"
  , sid = "xxxxxxx"
  , callerId = "xxxxxxx"
  }

in

{ signingKeys = signingKeys
, smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
, exotelCfg = exotelCfg
, googleMapsKey = ""
}
