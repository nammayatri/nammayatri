let globalCommon = ../../generic/common.dhall

let signingKeys =
[
  -- Gateway
    globalCommon.mkSigningKey "39" "xxxxxxx"

  -- FMD
  -- BAP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bap-key-sandbox"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-local-fmd-bap-key-sandbox" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bpp-key-sandbox"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-dunzo-fmd-bpp-key-sandbox" "xxxxxxx"

  -- Mobility
  -- BAP
  , globalCommon.mkSigningKey "35" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "37" "xxxxxxx"
  , globalCommon.mkSigningKey "38" "xxxxxxx"

  -- Metro
  -- BAP
  , globalCommon.mkSigningKey "36" "xxxxxxx"
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
