let globalCommon = ../../generic/common.dhall

let signingKeys =
  [
  -- Gateway
    globalCommon.mkSigningKey "22" "xxxxxxx"

  -- FMD
  -- BAP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bap-key-dev"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-local-fmd-bap-key-dev" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bpp-key-dev"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-dunzo-fmd-bpp-key-dev" "xxxxxxx"

  -- Mobility
  -- BAP
  , globalCommon.mkSigningKey "19" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "12" "xxxxxxx"
  , globalCommon.mkSigningKey "13" "xxxxxxx"
  , globalCommon.mkSigningKey "14" "xxxxxxx"
  , globalCommon.mkSigningKey "15" "xxxxxxx"
  , globalCommon.mkSigningKey "16" "xxxxxxx"
  , globalCommon.mkSigningKey "17" "xxxxxxx"
  , globalCommon.mkSigningKey "18" "xxxxxxx"

  -- Metro
  -- BAP
  , globalCommon.mkSigningKey "20" "xxxxxxx"

  -- Parking
  -- BAP
  , globalCommon.mkSigningKey "56" "xxxxxxx"
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
