let globalCommon = ../../generic/common.dhall

let signingKeys =
[
  -- Gateway
    globalCommon.mkSigningKey "juspay-bg-1-key" "xxxxxxx"

  -- FMD
  -- BAP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bap-key-sandbox"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-local-fmd-bap-key-sandbox" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bpp-key-sandbox"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-dunzo-fmd-bpp-key-sandbox" "xxxxxxx"

  -- Mobility
  -- BAP
  , globalCommon.mkSigningKey "juspay-mobility-bap-1-key-sandbox" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mobility-bpp-1-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-2-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-3-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-4-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-5-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-6-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-7-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-8-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-9-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-10-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-11-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-12-key-sandbox" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-13-key-sandbox" "xxxxxxx"
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
}
