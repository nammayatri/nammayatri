let globalCommon = ../../generic/common.dhall

let signingKeys =
  [
  -- Gateway
    globalCommon.mkSigningKey "juspay-bg-1-key" "xxxxxxx"

  -- FMD
  -- BAP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bap-key"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-local-fmd-bap-key" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bpp-key"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-dunzo-fmd-bpp-key" "xxxxxxx"

  -- Mobility
  -- BAP
  , globalCommon.mkSigningKey "juspay-mobility-bap-1-key" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mobility-bpp-1-key" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-2-key" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-3-key" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-4-key" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-5-key" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-6-key" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-7-key" "xxxxxxx"
  ]

in

{ signingKeys = signingKeys
, smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
}
