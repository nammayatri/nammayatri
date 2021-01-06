let globalCommon = ../../generic/common.dhall

let signingKeys =
  [
  -- Gateway
    globalCommon.mkSigningKey "juspay-bg-1-key" "xxxxxxx"

  -- FMD
  -- BAP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bap-key-dev"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-local-fmd-bap-key-dev" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mock-fmd-bpp-key-dev"  "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-dunzo-fmd-bpp-key-dev" "xxxxxxx"

  -- Mobility
  -- BAP
  , globalCommon.mkSigningKey "juspay-mobility-bap-1-key-dev" "xxxxxxx"
  -- BPP
  , globalCommon.mkSigningKey "juspay-mobility-bpp-1-key-dev" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-2-key-dev" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-3-key-dev" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-4-key-dev" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-5-key-dev" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-6-key-dev" "xxxxxxx"
  , globalCommon.mkSigningKey "juspay-mobility-bpp-7-key-dev" "xxxxxxx"
  ]

in

{ signingKeys = signingKeys
, smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
}
