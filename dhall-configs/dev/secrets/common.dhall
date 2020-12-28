let globalCommon = ../../generic/common.dhall

let signingKeys =
[
  globalCommon.mkSigningKey "mobility-app-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "fmd-test-app-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "juspay-bg-1-key"  "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "juspay-mobility-bap-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "juspay-mobility-bpp-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "juspay-bg-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "juspay-mock-bap-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "juspay-mock-bpp-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, globalCommon.mkSigningKey "another-test-cabs" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
]

in

{ signingKeys = signingKeys
, smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
}
