let mkSigningKey =
 \(uniqueKeyId : Text) -> \(signPrivKey : Text) ->
  {
    uniqueKeyId = uniqueKeyId
  , signPrivKey = signPrivKey
  }

let signingKeys = 
[
  mkSigningKey "mobility-app-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, mkSigningKey "fmd-test-app-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, mkSigningKey "juspay-bg-1-key"  "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, mkSigningKey "juspay-mobility-bap-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, mkSigningKey "juspay-mobility-bpp-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, mkSigningKey "juspay-bg-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, mkSigningKey "juspay-mock-bap-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, mkSigningKey "juspay-mock-bpp-1-key" "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
]

in

{ signingKeys = signingKeys
, smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
}
