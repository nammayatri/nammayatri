let registrySecrets =
  { encryptionPrivKeyB16 = "xxxxxxx"
    -- Private key PEM-encoded: xxxxxxx
    -- Associated public key in B64 format without ASN.1 headers: xxxxxxx
  }

in

{ dbUserId = "atlas"
, dbPassword = "atlas"
, smsOtpHash = "xxxxxxx"
, registrySecrets = registrySecrets
, signingKey = "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, encHashSalt = "How wonderful it is that nobody need wait a single moment before starting to improve the world"
}
