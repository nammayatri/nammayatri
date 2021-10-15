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
}
