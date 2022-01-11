let payee = "<payment endpoint json stringified>"

let registrySecrets =
  { encryptionPrivKeyB16 = "xxxxxxx"
    -- Private key PEM-encoded: xxxxxxx
    -- Associated public key in B64 format without ASN.1 headers: xxxxxxx
  }

in

{ dbUserId = "atlas"
, dbPassword = "atlas"
, payee = payee
, registrySecrets = registrySecrets
, signingKey = "xxxxxxx"
}
