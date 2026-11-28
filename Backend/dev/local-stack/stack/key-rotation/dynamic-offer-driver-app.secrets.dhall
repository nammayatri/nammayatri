-- dynamic-offer-driver-app's secrets, as upstream ships them (Backend/dhall-configs/dev/secrets/
-- dynamic-offer-driver-app.dhall at 03a7531, the ref the running images are built from), with ONE
-- change: the hash salt is read from the environment, MOVIN_HASH_SALT, which
-- the server keeps in /opt/ny/secrets. Mounted over the upstream file by the
-- key rotation (README.md beside this file); until then nothing reads it.
{ dbUserId = "atlas_driver_offer_bpp_user"
, dbPassword = "atlas"
, smsOtpHash = "xxxxxxx"
, signingKey = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="
, encHashSalt = env:MOVIN_HASH_SALT as Text
, dashboardToken = "some-secret-dashboard-token-for-driver-offer-bpp"
}
