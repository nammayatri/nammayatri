{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.AppAttest.Verify
  ( AttestationCheck (..),
    VerifiedKey (..),
    AssertionCheck (..),
    verifyAttestation,
    verifyAssertion,
    appleAppAttestationRootCaPem,
    trustAnchorsFromPem,
    appIdHash,
  )
where

import AppAttestation.AppAttest.Encoding
import AppAttestation.Canonical (sha256)
import AppAttestation.Domain.Types.Common
import AppAttestation.Types (Reason (..))
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (os2ip)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Hourglass as Hourglass
import qualified Data.PEM as PEM
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.X509 as X509
import qualified Data.X509.CertificateStore as X509
import qualified Data.X509.Validation as X509
import Kernel.Prelude

data AttestationCheck = AttestationCheck
  { trustAnchors :: X509.CertificateStore,
    now :: UTCTime,
    keyId :: ByteString,
    clientDataHash :: ByteString,
    allowedApps :: [IosAppConfig]
  }

data VerifiedKey = VerifiedKey
  { publicKey :: ByteString,
    iosApp :: IosAppConfig,
    receipt :: Maybe ByteString
  }
  deriving (Show, Eq)

data AssertionCheck = AssertionCheck
  { publicKey :: ByteString,
    clientDataHash :: ByteString,
    teamId :: Text,
    bundleId :: Text,
    storedSignCount :: Int
  }

verifyAttestation :: AttestationCheck -> ByteString -> Either Reason VerifiedKey
verifyAttestation AttestationCheck {..} rawAttestation = do
  attestation <- note ATTESTATION_MALFORMED $ decodeAttestationObject rawAttestation
  unless (attestation.format == "apple-appattest") $ Left ATTESTATION_MALFORMED
  certificates <- note ATTESTATION_MALFORMED . traverse (rightToMaybe . X509.decodeSignedCertificate) $ attestation.certificateChain
  credentialCertificate <- note ATTESTATION_MALFORMED $ X509.getCertificate <$> listToMaybe certificates
  unless (null $ validateChain trustAnchors now certificates) $ Left CERT_CHAIN_INVALID

  let expectedNonce = sha256 $ attestation.authData <> clientDataHash
  unless (attestedNonce credentialCertificate == Just expectedNonce) $ Left NONCE_MISMATCH

  publicKey <- note ATTESTATION_MALFORMED $ uncompressedPoint credentialCertificate
  unless (sha256 publicKey == keyId) $ Left KEY_ID_MISMATCH

  authenticatorData <- note ATTESTATION_MALFORMED $ parseAuthenticatorData attestation.authData
  iosApp <- note APP_NOT_ALLOWED $ find (\candidate -> appIdHash candidate.teamId candidate.bundleId == authenticatorData.rpIdHash) allowedApps
  unless (authenticatorData.signCount == 0) $ Left COUNTER_NOT_ZERO

  credential <- note ATTESTATION_MALFORMED authenticatorData.attestedCredential
  unless (credential.aaguid == environmentAaguid iosApp.environment) $ Left ENVIRONMENT_MISMATCH
  unless (credential.credentialId == keyId) $ Left KEY_ID_MISMATCH
  pure VerifiedKey {receipt = attestation.receipt, ..}

verifyAssertion :: AssertionCheck -> ByteString -> Either Reason Int
verifyAssertion AssertionCheck {..} rawAssertion = do
  assertion <- note ASSERTION_MALFORMED $ decodeAssertionObject rawAssertion
  authenticatorData <- note ASSERTION_MALFORMED $ parseAuthenticatorData assertion.authenticatorData
  unless (authenticatorData.rpIdHash == appIdHash teamId bundleId) $ Left RP_ID_MISMATCH

  key <- note SIGNATURE_INVALID $ p256PublicKey publicKey
  signature <- note ASSERTION_MALFORMED $ decodeEcdsaSignature assertion.signature
  let nonce = sha256 $ assertion.authenticatorData <> clientDataHash
  unless (ECDSA.verify SHA256 key signature nonce) $ Left SIGNATURE_INVALID

  unless (authenticatorData.signCount > storedSignCount) $ Left COUNTER_NOT_INCREASING
  pure authenticatorData.signCount

appIdHash :: Text -> Text -> ByteString
appIdHash teamId bundleId = sha256 . TE.encodeUtf8 $ teamId <> "." <> bundleId

environmentAaguid :: AppAttestEnvironment -> ByteString
environmentAaguid = \case
  PRODUCTION -> "appattest" <> BS.replicate 7 0
  DEVELOPMENT -> "appattestdevelop"

validateChain :: X509.CertificateStore -> UTCTime -> [X509.SignedCertificate] -> [X509.FailedReason]
validateChain trustAnchors now certificates =
  X509.validatePure validationTime X509.defaultHooks checks trustAnchors ("", "") (X509.CertificateChain certificates)
  where
    validationTime = Hourglass.timeConvert . Hourglass.Elapsed . Hourglass.Seconds . floor $ utcTimeToPOSIXSeconds now
    checks = X509.defaultChecks {X509.checkFQHN = False}

attestedNonce :: X509.Certificate -> Maybe ByteString
attestedNonce certificate = do
  X509.Extensions mbExtensions <- Just $ X509.certExtensions certificate
  extension <- mbExtensions >>= find ((== nonceExtensionOid) . X509.extRawOID)
  elements <- rightToMaybe $ decodeASN1' DER (X509.extRawContent extension)
  listToMaybe [nonce | OctetString nonce <- elements]
  where
    nonceExtensionOid = [1, 2, 840, 113635, 100, 8, 2]

uncompressedPoint :: X509.Certificate -> Maybe ByteString
uncompressedPoint certificate = case X509.certPubKey certificate of
  X509.PubKeyEC (X509.PubKeyEC_Named ECC.SEC_p256r1 (X509.SerializedPoint point)) -> Just point
  _ -> Nothing

p256PublicKey :: ByteString -> Maybe ECDSA.PublicKey
p256PublicKey raw = do
  guard $ BS.length raw == 65 && BS.head raw == 4
  let curve = ECC.getCurveByName ECC.SEC_p256r1
      (x, y) = BS.splitAt 32 $ BS.drop 1 raw
      point = ECC.Point (os2ip x) (os2ip y)
  guard $ ECC.isPointValid curve point
  pure $ ECDSA.PublicKey curve point

decodeEcdsaSignature :: ByteString -> Maybe ECDSA.Signature
decodeEcdsaSignature raw = case decodeASN1' DER raw of
  Right [Start Sequence, IntVal r, IntVal s, End Sequence] -> Just $ ECDSA.Signature r s
  _ -> Nothing

trustAnchorsFromPem :: Text -> Maybe X509.CertificateStore
trustAnchorsFromPem pem = do
  blocks <- rightToMaybe . PEM.pemParseBS $ TE.encodeUtf8 pem
  certificates <- traverse (rightToMaybe . X509.decodeSignedCertificate . PEM.pemContent) blocks
  guard . not $ null certificates
  pure $ X509.makeCertificateStore certificates

note :: e -> Maybe a -> Either e a
note err = maybe (Left err) Right

appleAppAttestationRootCaPem :: Text
appleAppAttestationRootCaPem =
  "-----BEGIN CERTIFICATE-----\n\
  \MIICITCCAaegAwIBAgIQC/O+DvHN0uD7jG5yH2IXmDAKBggqhkjOPQQDAzBSMSYw\n\
  \JAYDVQQDDB1BcHBsZSBBcHAgQXR0ZXN0YXRpb24gUm9vdCBDQTETMBEGA1UECgwK\n\
  \QXBwbGUgSW5jLjETMBEGA1UECAwKQ2FsaWZvcm5pYTAeFw0yMDAzMTgxODMyNTNa\n\
  \Fw00NTAzMTUwMDAwMDBaMFIxJjAkBgNVBAMMHUFwcGxlIEFwcCBBdHRlc3RhdGlv\n\
  \biBSb290IENBMRMwEQYDVQQKDApBcHBsZSBJbmMuMRMwEQYDVQQIDApDYWxpZm9y\n\
  \bmlhMHYwEAYHKoZIzj0CAQYFK4EEACIDYgAERTHhmLW07ATaFQIEVwTtT4dyctdh\n\
  \NbJhFs/Ii2FdCgAHGbpphY3+d8qjuDngIN3WVhQUBHAoMeQ/cLiP1sOUtgjqK9au\n\
  \Yen1mMEvRq9Sk3Jm5X8U62H+xTD3FE9TgS41o0IwQDAPBgNVHRMBAf8EBTADAQH/\n\
  \MB0GA1UdDgQWBBSskRBTM72+aEH/pwyp5frq5eWKoTAOBgNVHQ8BAf8EBAMCAQYw\n\
  \CgYIKoZIzj0EAwMDaAAwZQIwQgFGnByvsiVbpTKwSga0kP0e8EeDS4+sQmTvb7vn\n\
  \53O5+FRXgeLhpJ06ysC5PrOyAjEAp5U4xDgEgllF7En3VcE3iexZZtKeYnpqtijV\n\
  \oyFraWVIyd/dganmrduC1bmTBGwD\n\
  \-----END CERTIFICATE-----\n"
