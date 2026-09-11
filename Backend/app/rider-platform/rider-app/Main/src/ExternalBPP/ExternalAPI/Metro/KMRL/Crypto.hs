module ExternalBPP.ExternalAPI.Metro.KMRL.Crypto
  ( encryptAndSign,
    verifyAndDecrypt,
    publicKeyFromCertPem,
    privateKeyFromPem,
    RSAPublicKey,
    RSAPrivateKey,
  )
where

import qualified Codec.Compression.Zlib.Raw as Raw
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), aeadAppendHeader, aeadDecrypt, aeadEncrypt, aeadFinalize, aeadInit, cipherInit)
import Crypto.Error (eitherCryptoError)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.OAEP as OAEP
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.X509 (PrivKey (..), PubKey (..), SignedCertificate, certPubKey, getCertificate)
import qualified Data.X509.Memory as X509Mem
import Kernel.Prelude

mapLeft :: (a -> Text) -> Either a b -> Either Text b
mapLeft f = either (Left . f) Right

b64 :: BS.ByteString -> BS.ByteString
b64 = BS.takeWhile (/= 61) . B64U.encode

unb64 :: BS.ByteString -> Either Text BS.ByteString
unb64 s =
  mapLeft toText $
    B64U.decode (s <> BS.replicate ((4 - BS.length s `mod` 4) `mod` 4) 61)

jweHeader :: BS.ByteString
jweHeader = "{\"alg\":\"RSA-OAEP-256\",\"enc\":\"A256GCM\",\"zip\":\"DEF\"}"

jwsHeader :: BS.ByteString
jwsHeader = "{\"alg\":\"RS256\"}"

encryptAndSign ::
  (MonadRandom m) =>
  RSA.PublicKey ->
  RSA.PrivateKey ->
  BS.ByteString ->
  m (Either Text BS.ByteString)
encryptAndSign pub priv payload = do
  cek <- getRandomBytes 32
  iv <- getRandomBytes 12
  encKeyE <- OAEP.encrypt (OAEP.defaultOAEPParams SHA256) pub cek
  pure $ do
    encKey <- mapLeft show encKeyE
    let protectedB64 = b64 jweHeader
        deflated = BL.toStrict . Raw.compress . BL.fromStrict $ payload
    cipher <- mapLeft show . eitherCryptoError $ cipherInit cek
    aead <- mapLeft show . eitherCryptoError $ aeadInit AEAD_GCM (cipher :: AES256) (iv :: BS.ByteString)
    let (ct, aeadFinal) = aeadEncrypt (aeadAppendHeader aead protectedB64) deflated
        tag = convert (aeadFinalize aeadFinal 16) :: BS.ByteString
        jweCompact = BS.intercalate "." [protectedB64, b64 encKey, b64 iv, b64 ct, b64 tag]
        signingInput = b64 jwsHeader <> "." <> b64 jweCompact
    sig <- mapLeft show $ PKCS15.sign Nothing (Just SHA256) priv signingInput
    pure (signingInput <> "." <> b64 sig)

verifyAndDecrypt ::
  RSA.PublicKey ->
  RSA.PrivateKey ->
  BS.ByteString ->
  Either Text BS.ByteString
verifyAndDecrypt pub priv token = do
  (jwsH, jwsPayloadB64, jwsSig) <- case C8.split '.' token of
    [a, b, c] -> Right (a, b, c)
    parts -> Left ("JWS must have 3 parts, got " <> show (length parts))
  sig <- unb64 jwsSig
  unless (PKCS15.verify (Just SHA256) pub (jwsH <> "." <> jwsPayloadB64) sig) $
    Left "JWS signature verification failed"
  jweCompact <- unb64 jwsPayloadB64
  (protectedB64, encKeyB64, ivB64, ctB64, tagB64) <- case C8.split '.' jweCompact of
    [a, b, c, d, e] -> Right (a, b, c, d, e)
    parts -> Left ("JWE must have 5 parts, got " <> show (length parts))
  encKey <- unb64 encKeyB64
  iv <- unb64 ivB64
  ct <- unb64 ctB64
  tag <- unb64 tagB64
  cek <- mapLeft show $ OAEP.decrypt Nothing (OAEP.defaultOAEPParams SHA256) priv encKey
  cipher <- mapLeft show . eitherCryptoError $ cipherInit cek
  aead <- mapLeft show . eitherCryptoError $ aeadInit AEAD_GCM (cipher :: AES256) iv
  let (plain, aeadFinal) = aeadDecrypt (aeadAppendHeader aead protectedB64) ct
      expected = convert (aeadFinalize aeadFinal 16) :: BS.ByteString
  unless (expected == tag) $ Left "GCM authentication tag mismatch"
  pure (BL.toStrict . Raw.decompress . BL.fromStrict $ plain)

type RSAPublicKey = RSA.PublicKey

type RSAPrivateKey = RSA.PrivateKey

publicKeyFromCertPem :: BS.ByteString -> Either Text RSAPublicKey
publicKeyFromCertPem pem =
  case X509Mem.readSignedObjectFromMemory pem :: [SignedCertificate] of
    [] -> Left "operator certificate PEM contained no certificate"
    (cert : _) -> case certPubKey (getCertificate cert) of
      PubKeyRSA pub -> Right pub
      other -> Left ("operator certificate holds a " <> show (pubKeyKind other) <> " key, expected RSA")
  where
    pubKeyKind = \case
      PubKeyRSA _ -> "RSA" :: Text
      PubKeyDSA _ -> "DSA"
      PubKeyEC _ -> "EC"
      PubKeyEd25519 _ -> "Ed25519"
      _ -> "unrecognised"

privateKeyFromPem :: BS.ByteString -> Either Text RSAPrivateKey
privateKeyFromPem pem =
  case X509Mem.readKeyFileFromMemory pem of
    [] -> Left "private key PEM contained no key"
    keys -> case [k | PrivKeyRSA k <- keys] of
      (k : _) -> Right k
      [] -> Left "private key PEM held a non-RSA key"
