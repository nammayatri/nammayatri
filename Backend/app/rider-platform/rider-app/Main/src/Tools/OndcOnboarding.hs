{-# LANGUAGE OverloadedStrings #-}

module Tools.OndcOnboarding
  ( decryptChallenge,
    signRequestId,
    x25519PrivateFromDer,
    x25519PublicFromDer,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cipherInit, ecbDecrypt)
import Crypto.Error (eitherCryptoError)
import qualified Crypto.PubKey.Curve25519 as X25519
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import Kernel.Types.Base64 (Base64 (..))

-- X25519 keys arrive base64'd and DER-wrapped, the shape openssl emits for
-- -algorithm X25519. Both encodings are fixed length with a constant prefix, so the
-- prefix is checked rather than the trailing bytes taken on faith: a key for another
-- algorithm would otherwise be sliced into 32 meaningless bytes and fail later as a
-- wrong answer instead of a wrong key.
pkcs8X25519Prefix :: ByteString
pkcs8X25519Prefix = BS.pack [0x30, 0x2e, 0x02, 0x01, 0x00, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x6e, 0x04, 0x22, 0x04, 0x20]

spkiX25519Prefix :: ByteString
spkiX25519Prefix = BS.pack [0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x6e, 0x03, 0x21, 0x00]

rawAfterPrefix :: Text -> ByteString -> ByteString -> Either Text ByteString
rawAfterPrefix what prefix der
  | not (prefix `BS.isPrefixOf` der) = Left (what <> ": not a DER-encoded X25519 key")
  | BS.length raw /= 32 = Left (what <> ": expected 32 key bytes, got " <> T.pack (show (BS.length raw)))
  | otherwise = Right raw
  where
    raw = BS.drop (BS.length prefix) der

decodeB64 :: Text -> Text -> Either Text ByteString
decodeB64 what = first (\e -> what <> ": " <> T.pack e) . B64.decode . TE.encodeUtf8 . T.strip

x25519PrivateFromDer :: Text -> Either Text X25519.SecretKey
x25519PrivateFromDer b64 = do
  der <- decodeB64 "encryption private key" b64
  raw <- rawAfterPrefix "encryption private key" pkcs8X25519Prefix der
  first (T.pack . show) (eitherCryptoError (X25519.secretKey raw))

x25519PublicFromDer :: Text -> Either Text X25519.PublicKey
x25519PublicFromDer b64 = do
  der <- decodeB64 "registry public key" b64
  raw <- rawAfterPrefix "registry public key" spkiX25519Prefix der
  first (T.pack . show) (eitherCryptoError (X25519.publicKey raw))

-- | Answer the registry's subscribe challenge.
--
-- The shared secret is X25519 between our encryption key and the registry's, and the
-- challenge is AES-ECB under it. The plaintext is a UUID; ECB leaves whole blocks, so
-- the tail past 36 characters is padding. Go slices [:36] with the decrypt error
-- discarded, which turns a wrong key into an index-out-of-range rather than a message.
decryptChallenge :: Text -> Text -> Text -> Either Text Text
decryptChallenge privB64 pubB64 challengeB64 = do
  priv <- x25519PrivateFromDer privB64
  pub <- x25519PublicFromDer pubB64
  cipherText <- decodeB64 "challenge" challengeB64
  when (BS.null cipherText || BS.length cipherText `mod` 16 /= 0) $
    Left ("challenge is not a whole number of AES blocks: " <> T.pack (show (BS.length cipherText)))
  let secret = convert (X25519.dh pub priv) :: ByteString
  aes <- first (T.pack . show) (eitherCryptoError (cipherInit secret)) :: Either Text AES256
  let plain = C8.unpack (ecbDecrypt aes cipherText)
      answer = take 36 plain
  -- A wrong key decrypts to 36 bytes of noise rather than failing, and we would hand
  -- that to the registry as our answer. The plaintext is always a uuid, so shape-check
  -- it and report the key instead of echoing rubbish.
  if length answer == 36 && looksLikeUuid answer
    then Right (T.pack answer)
    else Left "decrypted challenge is not a uuid; the encryption key is probably wrong"

looksLikeUuid :: String -> Bool
looksLikeUuid s =
  map length (splitOnDash s) == [8, 4, 4, 4, 12]
    && all (`elem` ("0123456789abcdefABCDEF-" :: String)) s
  where
    splitOnDash = foldr step [[]]
    step '-' acc = [] : acc
    step c (cur : rest) = (c : cur) : rest
    step _ [] = []

-- | The content of the ondc-site-verification meta tag: our Ed25519 signature over the
-- request id the registry sent, base64. Proves control of the domain.
signRequestId :: Base64 -> Text -> Either Text Text
signRequestId (Base64 key) requestId =
  case eitherCryptoError (Ed25519.secretKey key) of
    Left err -> Left ("signing key is not Ed25519: " <> T.pack (show err))
    Right sk ->
      let sig = Ed25519.sign sk (Ed25519.toPublic sk) (TE.encodeUtf8 requestId)
       in Right (TE.decodeUtf8 (B64.encode (convert sig)))
