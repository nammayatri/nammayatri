{-# LANGUAGE OverloadedStrings #-}

-- | Single home for BHIM's AES envelope crypto. Working assumption (confirm
-- with BHIM before prod): AES-256-CBC, random 16-byte IV prepended to the
-- ciphertext, base64 over (IV ‖ ciphertext), PKCS7 padding, key = BHIM AES key.
-- A change of mode/IV/padding should only touch this module.
module PartnerAuth.BHIM.Encryption
  ( encryptEnvelope,
    encryptEnvelopeWithIV,
    decryptEnvelope,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

pkcs7Pad :: BS.ByteString -> BS.ByteString
pkcs7Pad input =
  let blockSize = 16
      padLen = blockSize - (BS.length input `mod` blockSize)
   in input <> BS.replicate padLen (fromIntegral padLen)

pkcs7Unpad :: BS.ByteString -> Either String BS.ByteString
pkcs7Unpad bs
  | BS.null bs = Left "Empty plaintext"
  | otherwise =
    let padLen = fromIntegral (BS.last bs)
        len = BS.length bs
     in if padLen <= 0 || padLen > 16 || padLen > len
          then Left "Invalid PKCS7 padding length"
          else
            let (content, padding) = BS.splitAt (len - padLen) bs
             in if BS.all (== fromIntegral padLen) padding
                  then Right content
                  else Left "Invalid PKCS7 padding"

initCipher :: Text -> Either String AES.AES256
initCipher key = do
  let keyBytes = TE.encodeUtf8 key
  if BS.length keyBytes /= 32
    then Left "AES-256 requires a 32-byte (256-bit) key"
    else case CT.cipherInit keyBytes :: CE.CryptoFailable AES.AES256 of
      CE.CryptoPassed cipher -> Right cipher
      CE.CryptoFailed err -> Left $ "Cipher init failed: " <> show err

-- | Pure core: AES-256-CBC encrypt with a caller-supplied 16-byte IV.
-- Returns base64(IV ‖ ciphertext). Deterministic — used by tests.
encryptEnvelopeWithIV :: Text -> BS.ByteString -> Text -> Either String Text
encryptEnvelopeWithIV key ivBytes plaintext = do
  cipher <- initCipher key
  iv <- maybe (Left "Invalid IV (need 16 bytes)") Right (CT.makeIV ivBytes)
  let padded = pkcs7Pad (TE.encodeUtf8 plaintext)
      ciphertext = CT.cbcEncrypt cipher iv padded
  Right $ TE.decodeUtf8 (B64.encode (ivBytes <> ciphertext))

-- | Encrypt with a fresh random 16-byte IV (prepended), base64-encoded.
encryptEnvelope :: (MonadFlow m) => Text -> Text -> m Text
encryptEnvelope key plaintext = do
  ivBytes :: BS.ByteString <- liftIO (getRandomBytes 16)
  case encryptEnvelopeWithIV key ivBytes plaintext of
    Left err -> throwError $ InternalError ("PartnerAuth.BHIM AES encrypt failed: " <> T.pack err)
    Right enc -> pure enc

-- | Decrypt base64(IV ‖ ciphertext). Pure.
decryptEnvelope :: Text -> Text -> Either String Text
decryptEnvelope key encryptedText = do
  cipher <- initCipher key
  raw <- case B64.decode (TE.encodeUtf8 encryptedText) of
    Left err -> Left $ "Base64 decode failed: " <> err
    Right b -> Right b
  when (BS.length raw < 16) $ Left "Ciphertext too short (missing IV)"
  let (ivBytes, ciphertext) = BS.splitAt 16 raw
  iv <- maybe (Left "Invalid IV") Right (CT.makeIV ivBytes)
  let decrypted = CT.cbcDecrypt cipher iv ciphertext
  unpadded <- pkcs7Unpad decrypted
  case TE.decodeUtf8' unpadded of
    Left err -> Left $ "UTF-8 decode failed: " <> show err
    Right txt -> Right txt
