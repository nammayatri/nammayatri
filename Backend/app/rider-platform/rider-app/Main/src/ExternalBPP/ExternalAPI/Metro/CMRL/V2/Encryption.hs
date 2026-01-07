{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Encryption
  ( encryptPayload,
    decryptPayload,
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
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Error (CMRLV2Error (CMRLV2InternalError))
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
initCipher clientKey = do
  let keyBytes = TE.encodeUtf8 clientKey
  if BS.length keyBytes /= 32
    then Left "AES-256 requires a 32-byte (256-bit) key"
    else case CT.cipherInit keyBytes :: CE.CryptoFailable AES.AES256 of
      CE.CryptoPassed cipher -> Right cipher
      CE.CryptoFailed err -> Left $ "Cipher init failed: " <> show err

encryptPayload :: (MonadFlow m) => Text -> Text -> m Text
encryptPayload plaintext clientKey = do
  cipher <- case initCipher clientKey of
    Left err -> throwError $ CMRLV2InternalError (T.pack err)
    Right c -> pure c

  ivBytes <- liftIO $ getRandomBytes 16
  iv <- case CT.makeIV ivBytes of
    Nothing -> throwError $ CMRLV2InternalError "Failed to create IV"
    Just v -> pure v

  let padded = pkcs7Pad (TE.encodeUtf8 plaintext)
      ciphertext = CT.cbcEncrypt cipher iv padded
      finalBytes = ivBytes <> ciphertext
      encoded = TE.decodeUtf8 (B64.encode finalBytes)

  logInfo $ "Encrypted payload generated successfully"
  pure encoded

decryptPayload :: Text -> Text -> Either String Text
decryptPayload encryptedText clientKey = do
  cipher <- initCipher clientKey

  raw <- case B64.decode (TE.encodeUtf8 encryptedText) of
    Left err -> Left $ "Base64 decode failed: " <> err
    Right b -> Right b

  if BS.length raw < 16
    then Left "Ciphertext too short"
    else do
      let (ivBytes, ciphertext) = BS.splitAt 16 raw
      iv <- case CT.makeIV ivBytes of
        Nothing -> Left "Invalid IV"
        Just v -> Right v

      let decrypted = CT.cbcDecrypt cipher iv ciphertext
      unpadded <- pkcs7Unpad decrypted

      case TE.decodeUtf8' unpadded of
        Left err -> Left $ "UTF-8 decode failed: " <> show err
        Right txt -> Right txt
