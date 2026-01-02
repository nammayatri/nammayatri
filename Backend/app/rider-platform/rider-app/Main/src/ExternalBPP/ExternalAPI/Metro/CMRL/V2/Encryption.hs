{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Encryption where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error

-- PKCS5 padding (same as PKCS7 for AES block size of 16)
pkcs5Pad :: BS.ByteString -> BS.ByteString
pkcs5Pad bs =
  let blockSize = 16
      paddingLen = blockSize - (BS.length bs `mod` blockSize)
      padding = BS.replicate paddingLen (fromIntegral paddingLen)
   in bs <> padding

-- Remove PKCS5 padding
pkcs5Unpad :: BS.ByteString -> Either Text BS.ByteString
pkcs5Unpad bs
  | BS.null bs = Left "Empty bytestring"
  | otherwise =
    let lastByte = BS.last bs
        paddingLen = fromIntegral lastByte
     in if paddingLen > 0 && paddingLen <= 16 && paddingLen <= BS.length bs
          then
            let (content, padding) = BS.splitAt (BS.length bs - paddingLen) bs
             in if BS.all (== lastByte) padding
                  then Right content
                  else Left "Invalid padding"
          else Left "Invalid padding length"

-- Encrypt payload using AES-256-CBC with PKCS5 padding
encryptPayload :: (MonadFlow m, EncFlow m r, MonadReader r m) => CMRLV2Config -> BL.ByteString -> m Text
encryptPayload config payload = do
  encKey <- decrypt config.encryptionKey
  let keyBytes = TE.encodeUtf8 encKey
      key = BS.take 32 keyBytes -- AES-256 requires 32 bytes
      payloadBytes = BL.toStrict payload
      paddedPayload = pkcs5Pad payloadBytes

  -- Generate random IV (16 bytes for AES)
  iv <- liftIO $ getRandomBytes 16

  case ( CE.eitherCryptoError $ CT.cipherInit key :: Either CE.CryptoError AES.AES256,
         CT.makeIV iv
       ) of
    (Right cipher, Just initIV) -> do
      let encrypted = CT.cbcEncrypt cipher initIV paddedPayload
          -- Prepend IV to encrypted data (common practice)
          encryptedWithIV = iv <> encrypted
          encoded = B64.encode encryptedWithIV
      return $ TE.decodeUtf8 encoded
    _ -> throwError $ InternalError "Failed to initialize AES cipher for encryption"

-- Decrypt response using AES-256-CBC with PKCS5 padding
decryptPayload :: (MonadFlow m, EncFlow m r, MonadReader r m) => CMRLV2Config -> Text -> m BL.ByteString
decryptPayload config encryptedText = do
  encKey <- decrypt config.encryptionKey
  let keyBytes = TE.encodeUtf8 encKey
      key = BS.take 32 keyBytes -- AES-256 requires 32 bytes
  case B64.decode (TE.encodeUtf8 encryptedText) of
    Left err -> throwError $ InternalError $ "Base64 decode failed: " <> T.pack err
    Right encryptedWithIV -> do
      let (iv, encrypted) = BS.splitAt 16 encryptedWithIV

      case ( CE.eitherCryptoError $ CT.cipherInit key :: Either CE.CryptoError AES.AES256,
             CT.makeIV iv
           ) of
        (Right cipher, Just initIV) -> do
          let decrypted = CT.cbcDecrypt cipher initIV encrypted
          case pkcs5Unpad decrypted of
            Right unpadded -> return $ BL.fromStrict unpadded
            Left err -> throwError $ InternalError $ "PKCS5 unpad failed: " <> err
        _ -> throwError $ InternalError "Failed to initialize AES cipher for decryption"
