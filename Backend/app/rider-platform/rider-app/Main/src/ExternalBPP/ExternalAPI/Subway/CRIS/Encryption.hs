module ExternalBPP.ExternalAPI.Subway.CRIS.Encryption
  ( encryptPayload,
    decryptResponseData,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISError (..))
import Kernel.Utils.Common

cipherInit :: Text -> Either String AES.AES256
cipherInit clientKey = do
  let keyBS = TE.encodeUtf8 clientKey
  case CT.cipherInit keyBS :: CE.CryptoFailable AES.AES256 of
    CE.CryptoPassed a -> Right a
    CE.CryptoFailed err -> Left $ "Cipher initialization failed: " <> show err

-- Add PKCS7 padding function
pkcs7Pad :: ByteString -> ByteString
pkcs7Pad input =
  let blockSize = 16
      paddingLength = blockSize - (BS.length input `mod` blockSize)
      padding = BS.replicate paddingLength (fromIntegral paddingLength)
   in input <> padding

-- Add PKCS7 unpadding function
pkcs7Unpad :: ByteString -> Maybe ByteString
pkcs7Unpad bs
  | BS.null bs = Nothing
  | otherwise = do
    let lastByte = BS.last bs
        padLength = fromIntegral lastByte
    if padLength <= 0 || padLength > 16 || BS.length bs < padLength
      then Nothing
      else do
        let padding = BS.replicate padLength lastByte
            (content, actualPadding) = BS.splitAt (BS.length bs - padLength) bs
        if actualPadding == padding
          then Just content
          else Nothing

-- Function to encrypt the request payload
encryptPayload :: (MonadFlow m) => Text -> Text -> m Text
encryptPayload jsonStr clientKey = do
  logInfo $ "Exact JSON before encryption: " <> jsonStr
  let eitherCipher = cipherInit clientKey
  case eitherCipher of
    Left err -> throwError $ CRISError (show err)
    Right cipher -> do
      let plaintext = TE.encodeUtf8 jsonStr
          paddedPlaintext = pkcs7Pad plaintext
          encrypted = CT.ecbEncrypt cipher paddedPlaintext
          encryptedBase64 = decodeUtf8 $ B64.encode encrypted

      logInfo $ "Encrypted Payload: " <> encryptedBase64
      pure encryptedBase64

-- Function to decrypt response data
decryptResponseData :: Text -> Text -> Either String Text
decryptResponseData encryptedText clientKey = do
  cipher <- cipherInit clientKey
  decodedData <- case B64.decode $ encodeUtf8 encryptedText of
    Right d -> Right d
    Left err -> Left $ "Base64 decode failed: " <> show err

  -- Decrypt
  let decrypted = CT.ecbDecrypt cipher decodedData
  -- Remove PKCS7 padding
  let unpadded = pkcs7Unpad decrypted
  case unpadded of
    Nothing -> Left "Failed to unpad decrypted data"
    Just unpaddedData ->
      case decodeUtf8' unpaddedData of
        Left err -> Left $ "UTF-8 decode failed: " <> show err
        Right text -> Right text
