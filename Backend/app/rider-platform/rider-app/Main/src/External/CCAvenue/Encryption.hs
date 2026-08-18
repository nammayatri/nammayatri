-- | CCAvenue's @enc_request@ / @enc_response@ scheme.
--
-- CCAvenue does not use a standard envelope: the AES-128 key is the raw MD5 digest
-- of the merchant working key, the IV is the fixed byte string @00 01 .. 0f@, the
-- mode is CBC with PKCS#7 padding, and the ciphertext travels as lowercase hex.
-- All four of those are dictated by the gateway, so none of them is configurable.
module External.CCAvenue.Encryption
  ( encryptRequest,
    decryptResponse,
    parseFormFields,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude

-- | AES-128 key: the 16 raw bytes of MD5(workingKey), not its hex form.
cipherInit :: Text -> Either Text AES.AES128
cipherInit workingKey = do
  let keyBS = BA.convert (Hash.hash (TE.encodeUtf8 workingKey) :: Hash.Digest Hash.MD5) :: ByteString
  case CT.cipherInit keyBS :: CE.CryptoFailable AES.AES128 of
    CE.CryptoPassed cipher -> Right cipher
    CE.CryptoFailed err -> Left $ "CCAvenue cipher init failed: " <> show err

fixedIV :: AES.AES128 -> Either Text (CT.IV AES.AES128)
fixedIV _ =
  maybe (Left "CCAvenue IV construction failed") Right $
    CT.makeIV (BS.pack [0 .. 15])

pkcs7Pad :: ByteString -> ByteString
pkcs7Pad input =
  let padLength = 16 - (BS.length input `mod` 16)
   in input <> BS.replicate padLength (fromIntegral padLength)

pkcs7Unpad :: ByteString -> Maybe ByteString
pkcs7Unpad bs
  | BS.null bs = Nothing
  | otherwise =
    let padLength = fromIntegral (BS.last bs)
        (content, actualPadding) = BS.splitAt (BS.length bs - padLength) bs
     in if padLength <= 0 || padLength > 16 || BS.length bs < padLength
          then Nothing
          else
            if actualPadding == BS.replicate padLength (BS.last bs)
              then Just content
              else Nothing

-- | Plaintext JSON -> lowercase hex ciphertext for the @enc_request@ form field.
encryptRequest :: Text -> Text -> Either Text Text
encryptRequest workingKey plainText = do
  cipher <- cipherInit workingKey
  iv <- fixedIV cipher
  pure . TE.decodeUtf8 . convertToBase Base16 . CT.cbcEncrypt cipher iv . pkcs7Pad $ TE.encodeUtf8 plainText

-- | Hex ciphertext from @enc_response@ -> plaintext JSON.
decryptResponse :: Text -> Text -> Either Text Text
decryptResponse workingKey cipherHex = do
  cipher <- cipherInit workingKey
  iv <- fixedIV cipher
  cipherBytes <- case convertFromBase Base16 (TE.encodeUtf8 (T.strip cipherHex)) of
    Right (bytes :: ByteString) -> Right bytes
    Left err -> Left $ "CCAvenue enc_response is not valid hex: " <> T.pack err
  padded <-
    if BS.null cipherBytes || BS.length cipherBytes `mod` 16 /= 0
      then Left "CCAvenue enc_response length is not a multiple of the AES block size"
      else Right $ CT.cbcDecrypt cipher iv cipherBytes
  plainBytes <- maybe (Left "CCAvenue enc_response failed PKCS#7 unpadding") Right $ pkcs7Unpad padded
  first (\err -> "CCAvenue enc_response is not valid UTF-8: " <> show err) $ TE.decodeUtf8' plainBytes

parseFormFields :: Text -> [(Text, Text)]
parseFormFields body =
  [ (T.strip key, T.strip (T.drop 1 rest))
    | part <- T.splitOn "&" body,
      let (key, rest) = T.breakOn "=" part,
      not (T.null (T.strip key))
  ]
