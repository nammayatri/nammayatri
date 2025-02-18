module ExternalBPP.ExternalAPI.Direct.Utils where

import Crypto.Cipher.TripleDES
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.Error (CryptoFailable (..))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Base64
import Kernel.Utils.Common
import Tools.Error

generateQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> TicketPayload -> m Text
generateQR config ticket = do
  let cipherKey = config.cipherKey
      qrData = encodeToText ticket
  encryptedQR <- encryptDES cipherKey qrData & fromEitherM (\err -> InternalError $ "Failed to encrypt: " <> show err)
  return encryptedQR
  where
    encryptDES :: Base64 -> Text -> Either String Text
    encryptDES (Base64 cipherKey) plainText = do
      let cipherEither = cipherInit cipherKey :: CryptoFailable DES_EDE3
      case cipherEither of
        CryptoPassed cipher ->
          let paddedPlainText = pad (PKCS7 (blockSize cipher)) (TE.encodeUtf8 plainText)
           in Right $ TE.decodeUtf8 $ Base64.encode $ ecbEncrypt cipher paddedPlainText
        CryptoFailed err -> Left $ show err

decodeQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> Text -> m TicketPayload
decodeQR config encryptedQrData = do
  let cipherKey = config.cipherKey
  decryptedQR <- decryptDES cipherKey encryptedQrData & fromEitherM (\err -> InternalError $ "Failed to decrypt: " <> show err)
  ticket :: TicketPayload <- decodeFromText decryptedQR & fromMaybeM (InternalError "Could not decode the QR!")
  return ticket
  where
    decryptDES :: Base64 -> Text -> Either String Text
    decryptDES (Base64 cipherKey) encryptedText = do
      let cipherEither = cipherInit cipherKey :: CryptoFailable DES_EDE3
      case cipherEither of
        CryptoPassed cipher -> do
          let decodedCipherText = Base64.decodeLenient (TE.encodeUtf8 encryptedText)
              decryptedPaddedText = ecbDecrypt cipher decodedCipherText
              decryptedText = unpad (PKCS7 (blockSize cipher)) decryptedPaddedText
          case decryptedText of
            Just decryptText -> Right $ TE.decodeUtf8 decryptText
            Nothing -> Left "Could not decrypt!"
        CryptoFailed err -> Left $ show err

refreshQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> Text -> m (Maybe (Text, UTCTime))
refreshQR config encryptedQrData = do
  ticket :: TicketPayload <- decodeQR config encryptedQrData
  now <- getCurrentTime
  let mbRefreshAt = config.qrRefreshTtl <&> (\ttl -> addUTCTime (secondsToNominalDiffTime ttl) now)
  case (ticket.refreshAt, mbRefreshAt) of
    (Just refreshTime, Just refreshAt) ->
      if now > refreshTime
        then do
          qrData <- generateQR config $ ticket {refreshAt = Just refreshAt}
          pure $ Just (qrData, refreshAt)
        else pure Nothing
    _ -> pure Nothing
