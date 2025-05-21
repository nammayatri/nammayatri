module ExternalBPP.ExternalAPI.Direct.Utils where

import Crypto.Error as Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import Data.ByteString hiding (length)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.IntegratedBPPConfig
import ExternalBPP.ExternalAPI.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Base64
import Kernel.Utils.Common
import Tools.Error

qrColorHex :: [Text]
qrColorHex =
  [ "#4A148C", -- Deep Purple
    "#FFFFFF", -- White
    "#FF6F00", -- Amber
    "#006064", -- Cyan
    "#D81B60", -- Pink
    "#1B5E20", -- Green
    "#E65100" -- Deep Orange
  ]

generateTimeBasedOTP :: (MonadTime m, MonadFlow m) => m Int
generateTimeBasedOTP = do
  now <- addUTCTime (secondsToNominalDiffTime 19800) <$> getCurrentTime
  let epochSeconds = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds now
      -- Get the number of 24-hour periods since epoch
      daySeed = fromIntegral @_ @Integer $ epochSeconds.getSeconds `div` (24 * 60 * 60)
      -- Use a simple hash function that will work consistently across languages
      -- Multiply by a prime number and take modulo 9000 to get a 4-digit number
      otp = fromIntegral @Integer @Int $ ((daySeed * 9973) `mod` 9999) + 1000 -- Ensures 4-digit number between 1000-9999
  return otp

-- | Sign a request given the key
sign :: Base64 -> Text -> Either Crypto.CryptoError ByteString
sign (Base64 privateKey) msg =
  let sk = Ed25519.secretKey privateKey
      pk = Ed25519.toPublic <$> sk
      signature = Ed25519.sign <$> sk <*> pk <*> pure (TE.encodeUtf8 msg)
   in Crypto.eitherCryptoError $ BA.convert <$> signature

verify :: Base64 -> Text -> ByteString -> Either Crypto.CryptoError Bool
verify (Base64 privateKey) msg signatureBs =
  let sk = Ed25519.secretKey privateKey
      pk = Ed25519.toPublic <$> sk
      signature = Ed25519.signature signatureBs
   in Crypto.eitherCryptoError $ Ed25519.verify <$> pk <*> pure (TE.encodeUtf8 msg) <*> signature

generateQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> TicketPayload -> m Text
generateQR config ticket = do
  let secretKey = config.cipherKey
      qrData = ticket.ticketNumber <> "," <> ticket.fromRouteProviderCode <> "," <> ticket.toRouteProviderCode <> "," <> show ticket.adultQuantity <> "," <> show ticket.childQuantity <> "," <> ticket.vehicleTypeProviderCode <> "," <> show ticket.ticketAmount.getMoney <> "," <> maybe "" show ticket.otpCode <> "," <> maybe "" (\otpCode -> qrColorHex !! (otpCode `mod` length qrColorHex)) ticket.otpCode <> "," <> ticket.expiry <> "," <> maybe "" show ticket.refreshAt
  signature <- sign secretKey qrData & fromEitherM (\err -> InternalError $ "Failed to encrypt: " <> show err)
  return $ (TE.decodeUtf8 $ Base64.encode signature) <> "," <> qrData

decodeQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> Text -> m TicketPayload
decodeQR config signatureAndQrData = do
  let secretKey = config.cipherKey
  signature <- Base64.decode (TE.encodeUtf8 $ T.takeWhile (/= ',') signatureAndQrData) & fromEitherM (\err -> InternalError $ "Failed to decode signature to base64: " <> show err)
  let qrData = T.drop 1 $ T.dropWhile (/= ',') signatureAndQrData
  isVerified <- verify secretKey qrData signature & fromEitherM (\err -> InternalError $ "Failed to decrypt: " <> show err)
  unless isVerified $ throwError (InvalidRequest "Invalid QR data")
  (ticketNumber, fromRouteProviderCode, toRouteProviderCode, adultQuantity', childQuantity', vehicleTypeProviderCode, ticketAmount', otpCode', _colorCode, expiry, refreshAt') <-
    case T.strip <$> T.splitOn "," qrData of
      [ticketNumber, fromRouteProviderCode, toRouteProviderCode, adultQuantity', childQuantity', vehicleTypeProviderCode, ticketAmount', otpCode', colorCode, expiry, refreshAt'] -> pure (ticketNumber, fromRouteProviderCode, toRouteProviderCode, adultQuantity', childQuantity', vehicleTypeProviderCode, ticketAmount', otpCode', colorCode, expiry, refreshAt')
      _ -> throwError (InvalidRequest "Unable to decode QR data")
  adultQuantity :: Int <- (readMaybe . T.unpack $ adultQuantity') & fromMaybeM (InvalidRequest "Unable to decode adult quantity")
  childQuantity :: Int <- (readMaybe . T.unpack $ childQuantity') & fromMaybeM (InvalidRequest "Unable to decode child quantity")
  ticketAmount :: Money <- (readMaybe . T.unpack $ ticketAmount') & fromMaybeM (InvalidRequest "Unable to decode ticket amount")
  let refreshAt :: Maybe UTCTime = readMaybe . T.unpack $ refreshAt'
      otpCode :: Maybe Int = readMaybe . T.unpack $ otpCode'
  return $
    TicketPayload
      { ..
      }

refreshQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> Text -> m (Maybe (Text, UTCTime))
refreshQR config signatureAndQrData = do
  ticket :: TicketPayload <- decodeQR config signatureAndQrData
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
