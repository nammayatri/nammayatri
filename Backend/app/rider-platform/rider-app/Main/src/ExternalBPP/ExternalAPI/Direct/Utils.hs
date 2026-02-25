module ExternalBPP.ExternalAPI.Direct.Utils where

import Crypto.Error as Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import Data.ByteString hiding (length)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, parseTimeM)
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
      qrData = ticket.ticketNumber <> "," <> ticket.fromRouteProviderCode <> "," <> ticket.toRouteProviderCode <> "," <> show ticket.adultQuantity <> "," <> show ticket.childQuantity <> "," <> ticket.vehicleTypeProviderCode <> "," <> show ticket.ticketAmount.getMoney <> "," <> maybe "" show ticket.otpCode <> "," <> maybe "" (\otpCode -> qrColorHex !! (otpCode `mod` length qrColorHex)) ticket.otpCode <> "," <> ticket.expiry <> "," <> maybe "" show ticket.refreshAt <> "," <> fromMaybe "" ticket.fleetNo <> "," <> fromMaybe "" (T.intercalate "|" <$> ticket.seatLabels)
  signature <- sign secretKey qrData & fromEitherM (\err -> InternalError $ "Failed to encrypt: " <> show err)
  return $ (TE.decodeUtf8 $ Base64.encode signature) <> "," <> qrData

decodeQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DIRECTConfig -> Text -> m TicketPayload
decodeQR config signatureAndQrData = do
  let secretKey = config.cipherKey
  signature <- Base64.decode (TE.encodeUtf8 $ T.takeWhile (/= ',') signatureAndQrData) & fromEitherM (\err -> InternalError $ "Failed to decode signature to base64: " <> show err)
  let qrData = T.drop 1 $ T.dropWhile (/= ',') signatureAndQrData
  isVerified <- verify secretKey qrData signature & fromEitherM (\err -> InternalError $ "Failed to decrypt: " <> show err)
  unless isVerified $ throwError (InvalidRequest "Invalid QR data")
  let fields = T.strip <$> T.splitOn "," qrData

  -- helper
  let atMay' i = if length fields > i then Just (fields !! i) else Nothing
  let req i err = fromMaybeM (InvalidRequest err) (atMay' i)
  let opt i = fromMaybe "" (atMay' i)

  ticketNumber <- req 0 "Missing ticketNumber"
  fromRouteProviderCode <- req 1 "Missing fromRouteProviderCode"
  toRouteProviderCode <- req 2 "Missing toRouteProviderCode"
  adultQuantity' <- req 3 "Missing adultQuantity"
  childQuantity' <- req 4 "Missing childQuantity"
  vehicleTypeProviderCode <- req 5 "Missing vehicleTypeProviderCode"
  ticketAmount' <- req 6 "Missing ticketAmount"
  otpCode' <- req 7 "Missing otpCode"
  _colorCode <- req 8 "Missing colorCode"
  expiry <- req 9 "Missing expiry"
  refreshAt' <- req 10 "Missing refreshAt"
  let fleetNo' = opt 11
  let seatLabels' = opt 12
  now <- getCurrentTime
  let expiryIST = fromMaybe (addUTCTime (secondsToNominalDiffTime 330) now) $ parseUtcTime expiry
  adultQuantity :: Int <- (readMaybe . T.unpack $ adultQuantity') & fromMaybeM (InvalidRequest "Unable to decode adult quantity")
  childQuantity :: Int <- (readMaybe . T.unpack $ childQuantity') & fromMaybeM (InvalidRequest "Unable to decode child quantity")
  ticketAmount :: Money <- (readMaybe . T.unpack $ ticketAmount') & fromMaybeM (InvalidRequest "Unable to decode ticket amount")
  let refreshAt :: Maybe UTCTime = readMaybe . T.unpack $ refreshAt'
      otpCode :: Maybe Int = readMaybe . T.unpack $ otpCode'
  let fleetNo = if T.null fleetNo' then Nothing else Just fleetNo'
  let seatLabels = if T.null seatLabels' then Nothing else Just (T.splitOn "|" seatLabels')
  return $
    TicketPayload
      { ..
      }
  where
    parseUtcTime :: Text -> Maybe UTCTime
    parseUtcTime t = parseTimeM True defaultTimeLocale "%d-%m-%Y %H:%M:%S" (T.unpack t)

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
