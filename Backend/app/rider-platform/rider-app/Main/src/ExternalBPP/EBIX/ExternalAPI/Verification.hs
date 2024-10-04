module ExternalBPP.EBIX.ExternalAPI.Verification where

import BecknV2.FRFS.Enums
import Crypto.Cipher.TripleDES
import Crypto.Cipher.Types
import Crypto.Error (CryptoFailable (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Data.Time.Format
import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Base64
import Kernel.Utils.Common
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.Station as QStation
import Tools.Error

-- EBIX Encrypted QR code generation
-- 1. From Route Stop Srl No : 1258001
-- 2. To Route Stop Srl No : 1258016
-- 3. No Adult : 1
-- 4. No child : 1
-- 5. Bus Type Id : 4
-- 6. Exp Date time - Expiry date of QR code  dd-MM-yyyy HH:mm:ss  : 10-12-2021 14:19:50
-- 7. Transaction unique no (max 20 chars) : 123456712  for reconciliation
-- 8. Ticket Amount :  25
-- 9. Pagg ID : 12613 . ( you have to add 12613 identification code every time in qr data)
-- {tt: [{t: "1251001,1251022,1,0,4,12-04-2022 23:19:50,223451258,20,12613"}]}
getVerificationDetails :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => BecknConfig -> Text -> Seconds -> FRFSTicketBooking -> m (Text, Text, UTCTime, Text)
getVerificationDetails bapConfig txnUUID qrTtl booking = do
  routeId <- booking.routeId & fromMaybeM (InternalError "Route id not found.")
  fromStation <- B.runInReplica $ QStation.findById booking.fromStationId >>= fromMaybeM (StationNotFound booking.fromStationId.getId)
  toStation <- B.runInReplica $ QStation.findById booking.toStationId >>= fromMaybeM (StationNotFound booking.toStationId.getId)
  route <- B.runInReplica $ QRoute.findByRouteId routeId >>= fromMaybeM (RouteNotFound routeId.getId)
  fromRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code fromStation.code >>= fromMaybeM (RouteMappingDoesNotExist route.code fromStation.code)
  toRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code toStation.code >>= fromMaybeM (RouteMappingDoesNotExist route.code toStation.code)
  qrValidity <- addUTCTime (secondsToNominalDiffTime qrTtl) <$> getLocalCurrentTime 19800
  let adultQuantity = booking.quantity
      childQuantity :: Int = 0
      amount = booking.price.amountInt
      paggId :: Int = 12613
      busTypeId = getBusTypeId booking.vehicleVariant
      ticket = "{tt: [{t: \"" <> fromRoute.code <> "," <> toRoute.code <> "," <> show adultQuantity <> "," <> show childQuantity <> "," <> show busTypeId <> "," <> formatUtcTime qrValidity <> "," <> txnUUID <> "," <> show amount <> "," <> show paggId <> "\"}]}"
  cipherKey <- bapConfig.verificationCipher & fromMaybeM (InternalError $ "Verification Cipher Not Found for txnUUID : " <> txnUUID)
  encryptedQR <- encryptDES cipherKey ticket & fromEitherM (\err -> InternalError $ "Failed to encrypt: " <> show err)
  return (encryptedQR, "UNCLAIMED", qrValidity, txnUUID)
  where
    getBusTypeId :: VehicleVariant -> Int
    getBusTypeId = \case
      ORDINARY -> 2
      VOLVO_AC -> 3
      EXECUTIVE -> 4
      SPECIAL -> 5
      ASHOK_LEYLAND_AC -> 6
      GSAGAR -> 7
      ELECTRIC_VEHICLE -> 8
      MIDI_AC -> 9
      MIDI_NON_AC -> 10
      _ -> 1

    formatUtcTime :: UTCTime -> Text
    formatUtcTime utcTime = T.pack $ formatTime Time.defaultTimeLocale "%d-%m-%Y %H:%M:%S" utcTime

    pad :: Int -> BS.ByteString -> BS.ByteString
    pad blockSize' bs =
      let padLen = blockSize' - BS.length bs `mod` blockSize'
          padding = BS.replicate padLen (fromIntegral padLen)
       in BS.append bs padding

    encryptDES :: Base64 -> Text -> Either String Text
    encryptDES (Base64 cipherKey) plainText = do
      let cipherEither = cipherInit (Base64.decodeLenient cipherKey) :: CryptoFailable DES_EDE3
      case cipherEither of
        CryptoPassed cipher ->
          let paddedPlainText = pad (blockSize cipher) (TE.encodeUtf8 plainText)
           in Right $ TE.decodeUtf8 $ Base64.encode $ ecbEncrypt cipher paddedPlainText
        CryptoFailed err -> Left $ show err
