module ExternalBPP.Bus.ExternalAPI.EBIX.Verification where

import Crypto.Cipher.TripleDES
import Crypto.Cipher.Types
import Crypto.Error (CryptoFailable (..))
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Data.Time.Format
import Domain.Types.FRFSQuote
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.Bus.ExternalAPI.EBIX.Auth
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Base64
import Kernel.Utils.Common
import Servant hiding (route, throwError)
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
-- 9. Agent ID : 5185
-- 10. UDF1
-- 11. UDF2
-- 12. UDF3
-- 13. UDF4
-- 14. MB_TKT_ID = 130
-- 15. UDF5
-- 16. UDF6
-- {tt: [{t: "37001,37017,1,0,5,10-10-2024 19:04:54,2185755416,13,5185,,,,,130,,,"}]}
getVerificationDetails :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => EBIXConfig -> Text -> Integer -> Seconds -> FRFSTicketBooking -> m (Text, UTCTime)
getVerificationDetails config txnUUID ticketNum qrTtl booking = do
  routeId <-
    maybe (throwError (InternalError "Route id not found.")) pure $
      booking.routeId >>= \case
        Bus routeId -> Just routeId
        _ -> Nothing
  busTypeId <- booking.serviceTierProviderCode & fromMaybeM (InternalError "Bus Provider Code Not Found.")
  fromStation <- B.runInReplica $ QStation.findById booking.fromStationId >>= fromMaybeM (StationNotFound booking.fromStationId.getId)
  toStation <- B.runInReplica $ QStation.findById booking.toStationId >>= fromMaybeM (StationNotFound booking.toStationId.getId)
  route <- B.runInReplica $ QRoute.findByRouteId routeId >>= fromMaybeM (RouteNotFound routeId.getId)
  fromRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code fromStation.code >>= fromMaybeM (RouteMappingDoesNotExist route.code fromStation.code)
  toRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code toStation.code >>= fromMaybeM (RouteMappingDoesNotExist route.code toStation.code)
  qrValidity <- addUTCTime (secondsToNominalDiffTime qrTtl) <$> getCurrentTime
  let adultQuantity = booking.quantity
      childQuantity :: Int = 0
      amount = booking.price.amountInt
      qrValidityIST = addUTCTime (secondsToNominalDiffTime 19800) qrValidity
      ticket = "{tt: [{t: \"" <> fromRoute.providerCode <> "," <> toRoute.providerCode <> "," <> show adultQuantity <> "," <> show childQuantity <> "," <> busTypeId <> "," <> formatUtcTime qrValidityIST <> "," <> txnUUID <> "," <> show amount <> "," <> config.agentId <> ",,,,," <> show ticketNum <> ",,,\"}]}"
  return (ticket, qrValidity)
  where
    formatUtcTime :: UTCTime -> Text
    formatUtcTime utcTime = T.pack $ formatTime Time.defaultTimeLocale "%d-%m-%Y %H:%M:%S" utcTime

generateQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r) => EBIXConfig -> Text -> Integer -> Seconds -> FRFSTicketBooking -> m (Text, Text, UTCTime, Text)
generateQR config txnUUID ticketNum qrTtl booking = do
  (qrData, qrValidity) <- getVerificationDetails config txnUUID ticketNum qrTtl booking
  let cipherKey = Base64 "VGhpc0lzVGVzdEVuY3J5cHRpb25LZXkh"
  encryptedQR <- encryptDES cipherKey qrData & fromEitherM (\err -> InternalError $ "Failed to encrypt: " <> show err)
  return (encryptedQR, "UNCLAIMED", qrValidity, show ticketNum)
  where
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

newtype Ticket = Ticket
  { t :: Text
  }
  deriving (Generic)

instance ToJSON Ticket where
  toJSON = genericToJSON createQRJsonOptions

instance FromJSON Ticket where
  parseJSON = genericParseJSON createQRJsonOptions

newtype CreateQRReq = CreateQRReq
  { tt :: [Ticket]
  }
  deriving (Generic)

instance ToJSON CreateQRReq where
  toJSON = genericToJSON createQRJsonOptions

instance FromJSON CreateQRReq where
  parseJSON = genericParseJSON createQRJsonOptions

newtype QRRes = QRRes
  { qrString :: Text
  }
  deriving (Generic)

instance ToJSON QRRes where
  toJSON = genericToJSON createQRJsonOptions

instance FromJSON QRRes where
  parseJSON = genericParseJSON createQRJsonOptions

newtype CreateQRRes = CreateQRRes
  { _data :: QRRes
  }
  deriving (Generic)

instance ToJSON CreateQRRes where
  toJSON = genericToJSON createQRJsonOptions

instance FromJSON CreateQRRes where
  parseJSON = genericParseJSON createQRJsonOptions

createQRJsonOptions :: Options
createQRJsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_data" -> "DATA"
        "qrString" -> "QrString"
        a -> a
    }

type CreateQRAPI =
  "Api"
    :> "V2"
    :> "Cons"
    :> "CreateQr"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] CreateQRReq
    :> Get '[JSON] CreateQRRes

createQRAPI :: Proxy CreateQRAPI
createQRAPI = Proxy

generateQRByProvider :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => EBIXConfig -> Text -> Integer -> Seconds -> FRFSTicketBooking -> m (Text, Text, UTCTime, Text)
generateQRByProvider config txnUUID ticketNum qrTtl booking = do
  (qrData, qrValidity) <- getVerificationDetails config txnUUID ticketNum qrTtl booking
  token <- getAuthToken config
  encryptedQR <-
    callAPI config.networkHostUrl (ET.client createQRAPI (Just token) $ CreateQRReq [Ticket qrData]) "createQR" createQRAPI
      >>= fromEitherM (ExternalAPICallError (Just "CREATE_QR_API") config.networkHostUrl)
  return (encryptedQR._data.qrString, "UNCLAIMED", qrValidity, show ticketNum)
