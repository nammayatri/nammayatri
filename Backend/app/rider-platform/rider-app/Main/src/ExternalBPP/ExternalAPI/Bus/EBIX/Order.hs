module ExternalBPP.ExternalAPI.Bus.EBIX.Order where

import API.Types.UI.FRFSTicketService
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Format
import qualified Data.UUID as UU
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.EBIX.Auth
import ExternalBPP.ExternalAPI.Types
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (route, throwError)
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.Station as QStation
import Tools.Error
import Tools.JSON

data SaveMobTicketReq = SaveMobTicketReq
  { frRouteStopSrlNo :: Text,
    toRouteStopSrlNo :: Text,
    noAdult :: Text,
    noChild :: Text,
    btypeId :: Text,
    tktAmt :: HighPrecMoney,
    transNo :: Text,
    transDate :: Text,
    agentId :: Text
  }
  deriving (Generic)

instance FromJSON SaveMobTicketReq where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

instance ToJSON SaveMobTicketReq where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

data SaveMobTicketRes = SaveMobTicketRes
  { mbTktId :: Integer
  }
  deriving (Generic)

instance FromJSON SaveMobTicketRes where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

instance ToJSON SaveMobTicketRes where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToScreamingSnakeCase}

type SaveMobTicketAPI =
  "Api"
    :> "Cons"
    :> "SaveMobTicket"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] SaveMobTicketReq
    :> Get '[JSON] SaveMobTicketRes

saveMobTicketAPI :: Proxy SaveMobTicketAPI
saveMobTicketAPI = Proxy

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => EBIXConfig -> Id IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> m ProviderOrder
createOrder config integrationBPPConfigId qrTtl booking = do
  when (isJust booking.bppOrderId) $ throwError (InternalError $ "Order Already Created for Booking : " <> booking.id.getId)
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) :: Integer) -- This should be max 20 characters UUID (Using Transaction UUID)
      mbRouteStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
  routeStations <- mbRouteStations & fromMaybeM (InternalError "Route Stations Not Found.")
  tickets <- mapM (getTicketDetail config integrationBPPConfigId qrTtl booking) routeStations
  return ProviderOrder {..}

-- CUMTA Encrypted QR code generation
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
getTicketDetail :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => EBIXConfig -> Id IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> FRFSRouteStationsAPI -> m ProviderTicket
getTicketDetail config integrationBPPConfigId qrTtl booking routeStation = do
  busTypeId <- routeStation.vehicleServiceTier <&> (.providerCode) & fromMaybeM (InternalError "Bus Provider Code Not Found.")
  when (null routeStation.stations) $ throwError (InternalError "Empty Stations")
  let startStation = head routeStation.stations
      endStation = last routeStation.stations
  fromStation <- B.runInReplica $ QStation.findByStationCode startStation.code integrationBPPConfigId >>= fromMaybeM (StationNotFound $ startStation.code <> " for integratedBPPConfigId: " <> integrationBPPConfigId.getId)
  toStation <- B.runInReplica $ QStation.findByStationCode endStation.code integrationBPPConfigId >>= fromMaybeM (StationNotFound $ endStation.code <> " for integratedBPPConfigId: " <> integrationBPPConfigId.getId)
  route <- do
    B.runInReplica $
      QRoute.findByRouteCode routeStation.code integrationBPPConfigId
        >>= fromMaybeM (RouteNotFound routeStation.code)
  fromRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code fromStation.code integrationBPPConfigId >>= fromMaybeM (RouteMappingDoesNotExist route.code fromStation.code integrationBPPConfigId.getId)
  toRoute <- B.runInReplica $ QRouteStopMapping.findByRouteCodeAndStopCode route.code toStation.code integrationBPPConfigId >>= fromMaybeM (RouteMappingDoesNotExist route.code toStation.code integrationBPPConfigId.getId)
  now <- addUTCTime (secondsToNominalDiffTime 19800) <$> getCurrentTime
  qrValidity <- addUTCTime (secondsToNominalDiffTime qrTtl) <$> getCurrentTime
  ticketNumber <- do
    id <- generateGUID
    uuid <- UU.fromText id & fromMaybeM (InternalError "Not being able to parse into UUID")
    return $ show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) :: Integer)
  let amount = routeStation.priceWithCurrency.amount
  let qrValidityIST = addUTCTime (secondsToNominalDiffTime 19800) qrValidity
      adultQuantity = booking.quantity
      childQuantity :: Int = 0
  let ticketReq =
        SaveMobTicketReq
          { frRouteStopSrlNo = fromRoute.providerCode,
            toRouteStopSrlNo = toRoute.providerCode,
            noAdult = show booking.quantity,
            noChild = "0",
            btypeId = busTypeId,
            tktAmt = amount,
            transNo = ticketNumber,
            transDate = T.pack $ formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" now,
            agentId = config.agentId
          }
  token <- getAuthToken config
  ticketOder <-
    callAPI config.networkHostUrl (ET.client saveMobTicketAPI (Just token) ticketReq) "saveMobTicket" saveMobTicketAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_SAVE_MOB_TICKET_API") config.networkHostUrl)
  let ticket = fromRoute.providerCode <> "," <> toRoute.providerCode <> "," <> show adultQuantity <> "," <> show childQuantity <> "," <> busTypeId <> "," <> (T.pack $ formatTime Time.defaultTimeLocale "%d-%m-%Y %H:%M:%S" qrValidityIST) <> "," <> ticketNumber <> "," <> show amount <> "," <> config.agentId <> ",,,,," <> show ticketOder.mbTktId <> ",,,"
  qrData <- generateQR config ticket
  return $
    ProviderTicket
      { ticketNumber = ticketNumber,
        qrData,
        qrStatus = "UNCLAIMED",
        qrValidity,
        description = Nothing,
        qrRefreshAt = Nothing
      }

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

generateQR :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => EBIXConfig -> Text -> m Text
generateQR config qrData = do
  token <- getAuthToken config
  encryptedQR <-
    callAPI config.networkHostUrl (ET.client createQRAPI (Just token) $ CreateQRReq [Ticket qrData]) "createQR" createQRAPI
      >>= fromEitherM (ExternalAPICallError (Just "CREATE_QR_API") config.networkHostUrl)
  return encryptedQR._data.qrString
