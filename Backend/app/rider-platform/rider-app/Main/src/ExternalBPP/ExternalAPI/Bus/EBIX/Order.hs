module ExternalBPP.ExternalAPI.Bus.EBIX.Order where

import API.Types.UI.FRFSTicketService
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Format
import qualified Data.UUID as UU
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig as DIBC
import EulerHS.Types as ET hiding (Log)
import ExternalBPP.ExternalAPI.Bus.EBIX.Auth
import ExternalBPP.ExternalAPI.Types
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant hiding (route, throwError)
import SharedLogic.FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
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
  { _data :: SaveMobileTicket
  }
  deriving (Generic)

instance FromJSON SaveMobTicketRes where
  parseJSON = genericParseJSON saveMobileTicketJsonOptions

instance ToJSON SaveMobTicketRes where
  toJSON = genericToJSON saveMobileTicketJsonOptions

data SaveMobileTicket = SaveMobileTicket
  { mbTktId :: Text
  }
  deriving (Generic)

instance FromJSON SaveMobileTicket where
  parseJSON = genericParseJSON saveMobileTicketJsonOptions

instance ToJSON SaveMobileTicket where
  toJSON = genericToJSON saveMobileTicketJsonOptions

saveMobileTicketJsonOptions :: Options
saveMobileTicketJsonOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_data" -> "DATA"
        "mbTktId" -> "MB_TKT_ID"
        a -> a
    }

type SaveMobTicketAPI =
  "Api"
    :> "V2"
    :> "Cons"
    :> "SaveMobTicket"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] SaveMobTicketReq
    :> Get '[JSON] SaveMobTicketRes

saveMobTicketAPI :: Proxy SaveMobTicketAPI
saveMobTicketAPI = Proxy

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, MonadReader r m, HasKafkaProducer r) => EBIXConfig -> IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> [FRFSQuoteCategory] -> m ProviderOrder
createOrder config integratedBPPConfig qrTtl booking quoteCategories = do
  orderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> getBppOrderId booking
  let mbRouteStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
  routeStations <- mbRouteStations & fromMaybeM (InternalError "Route Stations Not Found.")
  tickets <- mapM (getTicketDetail config integratedBPPConfig qrTtl booking quoteCategories) routeStations
  return ProviderOrder {..}

getBppOrderId :: (MonadFlow m) => FRFSTicketBooking -> m Text
getBppOrderId booking = do
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let orderId = show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID)) :: Integer) -- This should be max 20 characters UUID (Using Transaction UUID)
  return orderId

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
getTicketDetail :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c, HasKafkaProducer r) => EBIXConfig -> IntegratedBPPConfig -> Seconds -> FRFSTicketBooking -> [FRFSQuoteCategory] -> FRFSRouteStationsAPI -> m ProviderTicket
getTicketDetail config integratedBPPConfig qrTtl booking quoteCategories routeStation = do
  busTypeId <- routeStation.vehicleServiceTier <&> (.providerCode) & fromMaybeM (InternalError "Bus Provider Code Not Found.")
  when (null routeStation.stations) $ throwError (InternalError "Empty Stations")
  let startStation = head routeStation.stations
      endStation = last routeStation.stations
  fromStation <- B.runInReplica $ OTPRest.getStationByGtfsIdAndStopCode startStation.code integratedBPPConfig >>= fromMaybeM (StationNotFound $ startStation.code <> " for integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  toStation <- B.runInReplica $ OTPRest.getStationByGtfsIdAndStopCode endStation.code integratedBPPConfig >>= fromMaybeM (StationNotFound $ endStation.code <> " for integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  route <- OTPRest.getRouteByRouteId integratedBPPConfig routeStation.code >>= fromMaybeM (RouteNotFound routeStation.code)
  fromRoute <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode fromStation.code route.code integratedBPPConfig <&> listToMaybe >>= fromMaybeM (RouteMappingDoesNotExist route.code fromStation.code integratedBPPConfig.id.getId)
  toRoute <- OTPRest.getRouteStopMappingByStopCodeAndRouteCode toStation.code route.code integratedBPPConfig <&> listToMaybe >>= fromMaybeM (RouteMappingDoesNotExist route.code toStation.code integratedBPPConfig.id.getId)
  now <- addUTCTime (secondsToNominalDiffTime 19800) <$> getCurrentTime
  qrValidity <- addUTCTime (secondsToNominalDiffTime qrTtl) <$> getCurrentTime
  ticketNumber <- do
    id <- generateGUID
    uuid <- UU.fromText id & fromMaybeM (InternalError "Not being able to parse into UUID")
    return $ show (fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords uuid)) :: Integer)
  let qrValidityIST = addUTCTime (secondsToNominalDiffTime 19800) qrValidity
      fareParameters = calculateFareParametersWithBookingFallback (mkCategoryPriceItemFromQuoteCategories quoteCategories) booking
      singleAdultTicketPrice = (getUnitPriceFromPriceItem fareParameters.adultItem).amount
      adultQuantity = maybe 0 (.quantity) fareParameters.adultItem
      childQuantity = maybe 0 (.quantity) fareParameters.childItem
  let ticketReq =
        SaveMobTicketReq
          { frRouteStopSrlNo = fromRoute.providerCode,
            toRouteStopSrlNo = toRoute.providerCode,
            noAdult = show adultQuantity,
            noChild = show childQuantity,
            btypeId = busTypeId,
            tktAmt = singleAdultTicketPrice,
            transNo = ticketNumber,
            transDate = T.pack $ formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" now,
            agentId = config.agentId
          }
  token <- getAuthToken config
  ticketOder <-
    callAPI config.networkHostUrl (ET.client saveMobTicketAPI (Just token) ticketReq) "saveMobTicket" saveMobTicketAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_SAVE_MOB_TICKET_API") config.networkHostUrl)
  let ticket = fromRoute.providerCode <> "," <> toRoute.providerCode <> "," <> show adultQuantity <> "," <> show childQuantity <> "," <> busTypeId <> "," <> (T.pack $ formatTime Time.defaultTimeLocale "%d-%m-%Y %H:%M:%S" qrValidityIST) <> "," <> ticketNumber <> "," <> show singleAdultTicketPrice <> "," <> config.agentId <> ",,,,," <> ticketOder._data.mbTktId <> ",,,"
  qrData <- generateQR config ticket
  return $
    ProviderTicket
      { ticketNumber = ticketNumber,
        vehicleNumber = Nothing,
        qrData,
        qrStatus = "UNCLAIMED",
        qrValidity,
        description = Nothing,
        qrRefreshAt = Nothing,
        commencingHours = Nothing
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
