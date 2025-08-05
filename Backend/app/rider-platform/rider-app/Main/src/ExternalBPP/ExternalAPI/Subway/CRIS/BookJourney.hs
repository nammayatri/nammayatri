module ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import BecknV2.FRFS.Enums as Enums
import Data.Aeson
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format
import Data.Time.LocalTime
import Data.UUID (UUID, fromText, toWords)
import Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Prelude hiding (find, readMaybe)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData, encryptPayload)
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude ((!!))
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.API
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.Person as QPerson

-- Encrypted request/response types for API
data EncryptedRequest = EncryptedRequest
  { app :: Text,
    data_ :: Text
  }
  deriving (Generic, Show)

instance ToJSON EncryptedRequest where
  toJSON EncryptedRequest {..} =
    object ["app" .= app, "data" .= data_]

data EncryptedResponse = EncryptedResponse
  { respCode :: Int,
    respMessage :: Text,
    encrypted :: Text,
    agentTicketData :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- API type using encrypted types
type BookJourneyAPI =
  "t" :> "uts.cris.in" :> "VBCU" :> "1" :> "bookJrny"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[JSON] EncryptedRequest
    :> Post '[JSON] EncryptedResponse

bookJourneyAPI :: Proxy BookJourneyAPI
bookJourneyAPI = Proxy

-- Business logic types remain the same
data CRISBookingRequest = CRISBookingRequest
  { mob :: Text,
    imei :: Text,
    appCode :: Text,
    sessionId :: Int,
    source :: Text,
    destination :: Text,
    via :: Text,
    routeId :: Text,
    classCode :: Text,
    trainType :: Text,
    tktType :: Text,
    journeyDate :: Text,
    adult :: Int,
    child :: Int,
    seniorMen :: Int,
    seniorWomen :: Int,
    fare :: Int,
    paymentCode :: Text,
    osBuildVersion :: Text,
    bookingMode :: Int,
    paymentStatus :: Int,
    gstPassengerName :: Text,
    gstIn :: Text,
    registrationId :: Text,
    sourceStationName :: Text,
    destinationStationName :: Text,
    zone :: Text,
    returnRouteId :: Int,
    osType :: Text,
    distance :: Int,
    chargeableAmount :: Int,
    bonusCreditAmount :: Int,
    bonusDebitAmount :: Int,
    tktTypeId :: Int,
    agentAccountId :: Text,
    bookAuthCode :: Text,
    agentAppTxnId :: Text,
    bankDeductedAmount :: Int,
    tpBookType :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CRISBookingResponse = CRISBookingResponse
  { ticketNumber :: Text, -- utsno
    amount :: Maybe HighPrecMoney, -- cashReceived
    source :: Maybe Text,
    destination :: Maybe Text,
    via :: Maybe Text,
    numAdults :: Maybe Int, -- adult
    numChildren :: Maybe Int, -- child
    classCode :: Maybe Text,
    ticketType :: Maybe Text, -- tktType
    trainType :: Maybe Text,
    serviceTax :: Maybe Text,
    transactionTime :: Maybe Text, -- txnTime
    journeyComment :: Maybe Text, -- jrnyCommencingString
    validUntil :: Maybe Int, -- jrnyCommencingHour
    journeyDate :: Maybe Text, -- journeyDate
    routeMessage :: Maybe Text,
    chargeableAmount :: Maybe HighPrecMoney,
    encryptedTicketData :: Text,
    showTicketValidity :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Add type for decrypted ticket data
data CRISTicketData = CRISTicketData
  { utsNumber :: Text,
    cashReceived :: Maybe HighPrecMoney,
    source :: Maybe Text,
    destination :: Maybe Text,
    via :: Maybe Text,
    adult :: Maybe Int,
    child :: Maybe Int,
    classCode :: Maybe Text,
    ticketType :: Maybe Text,
    trainType :: Maybe Text,
    serviceTax :: Maybe Text,
    txnTime :: Maybe Text,
    jrnyCommencingString :: Maybe Text,
    showTicketValidity :: Text,
    journeyDate :: Maybe Text,
    routeMessage :: Maybe Text,
    chargeableAmount :: Maybe HighPrecMoney,
    jrnyCommencingHour :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Main function that handles business logic
getBookJourney ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  CRISBookingRequest ->
  m CRISBookingResponse
getBookJourney config request = do
  logInfo $ "Booking request object: " <> show request

  -- 1. Construct and encrypt the request
  let jsonStr = constructBookingJson request
  encryptionKey <- decrypt config.encryptionKey -- Decrypt the key first
  encryptedPayload <- encryptPayload jsonStr encryptionKey
  let mobilePrefix = T.take 5 request.mob
  let mobileSuffix = T.takeEnd 5 request.mob
  agentKey <- decrypt config.agentDataDecryptionKey
  let decryptedAgentDataKey = mobileSuffix <> agentKey <> mobilePrefix

  -- 2. Make API call with encrypted request
  let encReq =
        EncryptedRequest
          { app = config.appCode,
            data_ = encryptedPayload
          }

  encResponse <- callCRISAPI config bookJourneyAPI (eulerClientFn encReq) "bookJourney"

  let (encryptedData, _) = T.breakOn "#" encResponse.agentTicketData

  -- 3. Handle the encrypted response
  if respCode encResponse == 0
    then do
      case decryptResponseData encryptedData decryptedAgentDataKey of
        Left err -> throwError $ InternalError $ "Failed to decrypt ticket data: " <> T.pack err
        Right decryptedJson -> do
          logInfo $ "Decrypted ticket data: " <> decryptedJson
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
            Left err -> throwError $ InternalError $ "Failed to parse ticket data: " <> T.pack err
            Right ticketData -> pure $ convertToBookingResponse ticketData encResponse.encrypted
    else throwError $ InternalError $ "Booking failed with code: " <> (show $ respCode encResponse)
  where
    eulerClientFn encReq token =
      let client = ET.client bookJourneyAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") encReq

-- Helper function to convert CRISTicketData to CRISBookingResponse
convertToBookingResponse :: CRISTicketData -> Text -> CRISBookingResponse
convertToBookingResponse ticketData encrypted =
  CRISBookingResponse
    { ticketNumber = ticketData.utsNumber,
      amount = ticketData.cashReceived,
      source = ticketData.source,
      destination = ticketData.destination,
      via = ticketData.via, -- Use record dot notation to be explicit
      numAdults = ticketData.adult,
      numChildren = ticketData.child,
      classCode = ticketData.classCode,
      ticketType = ticketData.ticketType,
      trainType = ticketData.trainType,
      serviceTax = ticketData.serviceTax,
      transactionTime = ticketData.txnTime,
      journeyComment = ticketData.jrnyCommencingString,
      validUntil = ticketData.jrnyCommencingHour,
      journeyDate = ticketData.journeyDate,
      routeMessage = ticketData.routeMessage,
      chargeableAmount = ticketData.chargeableAmount,
      encryptedTicketData = encrypted,
      showTicketValidity = ticketData.showTicketValidity
    }

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c) => CRISConfig -> IntegratedBPPConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m ProviderOrder
createOrder config integratedBPPConfig booking = do
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mbMobileNumber <- decrypt `mapM` person.mobileNumber
  fromStation <- OTPRest.getStationByGtfsIdAndStopCode booking.fromStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.fromStationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  toStation <- OTPRest.getStationByGtfsIdAndStopCode booking.toStationCode integratedBPPConfig >>= fromMaybeM (InternalError $ "Station not found for stationCode: " <> booking.toStationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)

  (osBuildVersion, osType, bookAuthCode) <- case (booking.osBuildVersion, booking.osType, booking.bookingAuthCode) of
    (Just osBuildVersion, Just osType, Just bookingAuthCode) -> return (osBuildVersion, osType, bookingAuthCode)
    _ -> throwError $ InternalError ("Invalid booking data: " <> show booking.osBuildVersion <> " " <> show booking.osType <> " " <> show booking.bookingAuthCode)

  mbImeiNumber <- decrypt `mapM` person.imeiNumber
  let deviceId = fromMaybe "ed409d8d764c04f7" mbImeiNumber
  (trainTypeCode, distance, crisRouteId, appSession) <-
    case (quote.fareDetails <&> (.trainTypeCode), quote.fareDetails <&> (.distance), quote.fareDetails <&> (.providerRouteId), quote.fareDetails <&> (.appSession)) of
      (Just trainTypeCode, Just distance, Just crisRouteId, Just appSession) -> return (trainTypeCode, distance, crisRouteId, appSession)
      _ -> throwError $ InternalError ("Invalid quote data: " <> show quote.fareDetails)

  orderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> getBppOrderId booking
  classCode <- getFRFSVehicleServiceTier quote
  startTime <- fromMaybeM (InternalError "Start time not found") booking.startTime

  let bookJourneyReq =
        CRISBookingRequest
          { mob = fromMaybe "9999999999" mbMobileNumber,
            imei = deviceId,
            appCode = config.appCode,
            sessionId = appSession,
            source = fromStation.code,
            destination = toStation.code,
            via = " ",
            routeId = crisRouteId,
            classCode = classCode,
            trainType = trainTypeCode,
            tktType = config.ticketType,
            journeyDate = T.pack $ formatTime defaultTimeLocale "%m-%d-%Y" startTime,
            adult = booking.quantity,
            child = 0,
            seniorMen = 0,
            seniorWomen = 0,
            fare = round booking.price.amount.getHighPrecMoney,
            paymentCode = config.appCode,
            osBuildVersion = osBuildVersion,
            bookingMode = 2,
            paymentStatus = 1,
            gstPassengerName = " ",
            gstIn = " ",
            registrationId = show config.tpAccountId,
            sourceStationName = fromStation.name,
            destinationStationName = toStation.name,
            zone = config.sourceZone,
            returnRouteId = 0,
            osType = osType,
            distance = distance.getMeters,
            chargeableAmount = round booking.price.amount.getHighPrecMoney,
            bonusCreditAmount = 0,
            bonusDebitAmount = 0,
            tktTypeId = 1,
            agentAccountId = show config.tpAccountId,
            bookAuthCode = bookAuthCode,
            agentAppTxnId = orderId,
            bankDeductedAmount = round booking.price.amount.getHighPrecMoney,
            tpBookType = 0
          }
  logInfo $ "GetBookJourney: " <> show bookJourneyReq
  bookJourneyResp <- getBookJourney config bookJourneyReq

  qrValidityTime <- parseTicketValidity bookJourneyResp.showTicketValidity

  return $
    ProviderOrder
      { orderId = bookJourneyResp.ticketNumber,
        tickets =
          [ ProviderTicket
              { ticketNumber = bookJourneyResp.ticketNumber,
                vehicleNumber = Nothing,
                description = bookJourneyResp.journeyComment,
                qrData = bookJourneyResp.encryptedTicketData,
                qrStatus = "UNCLAIMED",
                qrValidity = qrValidityTime,
                qrRefreshAt = Nothing
              }
          ]
      }

-- Helper function to construct JSON string
constructBookingJson :: CRISBookingRequest -> Text
constructBookingJson request = do
  let bookingRequest =
        object
          [ "mob" .= request.mob,
            "imei" .= request.imei,
            "appCode" .= request.appCode,
            "sessionID" .= request.sessionId,
            "source" .= request.source,
            "destination" .= request.destination,
            "via" .= request.via,
            "routeID" .= request.routeId,
            "classCode" .= request.classCode,
            "trainType" .= request.trainType,
            "tktType" .= request.tktType,
            "journeyDate" .= request.journeyDate,
            "adult" .= (request.adult :: Int),
            "child" .= (request.child :: Int),
            "senoirMen" .= (request.seniorMen :: Int),
            "senoirWoman" .= (request.seniorWomen :: Int),
            "fare" .= (request.fare :: Int),
            "paymentCode" .= request.paymentCode,
            "osBuildVersion" .= request.osBuildVersion,
            "bookingMode" .= (request.bookingMode :: Int),
            "paymentStatus" .= (request.paymentStatus :: Int),
            "gstPassengerName" .= request.gstPassengerName,
            "GstIN" .= request.gstIn,
            "registrationID" .= request.registrationId,
            "sourceStationName" .= request.sourceStationName,
            "destinationStationName" .= request.destinationStationName,
            "zone" .= request.zone,
            "returnRouteID" .= (request.returnRouteId :: Int),
            "osType" .= request.osType,
            "distance" .= (request.distance :: Int),
            "chargeableAmount" .= (request.chargeableAmount :: Int),
            "bonusCreditAmount" .= (request.bonusCreditAmount :: Int),
            "bonusDebitAmount" .= (request.bonusDebitAmount :: Int),
            "tktTypeID" .= (request.tktTypeId :: Int),
            "agentAccountID" .= request.agentAccountId,
            "bookAuthCode" .= request.bookAuthCode,
            "agentAppTxnID" .= request.agentAppTxnId,
            "bankDeductedAmount" .= (request.bankDeductedAmount :: Int),
            "tpBookType" .= (request.tpBookType :: Int)
          ]
  decodeUtf8 $ LBS.toStrict $ encode bookingRequest

getFRFSVehicleServiceTier ::
  (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  DFRFSQuote.FRFSQuote ->
  m Text
getFRFSVehicleServiceTier quote = do
  let routeStations :: Maybe [FRFSTicketServiceAPI.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
  let mbServiceTier = listToMaybe $ mapMaybe (.vehicleServiceTier) (fromMaybe [] routeStations)
  serviceTier <- mbServiceTier & fromMaybeM (InternalError "serviceTier not found")
  -- serviceTierType <- mbServiceTier._type & fromMaybeM (InternalError "serviceTierType not found")
  case serviceTier._type of
    Enums.FIRST_CLASS -> pure "FC"
    Enums.SECOND_CLASS -> pure "II"
    _ -> throwError $ InternalError "Invalid vehicle service tier"

alphabet :: String
alphabet = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

uuidToInteger :: UUID -> Integer
uuidToInteger u =
  let (w1, w2, w3, w4) = toWords u
   in (fromIntegral w1 `shiftL` 96)
        .|. (fromIntegral w2 `shiftL` 64)
        .|. (fromIntegral w3 `shiftL` 32)
        .|. fromIntegral w4

intToBase62 :: Integer -> String
intToBase62 0 = [alphabet !! 0]
intToBase62 n = reverse $ go n
  where
    go 0 = []
    go x = let (q, r) = x `divMod` 62 in (alphabet !! fromIntegral r) : go q

normalizeLength :: Int -> String -> String
normalizeLength l s
  | length s < l = replicate (l - length s) (alphabet !! 0) ++ s
  | otherwise = take l s

uuidTo21CharString :: UUID -> Text
uuidTo21CharString = T.pack . normalizeLength 21 . intToBase62 . uuidToInteger

getBppOrderId :: (MonadFlow m) => FRFSTicketBooking -> m Text
getBppOrderId booking = do
  bookingUUID <- fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  return $ uuidTo21CharString bookingUUID --- The length should be 21 characters (alphanumeric)

-- Parse IST datetime string to UTCTime
parseTicketValidity :: (MonadFlow m) => Text -> m UTCTime
parseTicketValidity validityStr = do
  let timeFormat = "%d/%m/%Y %H:%M:%S" --Parse format: "04/08/2025 23:59:00" in IST
  case parseTimeM True defaultTimeLocale timeFormat (T.unpack validityStr) of
    Nothing -> throwError $ InternalError $ "Failed to parse ticket validity: " <> validityStr
    Just localTime -> do
      let istTimeZone = TimeZone (5 * 60 + 30) False "IST" -- IST is UTC+5:30
      let zonedTime = ZonedTime localTime istTimeZone
      return $ zonedTimeToUTC zonedTime
