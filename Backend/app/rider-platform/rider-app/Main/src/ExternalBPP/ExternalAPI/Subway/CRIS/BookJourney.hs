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
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import EulerHS.Prelude hiding (readMaybe)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData, encryptPayload)
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISError (..), CRISErrorUnhandled (..))
import ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare (mkRouteFareCacheKey)
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Prelude ((!!))
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.API
import SharedLogic.FRFSUtils
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
    encrypted :: Maybe Text,
    agentTicketData :: Maybe Text
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

  case (respCode encResponse, encResponse.agentTicketData, encResponse.encrypted) of
    -- Case 1: Non-zero response code (API error)
    (code, _, _)
      | code /= 0 ->
        throwError $ CRISError $ "API returned error code " <> show code <> ": " <> encResponse.respMessage
    -- Case 2: Success code but no agent ticket data
    (0, Nothing, _) ->
      throwError $ CRISError $ "No ticket data received from API: " <> encResponse.respMessage
    -- Case 3: Success code but no encrypted field
    (0, Just _, Nothing) ->
      throwError $ CRISError $ "No encrypted data received from API: " <> encResponse.respMessage
    -- Case 4: Success case - process the ticket data
    (0, Just agentTicketData, Just encrypted) -> do
      let (encryptedData, _) = T.breakOn "#" agentTicketData
      case decryptResponseData encryptedData decryptedAgentDataKey of
        Left err -> do
          logError $ "Failed to decrypt ticket data: " <> T.pack err
          throwError $ CRISError $ "Failed to decrypt ticket data"
        Right decryptedJson -> do
          logInfo $ "Decrypted ticket data: " <> decryptedJson
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
            Left err -> do
              logError $ "Failed to parse ticket data: " <> T.pack err
              throwError $ CRISError $ "Failed to parse ticket data"
            Right ticketData -> pure $ convertToBookingResponse ticketData encrypted
    -- Catch-all case (should never be reached)
    (code, _, _) ->
      throwError $ CRISErrorUnhandled $ "Unhandled response pattern: code=" <> show code <> ", message=" <> encResponse.respMessage
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

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasShortDurationRetryCfg r c) => CRISConfig -> IntegratedBPPConfig -> DFRFSTicketBooking.FRFSTicketBooking -> [FRFSQuoteCategory] -> m ProviderOrder
createOrder config integratedBPPConfig booking quoteCategories = do
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mbMobileNumber <- decrypt `mapM` person.mobileNumber
  fromStation <- OTPRest.getStationByGtfsIdAndStopCode booking.fromStationCode integratedBPPConfig >>= fromMaybeM (CRISError $ "Station not found for stationCode: " <> booking.fromStationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  toStation <- OTPRest.getStationByGtfsIdAndStopCode booking.toStationCode integratedBPPConfig >>= fromMaybeM (CRISError $ "Station not found for stationCode: " <> booking.toStationCode <> " and integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
  quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)

  (osBuildVersion, osType, bookAuthCode) <- case (booking.osBuildVersion, booking.osType, booking.bookingAuthCode) of
    (Just osBuildVersion, Just osType, Just bookingAuthCode) -> return (osBuildVersion, osType, bookingAuthCode)
    _ -> throwError $ CRISError ("Invalid booking data: " <> show booking.osBuildVersion <> " " <> show booking.osType <> " " <> show booking.bookingAuthCode)

  mbImeiNumber <- decrypt `mapM` person.imeiNumber
  let deviceId = fromMaybe "ed409d8d764c04f7" mbImeiNumber
  (trainTypeCode, distance, crisRouteId, appSession, ticketTypeCode) <-
    case (quote.fareDetails <&> (.trainTypeCode), quote.fareDetails <&> (.distance), quote.fareDetails <&> (.providerRouteId), quote.fareDetails <&> (.appSession), quote.fareDetails <&> (.ticketTypeCode)) of
      (Just trainTypeCode, Just distance, Just crisRouteId, Just appSession, Just ticketTypeCode) -> return (trainTypeCode, distance, crisRouteId, appSession, ticketTypeCode)
      _ -> throwError $ CRISError ("Invalid quote data: " <> show quote.fareDetails)

  orderId <- case booking.bppOrderId of
    Just oid -> return oid
    Nothing -> getBppOrderId booking
  classCode <- getFRFSVehicleServiceTier quote
  currentTime <- getCurrentTime
  let tpBookType = if config.enableBookType == Just True && booking.isSingleMode == Just True then 1 else 0

  let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
      adultQuantity = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity)
      childQuantity = find (\category -> category.categoryType == CHILD) fareParameters.priceItems <&> (.quantity)
      chargeableAmount = booking.totalPrice.amountInt.getMoney
  let bookJourneyReq =
        CRISBookingRequest
          { mob = fromMaybe "9999999999" mbMobileNumber,
            imei = deviceId,
            appCode = config.appCode,
            sessionId = appSession,
            source = fromStation.code,
            destination = toStation.code,
            via = fromMaybe " " $ quote.fareDetails <&> (.via),
            routeId = crisRouteId,
            classCode = classCode,
            trainType = trainTypeCode,
            tktType = ticketTypeCode,
            journeyDate = T.pack $ formatTime defaultTimeLocale "%m-%d-%Y" (utcToIST currentTime),
            adult = fromMaybe 0 adultQuantity,
            child = fromMaybe 0 childQuantity,
            seniorMen = 0,
            seniorWomen = 0,
            fare = chargeableAmount,
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
            chargeableAmount,
            bonusCreditAmount = 0,
            bonusDebitAmount = 0,
            tktTypeId = 1,
            agentAccountId = show config.tpAccountId,
            bookAuthCode = bookAuthCode,
            agentAppTxnId = orderId,
            bankDeductedAmount = chargeableAmount,
            tpBookType = tpBookType
          }
  logInfo $ "GetBookJourney: " <> show bookJourneyReq
  let changeOver = fromMaybe " " $ quote.fareDetails <&> (.via)
  bookJourneyResp <-
    getBookJourney config bookJourneyReq `safeCatch` \(err :: SomeException) -> do
      logError $ "Booking failed, clearing fare cache for route: " <> fromStation.code <> " -> " <> toStation.code
      let fareCacheKeyTrue = mkRouteFareCacheKey fromStation.code toStation.code changeOver True
      let fareCacheKeyFalse = mkRouteFareCacheKey fromStation.code toStation.code changeOver False
      Hedis.del fareCacheKeyTrue
      Hedis.del fareCacheKeyFalse
      logInfo $ "Cleared fare cache keys: " <> fareCacheKeyTrue <> ", " <> fareCacheKeyFalse
      throwM err

  qrValidityTime <- parseTicketValidity bookJourneyResp.showTicketValidity

  return $
    ProviderOrder
      { orderId = orderId,
        tickets =
          [ ProviderTicket
              { ticketNumber = bookJourneyResp.ticketNumber,
                vehicleNumber = Nothing,
                description = bookJourneyResp.journeyComment,
                qrData = bookJourneyResp.encryptedTicketData,
                qrStatus = "UNCLAIMED",
                qrValidity = qrValidityTime,
                qrRefreshAt = Nothing,
                commencingHours = bookJourneyResp.validUntil
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
  serviceTier <- mbServiceTier & fromMaybeM (CRISError "serviceTier not found")
  -- serviceTierType <- mbServiceTier._type & fromMaybeM (InternalError "serviceTierType not found")
  case serviceTier._type of
    Enums.FIRST_CLASS -> pure "FC"
    Enums.SECOND_CLASS -> pure "II"
    Enums.AC_EMU_FIRST_CLASS -> pure "FC"
    _ -> throwError $ CRISError "Invalid vehicle service tier"

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
  bookingUUID <- fromText booking.id.getId & fromMaybeM (CRISError "Booking Id not being able to parse into UUID")
  return $ uuidTo21CharString bookingUUID --- The length should be 21 characters (alphanumeric)

-- Convert UTC time to IST
utcToIST :: UTCTime -> LocalTime
utcToIST utcTime =
  let istTimeZone = TimeZone (5 * 60 + 30) False "IST" -- IST is UTC+5:30
   in utcToLocalTime istTimeZone utcTime

-- Parse IST datetime string to UTCTime
parseTicketValidity :: (MonadFlow m) => Text -> m UTCTime
parseTicketValidity validityStr = do
  let timeFormat = "%d/%m/%Y %H:%M:%S" --Parse format: "04/08/2025 23:59:00" in IST
  case parseTimeM True defaultTimeLocale timeFormat (T.unpack validityStr) of
    Nothing -> throwError $ CRISError $ "Failed to parse ticket validity: " <> validityStr
    Just localTime -> do
      let istTimeZone = TimeZone (5 * 60 + 30) False "IST" -- IST is UTC+5:30
      let zonedTime = ZonedTime localTime istTimeZone
      return $ zonedTimeToUTC zonedTime
