module ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import BecknV2.FRFS.Enums as Enums
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime)
import Data.Time.Format
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import EulerHS.Prelude hiding (find, readMaybe)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData, encryptPayload)
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant.API
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSTicketBokingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Station as QStation

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
    validUntil :: Text, -- showTicketValidity
    journeyDate :: Maybe Text, -- journeyDate
    routeMessage :: Maybe Text,
    chargeableAmount :: Maybe HighPrecMoney,
    encryptedTicketData :: Text
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
    chargeableAmount :: Maybe HighPrecMoney
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
  decryptedKey <- decrypt config.clientKey -- Decrypt the key first
  encryptedPayload <- encryptPayload jsonStr decryptedKey
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
       in client (Just $ "Bearer " <> token) (Just "application/json") encReq

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
      validUntil = ticketData.showTicketValidity,
      journeyDate = ticketData.journeyDate,
      routeMessage = ticketData.routeMessage,
      chargeableAmount = ticketData.chargeableAmount,
      encryptedTicketData = encrypted
    }

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CRISConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m ProviderOrder
createOrder config booking = do
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mbMobileNumber <- decrypt `mapM` person.mobileNumber
  fromStation <- QStation.findById booking.fromStationId >>= fromMaybeM (InternalError "From station not found")
  toStation <- QStation.findById booking.toStationId >>= fromMaybeM (InternalError "To station not found")
  quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)

  (osBuildVersion, osType, bookAuthCode) <- case (booking.osBuildVersion, booking.osType, booking.bookingAuthCode) of
    (Just osBuildVersion, Just osType, Just bookingAuthCode) -> return (osBuildVersion, osType, bookingAuthCode)
    _ -> throwError $ InternalError ("Invalid booking data: " <> show booking.osBuildVersion <> " " <> show booking.osType <> " " <> show booking.bookingAuthCode)

  mbImeiNumber <- decrypt `mapM` person.imeiNumber
  let deviceId = fromMaybe "ed409d8d764c04f7" mbImeiNumber
  (trainTypeCode, via, distance, crisRouteId, appSession) <-
    case (quote.fareDetails <&> (.trainTypeCode), quote.fareDetails <&> (.via), quote.fareDetails <&> (.distance), quote.fareDetails <&> (.providerRouteId), quote.fareDetails <&> (.appSession)) of
      (Just trainTypeCode, Just via, Just distance, Just crisRouteId, Just appSession) -> return (trainTypeCode, via, distance, crisRouteId, appSession)
      _ -> throwError $ InternalError ("Invalid quote data: " <> show quote.fareDetails)

  frfsTicketBookingPayment <- QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InternalError "FRFS ticket booking payment not found")
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
            via,
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
            agentAppTxnId = show frfsTicketBookingPayment.paymentOrderId,
            bankDeductedAmount = round booking.price.amount.getHighPrecMoney,
            tpBookType = 0
          }
  logInfo $ "GetBookJourney: " <> show bookJourneyReq
  bookJourneyResp <- getBookJourney config bookJourneyReq

  -- TODO: Consume this once get confirmation from CRIS
  -- qrValidityTime <- case parseTimeM True defaultTimeLocale "%d/%m/%Y %H:%M:%S" (T.unpack bookJourneyResp.validUntil) :: Maybe UTCTime of
  --   Nothing -> throwError $ InternalError $ "Failed to parse ticket validity time: " <> bookJourneyResp.validUntil
  --   Just time -> pure time

  now <- getCurrentTime
  let threeHoursFromNow = addUTCTime (3 * 60 * 60 :: NominalDiffTime) now

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
                qrValidity = threeHoursFromNow, -- TODO: Consume qrValidityTime from CRIS once confirmed
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
