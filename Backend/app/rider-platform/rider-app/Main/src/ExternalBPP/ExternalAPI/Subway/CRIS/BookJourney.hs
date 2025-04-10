module ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import BecknV2.FRFS.Enums as Enums
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
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
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
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
  { respCode :: Text,
    respMessage :: Text,
    encrypted :: Text,
    agentTicketData :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- API type using encrypted types
type BookJourneyAPI =
  "t" :> "uts.cris.in" :> "B" :> "1" :> "bookJrny"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> ReqBody '[JSON] EncryptedRequest
    :> Post '[JSON] EncryptedResponse

bookJourneyAPI :: Proxy BookJourneyAPI
bookJourneyAPI = Proxy

-- Business logic types remain the same
data CRISBookingRequest = CRISBookingRequest
  { agentAccountId :: Text,
    mob :: Text,
    imei :: Text,
    appCode :: Text,
    sessionID :: Int,
    source :: Text,
    destination :: Text,
    via :: Text,
    routeID :: Int,
    classCode :: Text,
    trainType :: Text,
    tktType :: Text,
    journeyDate :: Text,
    adult :: Int,
    child :: Int,
    senoirMen :: Int,
    senoirWoman :: Int,
    fare :: Int,
    paymentCode :: Text,
    osBuildVersion :: Int,
    bookingMode :: Int,
    paymentStatus :: Int,
    registrationID :: Text,
    sourceStationName :: Text,
    destinationStationName :: Text,
    zone :: Text,
    osType :: Text,
    distance :: Int,
    chargeableAmount :: Int,
    tktTypeID :: Int,
    agentAppTxnID :: Text,
    bookAuthCode :: Text,
    bankDeductedAmount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CRISBookingResponse = CRISBookingResponse
  { ticketNumber :: Text, -- utsno
    amount :: Text, -- cashReceived
    source :: Text,
    destination :: Text,
    via :: Text,
    numAdults :: Text, -- adult
    numChildren :: Text, -- child
    classCode :: Text,
    ticketType :: Text, -- tktType
    trainType :: Text,
    serviceTax :: Text,
    transactionTime :: Text, -- txnTime
    journeyComment :: Text, -- jrnyCommencingString
    validUntil :: Text, -- showTicketValidity
    journeyDate :: Text, -- jrnyDate
    routeMessage :: Text,
    chargeableAmount :: Text,
    encryptedTicketData :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Add type for decrypted ticket data
data CRISTicketData = CRISTicketData
  { utsno :: Text,
    cashReceived :: Text,
    source :: Text,
    destination :: Text,
    via :: Text,
    adult :: Text,
    child :: Text,
    classCode :: Text,
    tktType :: Text,
    trainType :: Text,
    serviceTax :: Text,
    txnTime :: Text,
    jrnyCommencingString :: Text,
    showTicketValidity :: Text,
    jrnyDate :: Text,
    routeMessage :: Text,
    chargeableAmount :: Text
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

  -- 2. Make API call with encrypted request
  let encReq =
        EncryptedRequest
          { app = "CUMTA",
            data_ = encryptedPayload
          }

  encResponse <- callCRISAPI config bookJourneyAPI (eulerClientFn encReq) "bookJourney"

  -- 3. Handle the encrypted response
  if respCode encResponse == "0"
    then do
      case decryptResponseData encResponse.agentTicketData decryptedKey of
        Left err -> throwError $ InternalError $ "Failed to decrypt ticket data: " <> T.pack err
        Right decryptedJson -> do
          logInfo $ "Decrypted ticket data: " <> decryptedJson
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
            Left err -> throwError $ InternalError $ "Failed to parse ticket data: " <> T.pack err
            Right ticketData -> pure $ convertToBookingResponse ticketData encResponse.encrypted
    else throwError $ InternalError $ "Booking failed with code: " <> respCode encResponse
  where
    eulerClientFn encReq token =
      let client = ET.client bookJourneyAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") encReq

-- Helper function to convert CRISTicketData to CRISBookingResponse
convertToBookingResponse :: CRISTicketData -> Text -> CRISBookingResponse
convertToBookingResponse ticketData encrypted =
  CRISBookingResponse
    { ticketNumber = ticketData.utsno,
      amount = ticketData.cashReceived,
      source = ticketData.source,
      destination = ticketData.destination,
      via = ticketData.via, -- Use record dot notation to be explicit
      numAdults = ticketData.adult,
      numChildren = ticketData.child,
      classCode = ticketData.classCode,
      ticketType = ticketData.tktType,
      trainType = ticketData.trainType,
      serviceTax = ticketData.serviceTax,
      transactionTime = ticketData.txnTime,
      journeyComment = ticketData.jrnyCommencingString,
      validUntil = ticketData.showTicketValidity,
      journeyDate = ticketData.jrnyDate,
      routeMessage = ticketData.routeMessage,
      chargeableAmount = ticketData.chargeableAmount,
      encryptedTicketData = encrypted
    }

createOrder :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => CRISConfig -> DFRFSTicketBooking.FRFSTicketBooking -> m ProviderOrder
createOrder config booking = do
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mbMobileNumber <- decrypt `mapM` person.mobileNumber
  let sessionId = 10 -- TODO: correct this
  fromStation <- QStation.findById booking.fromStationId >>= fromMaybeM (InternalError "From station not found")
  toStation <- QStation.findById booking.toStationId >>= fromMaybeM (InternalError "To station not found")
  frfsSearch <- QFRFSSearch.findById booking.searchId >>= fromMaybeM (SearchRequestNotFound booking.searchId.getId)

  -- Handle crisSearchData properly based on its actual type
  crisSearchData <- fromMaybeM (InternalError "CRIS search data not found") frfsSearch.crisSearchData
  osBuildVersion <- fromMaybeM (InternalError "OS build version not found") crisSearchData.osBuildVersion
  osType <- fromMaybeM (InternalError "OS type not found") crisSearchData.osType
  deviceId <- fromMaybeM (InternalError "Device ID not found") crisSearchData.deviceId
  bookAuthCode <- fromMaybeM (InternalError "Booking auth code not found") crisSearchData.bookAuthCode

  frfsTicketBookingPayment <- QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InternalError "FRFS ticket booking payment not found")
  classCode <- getFRFSVehicleServiceTier booking

  startTime <- fromMaybeM (InternalError "Start time not found") booking.startTime

  let bookJourneyReq =
        CRISBookingRequest
          { agentAccountId = "3700001",
            mob = fromMaybe "9999999999" mbMobileNumber,
            imei = deviceId,
            appCode = "CUMTA",
            sessionID = sessionId,
            source = fromStation.code,
            destination = toStation.code,
            via = " ",
            routeID = 2012009481,
            classCode = classCode,
            trainType = "O",
            tktType = "J",
            journeyDate = T.pack $ formatTime defaultTimeLocale "%d_%m_%y" startTime,
            adult = booking.quantity,
            child = 0,
            senoirMen = 0,
            senoirWoman = 0,
            fare = round booking.price.amount.getHighPrecMoney,
            paymentCode = "CUMTA",
            osBuildVersion = osBuildVersion, -- Convert Text to Int
            bookingMode = 2,
            paymentStatus = 1,
            registrationID = "3700001",
            sourceStationName = fromStation.name,
            destinationStationName = toStation.name,
            zone = "SR",
            osType = osType,
            distance = 10,
            chargeableAmount = round booking.price.amount.getHighPrecMoney,
            tktTypeID = 1,
            agentAppTxnID = show frfsTicketBookingPayment.paymentOrderId,
            bookAuthCode = bookAuthCode,
            bankDeductedAmount = round booking.price.amount.getHighPrecMoney
          }

  bookJourneyResp <- getBookJourney config bookJourneyReq

  qrValidityTime <- case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack bookJourneyResp.validUntil) of
    Nothing -> throwError $ InternalError $ "Failed to parse ticket validity time: " <> bookJourneyResp.validUntil
    Just time -> pure time

  return $
    ProviderOrder
      { orderId = bookJourneyResp.ticketNumber,
        tickets =
          [ ProviderTicket
              { ticketNumber = bookJourneyResp.ticketNumber,
                description = Just bookJourneyResp.journeyComment,
                qrData = bookJourneyResp.encryptedTicketData,
                qrStatus = "",
                qrValidity = qrValidityTime,
                qrRefreshAt = Nothing
              }
          ]
      }

-- Helper function to construct JSON string
constructBookingJson :: CRISBookingRequest -> Text
constructBookingJson req =
  "{"
    <> "\"agentAccountId\":\""
    <> req.agentAccountId
    <> "\","
    <> "\"mob\":\""
    <> req.mob
    <> "\","
    <> "\"imei\":\""
    <> req.imei
    <> "\","
    <> "\"appCode\":\""
    <> req.appCode
    <> "\","
    <> "\"sessionID\":"
    <> show req.sessionID
    <> ","
    <> "\"source\":\""
    <> req.source
    <> "\","
    <> "\"destination\":\""
    <> req.destination
    <> "\","
    <> "\"via\":\""
    <> req.via
    <> "\","
    <> "\"routeID\":"
    <> show req.routeID
    <> ","
    <> "\"classCode\":\""
    <> req.classCode
    <> "\","
    <> "\"trainType\":\""
    <> req.trainType
    <> "\","
    <> "\"tktType\":\""
    <> req.tktType
    <> "\","
    <> "\"journeyDate\":\""
    <> req.journeyDate
    <> "\","
    <> "\"adult\":"
    <> show req.adult
    <> ","
    <> "\"child\":"
    <> show req.child
    <> ","
    <> "\"senoirMen\":"
    <> show req.senoirMen
    <> ","
    <> "\"senoirWoman\":"
    <> show req.senoirWoman
    <> ","
    <> "\"fare\":"
    <> show req.fare
    <> ","
    <> "\"paymentCode\":\""
    <> req.paymentCode
    <> "\","
    <> "\"osBuildVersion\":"
    <> show req.osBuildVersion
    <> ","
    <> "\"bookingMode\":"
    <> show req.bookingMode
    <> ","
    <> "\"paymentStatus\":"
    <> show req.paymentStatus
    <> ","
    <> "\"registrationID\":\""
    <> req.registrationID
    <> "\","
    <> "\"sourceStationName\":\""
    <> req.sourceStationName
    <> "\","
    <> "\"destinationStationName\":\""
    <> req.destinationStationName
    <> "\","
    <> "\"zone\":\""
    <> req.zone
    <> "\","
    <> "\"osType\":\""
    <> req.osType
    <> "\","
    <> "\"distance\":"
    <> show req.distance
    <> ","
    <> "\"chargeableAmount\":"
    <> show req.chargeableAmount
    <> ","
    <> "\"tktTypeID\":"
    <> show req.tktTypeID
    <> ","
    <> "\"agentAppTxnID\":\""
    <> req.agentAppTxnID
    <> "\","
    <> "\"bankDeductedAmount\":"
    <> show req.bankDeductedAmount
    <> ","
    <> "\"bookAuthCode\":\""
    <> req.bookAuthCode
    <> "\""
    <> "}"

getFRFSVehicleServiceTier ::
  (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m Text
getFRFSVehicleServiceTier booking = do
  quote <- QFRFSQuote.findById booking.quoteId >>= fromMaybeM (InternalError "Quote not found")
  let routeStations :: Maybe [FRFSTicketServiceAPI.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
  let mbServiceTier = listToMaybe $ mapMaybe (.vehicleServiceTier) (fromMaybe [] routeStations)
  serviceTier <- mbServiceTier & fromMaybeM (InternalError "serviceTier not found")
  -- serviceTierType <- mbServiceTier._type & fromMaybeM (InternalError "serviceTierType not found")
  case serviceTier._type of
    Enums.FIRST_CLASS -> pure "FC"
    Enums.SECOND_CLASS -> pure "II"
    _ -> throwError $ InternalError "Invalid vehicle service tier"
