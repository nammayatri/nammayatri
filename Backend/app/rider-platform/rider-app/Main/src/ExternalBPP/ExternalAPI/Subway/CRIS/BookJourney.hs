module ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import BecknV2.FRFS.Enums as Enums
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text
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
import Text.Read

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
  let mobilePrefix = T.take 5 request.mob
  let mobileSuffix = T.takeEnd 5 request.mob
  agentKey <- (decrypt config.agentDataDecryptionKey)
  let decryptedAgentDataKey = mobileSuffix <> agentKey <> mobilePrefix

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
      case decryptResponseData encResponse.agentTicketData decryptedAgentDataKey of
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
  fromStation <- QStation.findById booking.fromStationId >>= fromMaybeM (InternalError "From station not found")
  toStation <- QStation.findById booking.toStationId >>= fromMaybeM (InternalError "To station not found")
  frfsSearch <- QFRFSSearch.findById booking.searchId >>= fromMaybeM (SearchRequestNotFound booking.searchId.getId)

  -- Handle crisSearchData properly based on its actual type
  crisSearchData <- fromMaybeM (InternalError "CRIS search data not found") frfsSearch.crisSearchData
  osBuildVersionText <- fromMaybeM (InternalError "OS build version not found") crisSearchData.osBuildVersion
  osBuildVersion <- case readMaybe (T.unpack osBuildVersionText) of
    Nothing -> throwError $ InternalError $ "Invalid OS build version: " <> osBuildVersionText
    Just ver -> pure ver

  osType <- fromMaybeM (InternalError "OS type not found") crisSearchData.osType
  deviceId <- fromMaybeM (InternalError "Device ID not found") crisSearchData.deviceId
  bookAuthCode <- fromMaybeM (InternalError "Booking auth code not found") crisSearchData.bookAuthCode
  trainTypeCode <- fromMaybeM (InternalError "trainTypeCode not found") crisSearchData.trainType
  via <- fromMaybeM (InternalError "via not found") crisSearchData.via
  distance <- fromMaybeM (InternalError "distance not found") crisSearchData.distance
  crisRouteId <- fromMaybeM (InternalError "crisRouteId not found") crisSearchData.crisRouteId
  appSession <- fromMaybeM (InternalError "appSession not found") crisSearchData.crisAppSession

  frfsTicketBookingPayment <- QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InternalError "FRFS ticket booking payment not found")
  classCode <- getFRFSVehicleServiceTier booking

  startTime <- fromMaybeM (InternalError "Start time not found") booking.startTime

  let bookJourneyReq =
        CRISBookingRequest
          { agentAccountId = show config.tpAccountId,
            mob = fromMaybe "9999999999" mbMobileNumber,
            imei = deviceId,
            appCode = config.appCode,
            sessionID = appSession,
            source = fromStation.code,
            destination = toStation.code,
            via,
            routeID = crisRouteId,
            classCode = classCode,
            trainType = trainTypeCode,
            tktType = config.ticketType,
            journeyDate = T.pack $ formatTime defaultTimeLocale "%d_%m_%y" startTime,
            adult = booking.quantity,
            child = 0,
            senoirMen = 0,
            senoirWoman = 0,
            fare = round booking.price.amount.getHighPrecMoney,
            paymentCode = config.appCode,
            osBuildVersion = osBuildVersion,
            bookingMode = 2,
            paymentStatus = 1,
            registrationID = show config.tpAccountId,
            sourceStationName = fromStation.name,
            destinationStationName = toStation.name,
            zone = config.sourceZone,
            osType = osType,
            distance,
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
                vehicleNumber = Nothing,
                description = Just bookJourneyResp.journeyComment,
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
          [ "agentAccountId" .= request.agentAccountId,
            "mob" .= request.mob,
            "imei" .= request.imei,
            "appCode" .= request.appCode,
            "sessionID" .= request.sessionID,
            "source" .= request.source,
            "destination" .= request.destination,
            "via" .= request.via,
            "routeID" .= (request.routeID :: Int), -- Explicitly mark as Int
            "classCode" .= request.classCode,
            "trainType" .= request.trainType,
            "tktType" .= request.tktType,
            "journeyDate" .= request.journeyDate,
            "adult" .= (request.adult :: Int),
            "child" .= (request.child :: Int),
            "senoirMen" .= (request.senoirMen :: Int),
            "senoirWoman" .= (request.senoirWoman :: Int),
            "fare" .= (request.fare :: Int),
            "paymentCode" .= request.paymentCode,
            "osBuildVersion" .= (request.osBuildVersion :: Int),
            "bookingMode" .= (request.bookingMode :: Int),
            "paymentStatus" .= (request.paymentStatus :: Int),
            "registrationID" .= request.registrationID,
            "sourceStationName" .= request.sourceStationName,
            "destinationStationName" .= request.destinationStationName,
            "zone" .= request.zone,
            "osType" .= request.osType,
            "distance" .= (request.distance :: Int),
            "chargeableAmount" .= (request.chargeableAmount :: Int),
            "tktTypeID" .= (request.tktTypeID :: Int),
            "agentAppTxnID" .= request.agentAppTxnID,
            "bookAuthCode" .= request.bookAuthCode,
            "bankDeductedAmount" .= (request.bankDeductedAmount :: Int)
          ]
  decodeUtf8 $ LBS.toStrict $ encode bookingRequest

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
