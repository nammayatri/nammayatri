module Lib.JourneyModule.Types where

import API.Types.RiderPlatform.Management.FRFSTicket
import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Applicative (liftA2, (<|>))
import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingStatus as DBooking
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.EstimateStatus as DEstimate
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as FRFSSR
import qualified Domain.Types.FRFSTicketBooking as DFRFSBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.IntegratedBPPConfig as DTBC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.Location
import qualified Domain.Types.Location as DLocation
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RecentLocation as DRL
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import Domain.Types.RouteDetails
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.Station as DTS
import qualified Domain.Types.Station as DStation
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Environment
import EulerHS.Prelude (safeHead)
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface as EMInterface
import Kernel.External.MultiModal.Interface.Types (MultiModalLegGate)
import qualified Kernel.External.MultiModal.Interface.Types as KEMIT
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto hiding (isNothing, runInReplica)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Types.Price as KTP
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import Lib.JourneyLeg.Types
import qualified Lib.JourneyModule.State.Types as JMState
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import Lib.JourneyModule.Utils
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Booking (getfareBreakups)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.Ride as DARide
import qualified SharedLogic.Search as SLSearch
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Transformers.Booking as QTB
import Tools.Error
import Tools.Maps as Maps
import Tools.Metrics.BAPMetrics.Types
import Tools.Payment (roundToTwoDecimalPlaces)
import qualified Tools.SharedRedisKeys as SharedRedisKeys
import TransactionLogs.Types

type SearchRequestFlow m r c =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    EventStreamFlow m r,
    ClickhouseFlow m r,
    HasBAPMetrics m r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nyGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["ondcGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["collectRouteData" ::: Bool],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasField "hotSpotExpiry" r Seconds,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig]
  )

type ConfirmFlow m r c =
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    EventStreamFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasCoreMetrics r,
    HasShortDurationRetryCfg r c,
    HasBAPMetrics m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    Redis.HedisFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  )

type CancelFlow m r c =
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EventStreamFlow m r,
    HasBAPMetrics m r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    m ~ Kernel.Types.Flow.FlowR AppEnv
  )

type GetFareFlow m r =
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "shortDurationRetryCfg" r RetryCfg
  )

type GetStateFlow m r c =
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c
  )

type SearchJourneyLeg leg m = leg -> m SearchResponse

type GetFareJourneyLeg leg m = leg -> m (Bool, Maybe GetFareResponse)

type ConfirmJourneyLeg leg m = leg -> m ()

type CancelJourneyLeg leg m = leg -> m ()

type IsCancellableJourneyLeg leg m = leg -> m IsCancellableResponse

type UpdateJourneyLeg leg m = leg -> m ()

type GetJourneyLegState leg m = leg -> m JourneyLegState

type GetJourneyLeg leg m = leg -> m (Maybe LegInfo)

class JourneyLeg leg m where
  search :: SearchRequestFlow m r c => SearchJourneyLeg leg m
  confirm :: ConfirmFlow m r c => ConfirmJourneyLeg leg m
  update :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => UpdateJourneyLeg leg m
  cancel :: CancelFlow m r c => CancelJourneyLeg leg m
  isCancellable :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, m ~ Kernel.Types.Flow.FlowR AppEnv) => IsCancellableJourneyLeg leg m
  getState :: GetStateFlow m r c => GetJourneyLegState leg m
  getInfo :: GetStateFlow m r c => GetJourneyLeg leg m
  getFare :: GetFareFlow m r => GetFareJourneyLeg leg m

newtype SearchResponse = SearchResponse
  { id :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegState = Transit [JourneyLegStateData] | Single JourneyLegStateData

data NextStopDetails = NextStopDetails
  { stopCode :: Text,
    sequenceNumber :: Int,
    travelTime :: Maybe Seconds,
    travelDistance :: Maybe Meters,
    stopName :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehiclePosition = VehiclePosition
  { position :: Maybe LatLong, -- Bus's current lat/long
    vehicleId :: Text, -- Bus's ID/number
    upcomingStops :: [NextStopDetails] -- List of upcoming stops for this vehicle
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegStateData = JourneyLegStateData
  { status :: JourneyLegStatus,
    userPosition :: Maybe LatLong,
    vehiclePositions :: [VehiclePosition], -- Uses the modified VehiclePosition
    subLegOrder :: Int,
    legOrder :: Int,
    mode :: DTrip.MultimodalTravelMode
    -- boardedVehicles field removed
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetFareResponse = GetFareResponse {estimatedMinFare :: HighPrecMoney, estimatedMaxFare :: HighPrecMoney, serviceTypes :: Maybe [Spec.ServiceTierType]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInitData = JourneyInitData
  { legs :: [EMInterface.MultiModalLeg],
    parentSearchId :: Id DSR.SearchRequest,
    merchantId :: Id DM.Merchant,
    personId :: Id DP.Person,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    estimatedDistance :: Distance,
    estimatedDuration :: Seconds,
    startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    maximumWalkDistance :: Meters,
    straightLineThreshold :: Meters,
    relevanceScore :: Maybe Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegInfo = LegInfo
  { journeyLegId :: Maybe (Id DJL.JourneyLeg),
    skipBooking :: Bool,
    bookingAllowed :: Bool,
    pricingId :: Maybe Text,
    searchId :: Text,
    travelMode :: DTrip.MultimodalTravelMode,
    startTime :: UTCTime,
    order :: Int,
    status :: JourneyLegStatus, -- TODO :: To be Deprecated, remove this once UI starts consuming `legStatus` instead.
    estimatedDuration :: Maybe Seconds,
    estimatedMinFare :: Maybe PriceAPIEntity,
    estimatedMaxFare :: Maybe PriceAPIEntity,
    estimatedChildFare :: Maybe PriceAPIEntity,
    estimatedTotalFare :: Maybe PriceAPIEntity,
    estimatedDistance :: Maybe Distance,
    legExtraInfo :: LegExtraInfo,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    personId :: Id DP.Person,
    actualDistance :: Maybe Distance,
    totalFare :: Maybe PriceAPIEntity,
    entrance :: Maybe MultiModalLegGate,
    exit :: Maybe MultiModalLegGate,
    validTill :: Maybe UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyFRFSLegStatus = JourneyFRFSLegStatus
  { trackingStatus :: Maybe JMState.TrackingStatus,
    bookingStatus :: JMState.FRFSJourneyLegStatus,
    subLegOrder :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyTaxiLegStatus = JourneyTaxiLegStatus
  { trackingStatus :: Maybe JMState.TrackingStatus,
    bookingStatus :: JMState.TaxiJourneyLegStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyWalkLegStatus = JourneyWalkLegStatus
  { trackingStatus :: Maybe JMState.TrackingStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegExtraInfo = Walk WalkLegExtraInfo | Taxi TaxiLegExtraInfo | Metro MetroLegExtraInfo | Bus BusLegExtraInfo | Subway SubwayLegExtraInfo
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegSplitInfo = LegSplitInfo
  { amount :: HighPrecMoney,
    status :: Payment.RefundStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WalkLegExtraInfo = WalkLegExtraInfo
  { origin :: Location,
    destination :: Location,
    legStatus :: Maybe JourneyWalkLegStatus,
    id :: Id DWalkLeg.WalkLegMultimodal
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TaxiLegExtraInfo = TaxiLegExtraInfo
  { origin :: Location,
    destination :: Location,
    driverName :: Maybe Text,
    vehicleNumber :: Maybe Text,
    otp :: Maybe Text,
    legStatus :: Maybe JourneyTaxiLegStatus,
    serviceTierName :: Maybe Text,
    bookingId :: Maybe (Id DBooking.Booking),
    rideId :: Maybe (Id DRide.Ride),
    vehicleIconUrl :: Maybe BaseUrl,
    batchConfig :: Maybe SharedRedisKeys.BatchConfig,
    tollDifference :: Maybe Kernel.Types.Common.Price,
    chargeableRideDistance :: Maybe Distance,
    waitingCharges :: Maybe Kernel.Types.Common.Price,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    extraTimeFare :: Maybe Kernel.Types.Common.Price,
    extraDistanceFare :: Maybe Kernel.Types.Common.Price,
    fareProductType :: Maybe Text,
    bppRideId :: Maybe (Id DRide.BPPRide),
    driverMobileNumber :: Maybe Text,
    exoPhoneNumber :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLegExtraInfo = MetroLegExtraInfo
  { routeInfo :: [LegRouteInfo],
    legStatus :: [JourneyFRFSLegStatus],
    bookingId :: Maybe (Id DFRFSBooking.FRFSTicketBooking),
    tickets :: Maybe [Text],
    ticketValidity :: Maybe [UTCTime],
    ticketsCreatedAt :: Maybe [UTCTime],
    providerName :: Maybe Text,
    ticketNo :: Maybe [Text],
    adultTicketQuantity :: Maybe Int,
    childTicketQuantity :: Maybe Int,
    refund :: Maybe LegSplitInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubwayLegExtraInfo = SubwayLegExtraInfo
  { routeInfo :: [LegRouteInfo],
    legStatus :: [JourneyFRFSLegStatus],
    bookingId :: Maybe (Id DFRFSBooking.FRFSTicketBooking),
    tickets :: Maybe [Text],
    ticketValidity :: Maybe [UTCTime],
    ticketsCreatedAt :: Maybe [UTCTime],
    ticketValidityHours :: [Int],
    providerName :: Maybe Text,
    sdkToken :: Maybe Text,
    providerRouteId :: Maybe Text,
    deviceId :: Maybe Text,
    ticketTypeCode :: Maybe Text,
    selectedServiceTier :: Maybe LegServiceTier,
    ticketNo :: Maybe [Text],
    adultTicketQuantity :: Maybe Int,
    childTicketQuantity :: Maybe Int,
    refund :: Maybe LegSplitInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegRouteInfo = LegRouteInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    routeCode :: Text,
    subOrder :: Maybe Int,
    platformNumber :: Maybe Text,
    lineColor :: Maybe Text,
    lineColorCode :: Maybe Text,
    trainNumber :: Maybe Text,
    journeyStatus :: Maybe JourneyLegStatus,
    frequency :: Maybe Seconds,
    allAvailableRoutes :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BusLegExtraInfo = BusLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    legStatus :: [JourneyFRFSLegStatus],
    routeCode :: Text,
    bookingId :: Maybe (Id DFRFSBooking.FRFSTicketBooking),
    tickets :: Maybe [Text],
    ticketValidity :: Maybe [UTCTime],
    ticketsCreatedAt :: Maybe [UTCTime],
    routeName :: Maybe Text,
    providerName :: Maybe Text,
    selectedServiceTier :: Maybe LegServiceTier,
    frequency :: Maybe Seconds,
    alternateShortNames :: [Text],
    ticketNo :: Maybe [Text],
    adultTicketQuantity :: Maybe Int,
    childTicketQuantity :: Maybe Int,
    refund :: Maybe LegSplitInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateJourneyReq = UpdateJourneyReq
  { fare :: Maybe Price,
    modes :: Maybe [DTrip.MultimodalTravelMode],
    totalLegs :: Maybe Int,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingData = BookingData
  { bookingId :: Text,
    isRoundTrip :: Bool,
    ticketData :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UnifiedTicketQR = UnifiedTicketQR
  { version :: Text,
    _type :: Text,
    txnId :: Text,
    createdAt :: UTCTime,
    cmrl :: [BookingData],
    mtc :: [BookingData],
    cris :: [BookingData]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON UnifiedTicketQR where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON UnifiedTicketQR where
  parseJSON = withObject "UnifiedTicketQR" $ \o -> do
    version <- o .: "version"
    _type <- o .: "type"
    txnId <- o .: "txnId"
    createdAt <- o .: "createdAt"
    cmrl <- o .: "CMRL"
    mtc <- o .: "MTC"
    cris <- o .: "CRIS"
    return $ UnifiedTicketQR version _type txnId createdAt cmrl mtc cris

data Provider = CMRL | MTC | DIRECT | CRIS
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data BookingDataV2 = BookingDataV2
  { bookingId :: Text,
    isRoundTrip :: Bool,
    ticketData :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON BookingDataV2 where
  parseJSON = withObject "BookingDataV2" $ \o -> do
    bookingId <- o .: "BookingId"
    isRoundTrip <- o .: "isRoundTrip"
    ticketData <- o .: "data"
    return $ BookingDataV2 bookingId isRoundTrip ticketData

instance ToJSON BookingDataV2 where
  toJSON (BookingDataV2 bookingId isRoundTrip ticketData) =
    object
      [ "BookingId" .= bookingId,
        "isRoundTrip" .= isRoundTrip,
        "data" .= ticketData
      ]

data UnifiedTicketQRV2 = UnifiedTicketQRV2
  { version :: Text,
    _type :: Text,
    txnId :: Text,
    createdAt :: UTCTime,
    cmrl :: [BookingDataV2],
    mtc :: [BookingDataV2]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON UnifiedTicketQRV2 where
  parseJSON = withObject "UnifiedTicketQRV2" $ \o -> do
    version <- o .: "version"
    _type <- o .: "type"
    txnId <- o .: "txnId"
    createdAt <- o .: "createdAt"
    cmrl <- o .: "CMRL"
    mtc <- o .: "MTC"
    return $ UnifiedTicketQRV2 version _type txnId createdAt cmrl mtc

instance ToJSON UnifiedTicketQRV2 where
  toJSON (UnifiedTicketQRV2 version _type txnId createdAt cmrl mtc) =
    object
      [ "version" .= version,
        "type" .= _type,
        "txnId" .= txnId,
        "createdAt" .= createdAt,
        "CMRL" .= cmrl,
        "MTC" .= mtc
      ]

data IsCancellableResponse = IsCancellableResponse
  { canCancel :: Bool
  }

mapTaxiRideStatusToJourneyLegStatus :: DRide.RideStatus -> JourneyLegStatus
mapTaxiRideStatusToJourneyLegStatus status = case status of
  DRide.UPCOMING -> InPlan
  DRide.NEW -> Booked
  DRide.INPROGRESS -> Ongoing
  DRide.COMPLETED -> Completed
  DRide.CANCELLED -> Cancelled

mapTaxiBookingStatusToJourneyLegStatus :: DBooking.BookingStatus -> JourneyLegStatus
mapTaxiBookingStatusToJourneyLegStatus status = case status of
  DBooking.NEW -> InPlan
  DBooking.CONFIRMED -> InPlan
  DBooking.AWAITING_REASSIGNMENT -> Assigning
  DBooking.REALLOCATED -> Cancelled
  DBooking.COMPLETED -> Completed
  DBooking.CANCELLED -> Cancelled
  DBooking.TRIP_ASSIGNED -> Booked

getTaxiLegStatusFromBooking :: GetStateFlow m r c => DBooking.Booking -> Maybe DRide.Ride -> Maybe JourneyLegStatus -> m (JourneyLegStatus, Maybe LatLong)
getTaxiLegStatusFromBooking booking mRide journeyLegStatus = do
  case journeyLegStatus of
    Just Completed -> return (Completed, Nothing)
    _ -> do
      if (fromMaybe False booking.isSkipped)
        then return (Skipped, Nothing)
        else case mRide of
          Just ride -> do
            driverLocationResp <- try @_ @SomeException $ DARide.getDriverLoc ride.id
            case driverLocationResp of
              Left err -> do
                logError $ "location fetch failed: " <> show err
                return $ (mapTaxiRideStatusToJourneyLegStatus ride.status, Nothing)
              Right driverLocation -> do
                let journeyStatus =
                      case (ride.status, driverLocation.pickupStage) of
                        (DRide.NEW, Just stage) -> stage
                        _ -> mapTaxiRideStatusToJourneyLegStatus ride.status
                return $ (journeyStatus, Just $ LatLong driverLocation.lat driverLocation.lon)
          Nothing -> return $ (mapTaxiBookingStatusToJourneyLegStatus booking.status, Nothing)

getTaxiLegStatusFromSearch :: JourneySearchData -> Maybe DEstimate.EstimateStatus -> Maybe JourneyLegStatus -> JourneyLegStatus
getTaxiLegStatusFromSearch journeyLegInfo mbEstimateStatus journeyLegStatus =
  case journeyLegStatus of
    Just Completed -> Completed
    _ -> do
      if journeyLegInfo.skipBooking
        then Skipped
        else case mbEstimateStatus of
          Nothing -> InPlan
          Just DEstimate.NEW -> InPlan
          Just DEstimate.COMPLETED -> Booked
          Just DEstimate.CANCELLED -> Cancelled
          _ -> Assigning

getTollDifference :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => [DFareBreakup.FareBreakup] -> [DFareBreakup.FareBreakup] -> m Kernel.Types.Common.Price
getTollDifference fareBreakups estimatedFareBreakups = do
  let extractAmount = fromMaybe Kernel.Types.Common.Price {amountInt = 0, amount = 0.0, currency = KTP.INR} . fmap (getField @"amount")
      estimateAmount = extractAmount <$> safeHead $ filter (\item -> item.description == "TOLL_CHARGES") estimatedFareBreakups
      finalAmount = extractAmount <$> safeHead $ filter (\item -> item.description == "TOLL_CHARGES") fareBreakups
  subtractPrice finalAmount estimateAmount

getDistance :: DBooking.BookingDetails -> HighPrecMeters
getDistance = \case
  DBooking.OneWayDetails details -> distanceToHighPrecMeters $ details.distance
  DBooking.DriverOfferDetails details -> distanceToHighPrecMeters $ details.distance
  DBooking.OneWaySpecialZoneDetails details -> distanceToHighPrecMeters $ details.distance
  DBooking.InterCityDetails details -> distanceToHighPrecMeters $ details.distance
  DBooking.AmbulanceDetails details -> distanceToHighPrecMeters $ details.distance
  DBooking.DeliveryDetails details -> distanceToHighPrecMeters $ details.distance
  DBooking.MeterRideDetails _ -> 0
  DBooking.RentalDetails _ -> 0

mkLegInfoFromBookingAndRide :: GetStateFlow m r c => DBooking.Booking -> Maybe DRide.Ride -> Maybe MultiModalLegGate -> Maybe MultiModalLegGate -> Maybe JourneyLegStatus -> m LegInfo
mkLegInfoFromBookingAndRide booking mRide entrance exit journeyStatus = do
  toLocation <- QTB.getToLocation booking.bookingDetails & fromMaybeM (InvalidRequest "To Location not found")
  (status, _) <- getTaxiLegStatusFromBooking booking mRide journeyStatus
  (fareBreakups, estimatedFareBreakups) <- getfareBreakups booking mRide
  tollDifference <- getTollDifference fareBreakups estimatedFareBreakups
  batchConfig <- SharedRedisKeys.getBatchConfig booking.transactionId
  let mbLegSearchId = Just booking.transactionId
      mbPricingId = booking.quoteId <&> (.getId)
  bookingStatus <- JMStateUtils.getTaxiJourneyBookingStatus mbLegSearchId mbPricingId
  legStatus <-
    mapM
      ( \routeDetails -> do
          trackingStatus <- JMStateUtils.getTaxiJourneyLegTrackingStatus mbLegSearchId mbPricingId routeDetails
          return $ JourneyTaxiLegStatus {trackingStatus = trackingStatus, bookingStatus = bookingStatus}
      )
      booking.journeyRouteDetails
  let skipBooking =
        fromMaybe False booking.isSkipped -- TODO :: For backward compatibility, remove this first condition once 2nd condition is stable in Production
          || bookingStatus `elem` [JMState.TaxiRide DRide.CANCELLED, JMState.TaxiBooking DBooking.CANCELLED, JMState.TaxiEstimate DEstimate.CANCELLED]
  return $
    LegInfo
      { journeyLegId = booking.journeyLegId,
        skipBooking,
        bookingAllowed = True,
        searchId = booking.transactionId,
        pricingId = booking.quoteId <&> (.getId),
        travelMode = DTrip.Taxi,
        startTime = booking.startTime,
        order = fromMaybe 0 booking.journeyLegOrder,
        estimatedDuration = booking.estimatedDuration,
        estimatedMinFare = Just $ mkPriceAPIEntity booking.estimatedFare,
        estimatedChildFare = Nothing,
        estimatedMaxFare = Just $ mkPriceAPIEntity booking.estimatedFare,
        estimatedTotalFare = Just $ mkPriceAPIEntity booking.estimatedTotalFare,
        estimatedDistance = booking.estimatedDistance,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        personId = booking.riderId,
        status,
        legExtraInfo =
          Taxi $
            TaxiLegExtraInfo
              { origin = booking.fromLocation,
                destination = toLocation,
                legStatus = legStatus,
                driverName = mRide <&> (.driverName),
                vehicleNumber = mRide <&> (.vehicleNumber),
                otp = mRide <&> (.otp),
                serviceTierName = booking.serviceTierName,
                bookingId = Just $ booking.id,
                rideId = mRide <&> (.id),
                vehicleIconUrl = booking.vehicleIconUrl,
                batchConfig,
                tollDifference = Just tollDifference,
                chargeableRideDistance = mRide >>= (.chargeableDistance),
                waitingCharges = (.amount) <$> find (\item -> item.description == "WAITING_OR_PICKUP_CHARGES") fareBreakups,
                rideStartTime = (.rideStartTime) =<< mRide,
                rideEndTime = (.rideEndTime) =<< mRide,
                extraTimeFare = (.amount) <$> find (\item -> item.description == "TIME_BASED_FARE") fareBreakups,
                fareProductType = Just $ getBookingDetailsConstructor booking.bookingDetails,
                extraDistanceFare = (.amount) <$> find (\item -> item.description == "DIST_BASED_FARE") fareBreakups,
                bppRideId = mRide <&> (.bppRideId),
                driverMobileNumber = (\item -> Just $ item.driverMobileNumber) =<< mRide,
                exoPhoneNumber = Just booking.primaryExophone
              },
        actualDistance = mRide >>= (.traveledDistance),
        totalFare = mkPriceAPIEntity <$> (mRide >>= (.totalFare)),
        entrance = entrance,
        exit = exit,
        validTill = Nothing
      }
  where
    getBookingDetailsConstructor :: DBooking.BookingDetails -> Text
    getBookingDetailsConstructor (DBooking.OneWayDetails _) = "OneWayDetails"
    getBookingDetailsConstructor (DBooking.RentalDetails _) = "RentalDetails"
    getBookingDetailsConstructor (DBooking.DriverOfferDetails _) = "DriverOfferDetails"
    getBookingDetailsConstructor (DBooking.OneWaySpecialZoneDetails _) = "OneWaySpecialZoneDetails"
    getBookingDetailsConstructor (DBooking.InterCityDetails _) = "InterCityDetails"
    getBookingDetailsConstructor (DBooking.AmbulanceDetails _) = "AmbulanceDetails"
    getBookingDetailsConstructor (DBooking.DeliveryDetails _) = "DeliveryDetails"
    getBookingDetailsConstructor (DBooking.MeterRideDetails _) = "MeterRideDetails"

mkLegInfoFromSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DSR.SearchRequest -> Maybe MultiModalLegGate -> Maybe MultiModalLegGate -> Maybe JourneyLegStatus -> m LegInfo
mkLegInfoFromSearchRequest DSR.SearchRequest {..} entrance exit journeyLegStatus = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  (mbFareRange, mbEstimateStatus, mbEstimate) <-
    case journeyLegInfo'.pricingId of
      Just estId -> do
        mbEst <- QEstimate.findById (Id estId)
        return $ (mbEst <&> (.totalFareRange), mbEst <&> (.status), mbEst)
      Nothing -> return (Nothing, Nothing, Nothing)
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  batchConfig <- SharedRedisKeys.getBatchConfig id.getId
  let mbLegSearchId = Just id.getId
      mbPricingId = mbEstimate <&> (.id.getId)
  bookingStatus <- JMStateUtils.getTaxiJourneyBookingStatus mbLegSearchId mbPricingId
  legStatus <-
    mapM
      ( \routeDetails -> do
          trackingStatus <- JMStateUtils.getTaxiJourneyLegTrackingStatus mbLegSearchId mbPricingId routeDetails
          return $ JourneyTaxiLegStatus {trackingStatus = trackingStatus, bookingStatus = bookingStatus}
      )
      journeyRouteDetails
  let skipBooking =
        journeyLegInfo'.skipBooking -- TODO :: For backward compatibility, remove this first condition once 2nd condition is stable in Production
          || bookingStatus `elem` [JMState.TaxiRide DRide.CANCELLED, JMState.TaxiBooking DBooking.CANCELLED, JMState.TaxiEstimate DEstimate.CANCELLED]
  return $
    LegInfo
      { journeyLegId = journeyLegId,
        skipBooking = skipBooking,
        bookingAllowed = True,
        searchId = id.getId,
        pricingId = journeyLegInfo'.pricingId,
        travelMode = DTrip.Taxi,
        startTime = startTime,
        order = journeyLegInfo'.journeyLegOrder,
        estimatedDuration = estimatedRideDuration,
        estimatedChildFare = Nothing,
        estimatedMinFare = mkPriceAPIEntity <$> (mbFareRange <&> (.minFare)),
        estimatedMaxFare = mkPriceAPIEntity <$> (mbFareRange <&> (.maxFare)),
        estimatedTotalFare = Nothing,
        estimatedDistance = distance,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        personId = riderId,
        status = getTaxiLegStatusFromSearch journeyLegInfo' mbEstimateStatus journeyLegStatus,
        legExtraInfo =
          Taxi $
            TaxiLegExtraInfo
              { origin = fromLocation,
                destination = toLocation',
                legStatus = legStatus,
                driverName = Nothing,
                vehicleNumber = Nothing,
                otp = Nothing,
                serviceTierName = mbEstimate >>= (.serviceTierName),
                bookingId = Nothing,
                rideId = Nothing,
                vehicleIconUrl = mbEstimate >>= (.vehicleIconUrl),
                tollDifference = Nothing,
                batchConfig,
                chargeableRideDistance = Nothing,
                waitingCharges = Nothing,
                rideStartTime = Nothing,
                rideEndTime = Nothing,
                extraTimeFare = Nothing,
                extraDistanceFare = Nothing,
                fareProductType = Nothing,
                bppRideId = Nothing,
                driverMobileNumber = Nothing,
                exoPhoneNumber = Nothing
              },
        actualDistance = Nothing,
        totalFare = Nothing,
        entrance = entrance,
        exit = exit,
        validTill = Nothing
      }

getWalkLegStatusFromWalkLeg :: DWalkLeg.WalkLegMultimodal -> JourneySearchData -> JourneyLegStatus
getWalkLegStatusFromWalkLeg legData journeyLegInfo = do
  if journeyLegInfo.skipBooking
    then Skipped
    else castLegStatusFromWalkLegStatus legData.status

castLegStatusFromWalkLegStatus :: DWalkLeg.WalkLegStatus -> JourneyLegStatus
castLegStatusFromWalkLegStatus DWalkLeg.InPlan = InPlan
castLegStatusFromWalkLegStatus DWalkLeg.Ongoing = Ongoing
castLegStatusFromWalkLegStatus DWalkLeg.Finishing = Finishing
castLegStatusFromWalkLegStatus DWalkLeg.Completed = Completed

castWalkLegStatusFromLegStatus :: JourneyLegStatus -> DWalkLeg.WalkLegStatus
castWalkLegStatusFromLegStatus InPlan = DWalkLeg.InPlan
castWalkLegStatusFromLegStatus Ongoing = DWalkLeg.Ongoing
castWalkLegStatusFromLegStatus Finishing = DWalkLeg.Finishing
castWalkLegStatusFromLegStatus Completed = DWalkLeg.Completed
castWalkLegStatusFromLegStatus _ = DWalkLeg.InPlan

mkWalkLegInfoFromWalkLegData :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DWalkLeg.WalkLegMultimodal -> Maybe MultiModalLegGate -> Maybe MultiModalLegGate -> m LegInfo
mkWalkLegInfoFromWalkLegData legData@DWalkLeg.WalkLegMultimodal {..} entrance exit = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal walk search as no journeyLegInfo found")
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  legStatus <-
    mapM
      ( \routeDetails -> do
          let mbLegSearchId = Just id.getId
          trackingStatus <- JMStateUtils.getWalkJourneyLegStatus mbLegSearchId Nothing routeDetails
          return $ JourneyWalkLegStatus {trackingStatus = trackingStatus}
      )
      journeyRouteDetails
  let skipBooking = journeyLegInfo'.skipBooking
  return $
    LegInfo
      { journeyLegId = journeyLegId,
        skipBooking = skipBooking,
        bookingAllowed = False,
        searchId = id.getId,
        pricingId = Just id.getId,
        travelMode = DTrip.Walk,
        startTime = startTime,
        order = journeyLegInfo'.journeyLegOrder,
        estimatedDuration = estimatedDuration,
        estimatedMinFare = Nothing,
        estimatedMaxFare = Nothing,
        estimatedTotalFare = Nothing,
        estimatedChildFare = Nothing,
        estimatedDistance = estimatedDistance,
        merchantId = merchantId,
        merchantOperatingCityId,
        personId = riderId,
        status = getWalkLegStatusFromWalkLeg legData journeyLegInfo',
        legExtraInfo = Walk $ WalkLegExtraInfo {origin = fromLocation, destination = toLocation', legStatus = legStatus, id = id},
        actualDistance = estimatedDistance,
        totalFare = Nothing,
        entrance = entrance,
        exit = exit,
        validTill = Nothing
      }

getFRFSLegStatusFromBooking :: DFRFSBooking.FRFSTicketBooking -> JourneyLegStatus
getFRFSLegStatusFromBooking booking = case booking.status of
  DFRFSBooking.NEW -> InPlan
  DFRFSBooking.APPROVED -> InPlan
  DFRFSBooking.PAYMENT_PENDING -> InPlan
  DFRFSBooking.CONFIRMING -> Assigning
  DFRFSBooking.CONFIRMED -> Booked
  DFRFSBooking.FAILED -> Failed
  DFRFSBooking.CANCELLED -> Cancelled
  DFRFSBooking.COUNTER_CANCELLED -> Cancelled
  DFRFSBooking.CANCEL_INITIATED -> CancelInitiated
  DFRFSBooking.TECHNICAL_CANCEL_REJECTED -> InPlan

mkLegInfoFromFrfsBooking ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => DFRFSBooking.FRFSTicketBooking -> Maybe Distance -> Maybe Seconds -> m LegInfo
mkLegInfoFromFrfsBooking booking distance duration = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  let journeyRouteDetails' = booking.journeyRouteDetails
  tickets <- QFRFSTicket.findAllByTicketBookingId (booking.id)
  let ticketsData =
        case integratedBPPConfig.providerConfig of
          DIBC.ONDC config -> do
            if fromMaybe False config.singleTicketForMultiplePassengers
              then tickets
              else maybe [] (\ticket -> [ticket]) $ listToMaybe tickets
          _ -> tickets
  let ticketsCreatedAt = ticketsData <&> (.createdAt)
  let qrDataList = ticketsData <&> (.qrData)
  let qrValidity = ticketsData <&> (.validTill)
  let ticketNo = ticketsData <&> (.ticketNumber)
  journeyLegInfo' <- getLegRouteInfo journeyRouteDetails' integratedBPPConfig

  now <- getCurrentTime
  legOrder <- fromMaybeM (InternalError "Leg Order is Nothing") (booking.journeyLegOrder)
  let startTime = fromMaybe now booking.startTime
  -- TODO :: To be Deprecated, remove this once UI starts consuming `legExtraInfo.legStatus` instead.
  let legStatus =
        case booking.journeyLegStatus of
          Nothing -> getFRFSLegStatusFromBooking booking
          Just InPlan -> getFRFSLegStatusFromBooking booking
          Just status -> status
  let singleAdultPrice = roundToTwoDecimalPlaces . HighPrecMoney $ safeDiv (getHighPrecMoney booking.price.amount) (fromIntegral booking.quantity) -- TODO :: To be handled as single price cannot be obtained from this if more than 1 fare breakup (Child Quantity / Discounts)
      estimatedPrice =
        Price
          { amount = singleAdultPrice,
            amountInt = Money $ roundToIntegral singleAdultPrice,
            currency = booking.price.currency
          }
  let mbLegSearchId = Just booking.searchId.getId
      mbPricingId = Just booking.quoteId.getId
  bookingStatus <- JMStateUtils.getFRFSJourneyBookingStatus mbLegSearchId mbPricingId
  journeyLegStatus <-
    mapM
      ( \routeDetails -> do
          trackingStatus <- JMStateUtils.getFRFSJourneyLegTrackingStatus mbLegSearchId mbPricingId routeDetails
          return $ JourneyFRFSLegStatus {trackingStatus = trackingStatus, bookingStatus = bookingStatus, subLegOrder = routeDetails.subLegOrder}
      )
      journeyRouteDetails'
  legExtraInfo <- mkLegExtraInfo qrDataList qrValidity ticketsCreatedAt journeyRouteDetails' journeyLegStatus journeyLegInfo' ticketNo
  let skipBooking =
        fromMaybe False booking.isSkipped -- TODO :: For backward compatibility, remove this first condition once 2nd condition is stable in Production
          || bookingStatus `elem` [JMState.FRFSTicket DFRFSTicket.CANCELLED, JMState.FRFSBooking DFRFSBooking.CANCELLED]
  return $
    LegInfo
      { journeyLegId = booking.journeyLegId,
        skipBooking,
        bookingAllowed = True,
        searchId = booking.searchId.getId,
        pricingId = Just booking.quoteId.getId, -- Just booking.id.getId,
        travelMode = castCategoryToMode booking.vehicleType,
        startTime = startTime,
        order = legOrder,
        estimatedDuration = duration,
        estimatedMinFare = Just $ mkPriceAPIEntity estimatedPrice,
        estimatedChildFare = Nothing,
        estimatedMaxFare = Just $ mkPriceAPIEntity estimatedPrice,
        estimatedTotalFare = Nothing,
        estimatedDistance = distance,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        personId = booking.riderId,
        status = legStatus,
        legExtraInfo = legExtraInfo,
        actualDistance = distance,
        totalFare = mkPriceAPIEntity <$> (booking.finalPrice <|> Just booking.price),
        entrance = Nothing,
        exit = Nothing,
        validTill = (if null qrValidity then Nothing else Just $ maximum qrValidity) <|> Just booking.validTill
      }
  where
    mkLegExtraInfo qrDataList qrValidity ticketsCreatedAt journeyRouteDetails' legStatus journeyLegInfo' ticketNo = do
      mbBookingPayment <- QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
      refundBloc <- case mbBookingPayment of
        Just bookingPayment -> do
          refundEntries <- QRefunds.findAllByOrderId bookingPayment.paymentOrderId
          let matchingRefundEntry =
                find
                  ( \refundEntry ->
                      case refundEntry.split of
                        Just splits -> any (\split -> split.frfsBookingId == booking.id.getId) splits
                        Nothing -> False
                  )
                  refundEntries
          case matchingRefundEntry of
            Just refundEntry -> do
              let amount = case refundEntry.split of
                    Just splits -> find (\split -> split.frfsBookingId == booking.id.getId) splits <&> (.splitAmount)
                    Nothing -> Just refundEntry.refundAmount
              case amount of
                Just amount' -> return $ Just $ LegSplitInfo {amount = amount', status = refundEntry.status}
                Nothing -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
      case booking.vehicleType of
        Spec.METRO -> do
          return $
            Metro $
              MetroLegExtraInfo
                { routeInfo = journeyLegInfo',
                  legStatus = legStatus,
                  bookingId = Just booking.id,
                  tickets = Just qrDataList,
                  ticketValidity = Just qrValidity,
                  ticketsCreatedAt = Just ticketsCreatedAt,
                  providerName = Just booking.providerName,
                  ticketNo = Just ticketNo,
                  adultTicketQuantity = Just booking.quantity,
                  childTicketQuantity = booking.childTicketQuantity,
                  refund = refundBloc
                }
        Spec.BUS -> do
          journeyLegDetail <- listToMaybe journeyLegInfo' & fromMaybeM (InternalError "Journey Leg Detail not found")
          journeyRouteDetail <- listToMaybe journeyRouteDetails' & fromMaybeM (InternalError "Journey Route Detail not found")

          let fromStation = journeyLegDetail.originStop
          let toStation = journeyLegDetail.destinationStop
          let routeCode = journeyLegDetail.routeCode

          mbQuote <- QFRFSQuote.findById booking.quoteId
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Bus $
              BusLegExtraInfo
                { originStop = fromStation,
                  destinationStop = toStation,
                  legStatus = legStatus,
                  routeCode = routeCode,
                  bookingId = Just booking.id,
                  tickets = Just qrDataList,
                  ticketValidity = Just qrValidity,
                  ticketsCreatedAt = Just ticketsCreatedAt,
                  providerName = Just booking.providerName,
                  routeName = listToMaybe $ catMaybes $ map (.routeColorName) journeyRouteDetails',
                  frequency = listToMaybe $ catMaybes $ map (.frequency) journeyRouteDetails',
                  alternateShortNames = journeyRouteDetail.alternateShortNames,
                  selectedServiceTier = mbSelectedServiceTier,
                  ticketNo = Just ticketNo,
                  adultTicketQuantity = Just booking.quantity,
                  childTicketQuantity = booking.childTicketQuantity,
                  refund = refundBloc
                }
        Spec.SUBWAY -> do
          mbQuote <- QFRFSQuote.findById booking.quoteId
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          mbPerson <- QPerson.findById booking.riderId
          imeiNumber <- decrypt `mapM` (mbPerson >>= (.imeiNumber))
          let ticketValidityHours = liftA2 (\created validity -> round $ diffUTCTime validity created / 3600) ticketsCreatedAt qrValidity
          return $
            Subway $
              SubwayLegExtraInfo
                { routeInfo = journeyLegInfo',
                  bookingId = Just booking.id,
                  legStatus = legStatus,
                  tickets = Just qrDataList,
                  ticketValidity = Just qrValidity,
                  ticketsCreatedAt = Just ticketsCreatedAt,
                  ticketValidityHours = ticketValidityHours,
                  providerName = Just booking.providerName,
                  sdkToken = mbQuote >>= (.fareDetails) <&> (.sdkToken), -- required for show cris ticket
                  deviceId = imeiNumber, -- required for show cris ticket
                  providerRouteId = mbQuote >>= (.fareDetails) <&> (.providerRouteId), -- not required for show cris ticket but still sending for future use
                  ticketTypeCode = mbQuote >>= (.fareDetails) <&> (.ticketTypeCode), -- not required for cris sdk initiation
                  selectedServiceTier = mbSelectedServiceTier,
                  ticketNo = Just ticketNo,
                  adultTicketQuantity = Just booking.quantity,
                  childTicketQuantity = booking.childTicketQuantity,
                  refund = refundBloc
                }
    safeDiv :: (Eq a, Fractional a) => a -> a -> a
    safeDiv x 0 = x
    safeDiv x y = x / y

getLegRouteInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => [RouteDetails] -> DIBC.IntegratedBPPConfig -> m [LegRouteInfo]
getLegRouteInfo journeyRouteDetails integratedBPPConfig = do
  mapM transformJourneyRouteDetails journeyRouteDetails
  where
    transformJourneyRouteDetails :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => RouteDetails -> m LegRouteInfo
    transformJourneyRouteDetails journeyRouteDetail = do
      fromStationCode' <- fromMaybeM (InternalError "FromStationCode is missing") journeyRouteDetail.fromStopCode
      toStationCode' <- fromMaybeM (InternalError "ToStationCode is missing") journeyRouteDetail.toStopCode
      routeCode' <- fromMaybeM (InternalError "RouteCode is missing") (journeyRouteDetail.routeGtfsId <&> gtfsIdtoDomainCode)
      fromStation <- OTPRest.getStationByGtfsIdAndStopCode fromStationCode' integratedBPPConfig >>= fromMaybeM (InternalError $ "From Station not found in fetchPossibleRoutes: " <> show fromStationCode')
      toStation <- OTPRest.getStationByGtfsIdAndStopCode toStationCode' integratedBPPConfig >>= fromMaybeM (InternalError $ "To Station not found in fetchPossibleRoutes: " <> show toStationCode')
      route <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode' >>= fromMaybeM (RouteNotFound routeCode')
      validRoutes <- getRouteCodesFromTo fromStation.code toStation.code integratedBPPConfig
      return
        LegRouteInfo
          { originStop = stationToStationAPI fromStation,
            destinationStop = stationToStationAPI toStation,
            routeCode = route.code,
            subOrder = journeyRouteDetail.subLegOrder,
            platformNumber = journeyRouteDetail.fromStopPlatformCode,
            journeyStatus = journeyRouteDetail.journeyStatus,
            lineColor = journeyRouteDetail.routeColorName,
            lineColorCode = journeyRouteDetail.routeColorCode,
            trainNumber = Just route.shortName,
            frequency = journeyRouteDetail.frequency,
            allAvailableRoutes = validRoutes
          }

castCategoryToMode :: Spec.VehicleCategory -> DTrip.MultimodalTravelMode
castCategoryToMode Spec.METRO = DTrip.Metro
castCategoryToMode Spec.SUBWAY = DTrip.Subway
castCategoryToMode Spec.BUS = DTrip.Bus

mkLegInfoFromFrfsSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => FRFSSR.FRFSSearch -> Maybe HighPrecMoney -> Maybe Distance -> Maybe Seconds -> Maybe UTCTime -> m LegInfo
mkLegInfoFromFrfsSearchRequest frfsSearch@FRFSSR.FRFSSearch {..} fallbackFare distance duration startTime = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity frfsSearch
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  mRiderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing
  let isSearchFailed = fromMaybe False (journeyLegInfo >>= (.onSearchFailed))
  let bookingAllowed =
        case vehicleType of
          Spec.METRO -> not isSearchFailed && fromMaybe False (mRiderConfig >>= (.metroBookingAllowed))
          Spec.SUBWAY -> not isSearchFailed && fromMaybe False (mRiderConfig >>= (.suburbanBookingAllowed))
          Spec.BUS -> not isSearchFailed
  now <- getCurrentTime
  (mbEstimatedFare, mbQuote) <-
    case journeyLegInfo'.pricingId of
      Just quoteId -> do
        mbQuote <- QFRFSQuote.findById (Id quoteId)
        return $ (mkPriceAPIEntity <$> (mbQuote <&> (.price)), mbQuote)
      Nothing -> do
        if bookingAllowed && not journeyLegInfo'.skipBooking
          then do return (Nothing, Nothing)
          else return $ (mkPriceAPIEntity <$> (mkPrice Nothing <$> fallbackFare), Nothing)

  journeyLegRouteInfo' <- getLegRouteInfo journeyRouteDetails integratedBPPConfig

  legExtraInfo <- mkLegExtraInfo mbQuote journeyLegRouteInfo'

  return $
    LegInfo
      { journeyLegId = frfsSearch.journeyLegId,
        skipBooking = journeyLegInfo'.skipBooking,
        bookingAllowed,
        searchId = id.getId,
        pricingId = journeyLegInfo'.pricingId,
        travelMode = castCategoryToMode vehicleType,
        startTime = fromMaybe now startTime,
        order = journeyLegInfo'.journeyLegOrder,
        estimatedDuration = duration,
        estimatedMinFare = mbEstimatedFare,
        estimatedChildFare = mkPriceAPIEntity <$> (mbQuote >>= (.childPrice)),
        estimatedMaxFare = mbEstimatedFare,
        estimatedDistance = distance,
        estimatedTotalFare = Nothing,
        merchantId = merchantId,
        merchantOperatingCityId,
        personId = riderId,
        status = fromMaybe InPlan journeyLegStatus,
        legExtraInfo = legExtraInfo,
        actualDistance = Nothing,
        totalFare = Nothing,
        entrance = Nothing,
        exit = Nothing,
        validTill = (mbQuote <&> (.validTill)) <|> (frfsSearch.validTill)
      }
  where
    mkLegExtraInfo mbQuote journeyLegInfo' = do
      legStatus <-
        mapM
          ( \routeDetails -> do
              let mbLegSearchId = mbQuote <&> (.searchId.getId)
                  mbPricingId = mbQuote <&> (.id.getId)
              bookingStatus <- JMStateUtils.getFRFSJourneyBookingStatus mbLegSearchId mbPricingId
              trackingStatus <- JMStateUtils.getFRFSJourneyLegTrackingStatus mbLegSearchId mbPricingId routeDetails
              return $ JourneyFRFSLegStatus {trackingStatus = trackingStatus, bookingStatus = bookingStatus, subLegOrder = routeDetails.subLegOrder}
          )
          journeyRouteDetails
      case vehicleType of
        Spec.METRO -> do
          return $
            Metro $
              MetroLegExtraInfo
                { routeInfo = journeyLegInfo',
                  legStatus = legStatus,
                  bookingId = Nothing,
                  tickets = Nothing,
                  ticketValidity = Nothing,
                  ticketsCreatedAt = Nothing,
                  providerName = Nothing,
                  ticketNo = Nothing,
                  adultTicketQuantity = mbQuote <&> (.quantity),
                  childTicketQuantity = mbQuote >>= (.childTicketQuantity),
                  refund = Nothing
                }
        Spec.BUS -> do
          journeyLegDetail <- listToMaybe journeyLegInfo' & fromMaybeM (InternalError "Journey Leg Detail not found")
          journeyRouteDetail <- listToMaybe journeyRouteDetails & fromMaybeM (InternalError "Journey Route Detail not found")
          let fromStation = journeyLegDetail.originStop
          let toStation = journeyLegDetail.destinationStop
          let routeCode' = journeyLegDetail.routeCode

          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Bus $
              BusLegExtraInfo
                { originStop = fromStation,
                  destinationStop = toStation,
                  legStatus = legStatus,
                  routeCode = routeCode',
                  bookingId = Nothing,
                  tickets = Nothing,
                  ticketValidity = Nothing,
                  ticketsCreatedAt = Nothing,
                  providerName = Nothing,
                  selectedServiceTier = mbSelectedServiceTier,
                  alternateShortNames = journeyRouteDetail.alternateShortNames,
                  routeName = listToMaybe $ catMaybes $ map (.routeColorName) journeyRouteDetails,
                  frequency = listToMaybe $ catMaybes $ map (.frequency) journeyRouteDetails,
                  ticketNo = Nothing,
                  adultTicketQuantity = mbQuote <&> (.quantity),
                  childTicketQuantity = mbQuote >>= (.childTicketQuantity),
                  refund = Nothing
                }
        Spec.SUBWAY -> do
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Subway $
              SubwayLegExtraInfo
                { routeInfo = journeyLegInfo',
                  legStatus = legStatus,
                  bookingId = Nothing,
                  tickets = Nothing,
                  ticketValidity = Nothing,
                  ticketsCreatedAt = Nothing,
                  ticketValidityHours = [],
                  providerName = Nothing,
                  sdkToken = mbQuote >>= (.fareDetails) <&> (.sdkToken), -- required for cris sdk initiation
                  deviceId = Nothing, -- not required for cris sdk initiation
                  providerRouteId = mbQuote >>= (.fareDetails) <&> (.providerRouteId), -- required for cris sdk initiation
                  ticketTypeCode = mbQuote >>= (.fareDetails) <&> (.ticketTypeCode), -- required for cris sdk initiation
                  selectedServiceTier = mbSelectedServiceTier,
                  ticketNo = Nothing,
                  adultTicketQuantity = mbQuote <&> (.quantity),
                  childTicketQuantity = mbQuote >>= (.childTicketQuantity),
                  refund = Nothing
                }

getServiceTierFromQuote :: DFRFSQuote.FRFSQuote -> Maybe LegServiceTier
getServiceTierFromQuote quote = do
  let routeStations :: Maybe [FRFSTicketServiceAPI.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
  let mbServiceTier = listToMaybe $ mapMaybe (.vehicleServiceTier) (fromMaybe [] routeStations)
  mbServiceTier <&> \serviceTier -> do
    LegServiceTier
      { fare = mkPriceAPIEntity quote.price,
        quoteId = quote.id,
        serviceTierName = serviceTier.shortName,
        serviceTierType = serviceTier._type,
        serviceTierDescription = serviceTier.description,
        via = quote.fareDetails <&> (.via),
        trainTypeCode = quote.fareDetails <&> (.trainTypeCode)
      }

stationToStationAPI :: DTS.Station -> FRFSStationAPI
stationToStationAPI station =
  FRFSStationAPI
    { name = station.name,
      code = station.code,
      lat = station.lat,
      lon = station.lon,
      address = station.address,
      regionalName = station.regionalName,
      hindiName = station.hindiName,
      integratedBppConfigId = cast station.integratedBppConfigId
    }

mkSearchReqLocation :: LocationAddress -> Maps.LatLngV2 -> SLSearch.SearchReqLocation
mkSearchReqLocation address latLng = do
  SLSearch.SearchReqLocation
    { gps = LatLong {lat = latLng.latitude, lon = latLng.longitude},
      address = address
    }

mkJourney :: MonadFlow m => Id DP.Person -> Maybe UTCTime -> Maybe UTCTime -> Distance -> Seconds -> Id DJ.Journey -> Id DSR.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [EMInterface.MultiModalLeg] -> Meters -> Meters -> Maybe (Id DRL.RecentLocation) -> Maybe Double -> Bool -> Bool -> DLocation.Location -> Maybe DLocation.Location -> m DJ.Journey
mkJourney riderId startTime endTime estimatedDistance estiamtedDuration journeyId parentSearchId merchantId merchantOperatingCityId legs maximumWalkDistance straightLineThreshold mbRecentLocationId relevanceScore hasUserPreferredServiceTier hasUserPreferredTransitModes fromLocation toLocation = do
  let journeyLegsCount = length legs
      modes = map (\x -> convertMultiModalModeToTripMode x.mode (straightLineDistance x) (distanceToMeters x.distance) maximumWalkDistance straightLineThreshold) legs
  let isPublicTransportIncluded = any (`elem` [DTrip.Bus, DTrip.Metro, DTrip.Subway]) modes
  now <- getCurrentTime
  return $
    DJ.Journey
      { convenienceCost = 0,
        estimatedDistance = estimatedDistance,
        estimatedDuration = Just estiamtedDuration,
        id = journeyId,
        isPaymentSuccess = Nothing,
        totalLegs = journeyLegsCount,
        modes = modes,
        searchRequestId = parentSearchId.getId,
        merchantId = merchantId,
        status = DJ.NEW,
        riderId,
        startTime,
        endTime,
        merchantOperatingCityId = merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        DJ.recentLocationId = mbRecentLocationId, -- Fully qualify the field name
        isPublicTransportIncluded = Just isPublicTransportIncluded,
        relevanceScore,
        hasPreferredServiceTier = Just hasUserPreferredServiceTier,
        hasPreferredTransitModes = Just hasUserPreferredTransitModes,
        paymentOrderShortId = Nothing,
        journeyExpiryTime = Nothing,
        ..
      }
  where
    straightLineDistance leg = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)

mkJourneyLeg :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) => Int -> EMInterface.MultiModalLeg -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DJ.Journey -> Meters -> Meters -> Maybe GetFareResponse -> Maybe EMInterface.MultiModalLeg -> Maybe EMInterface.MultiModalLeg -> m DJL.JourneyLeg
mkJourneyLeg idx leg merchantId merchantOpCityId journeyId maximumWalkDistance straightLineThreshold fare mbPrev mbNext = do
  now <- getCurrentTime
  journeyLegId <- generateGUID
  routeDetails <- mapM (mkRouteDetail journeyLegId) leg.routeDetails
  let journeyLeg =
        DJL.JourneyLeg
          { agency = leg.agency,
            distance = Just leg.distance,
            duration = Just leg.duration,
            endLocation = leg.endLocation.latLng,
            fromArrivalTime = leg.fromArrivalTime,
            fromDepartureTime = leg.fromDepartureTime,
            fromStopDetails = leg.fromStopDetails,
            id = journeyLegId,
            journeyId,
            mode = convertMultiModalModeToTripMode leg.mode straightLineDistance (distanceToMeters leg.distance) maximumWalkDistance straightLineThreshold,
            -- polylinePoints = leg.polyline.encodedPolyline,
            routeDetails,
            sequenceNumber = idx,
            startLocation = leg.startLocation.latLng,
            toArrivalTime = leg.toArrivalTime,
            toDepartureTime = leg.toDepartureTime,
            toStopDetails = leg.toStopDetails,
            serviceTypes = fare >>= (.serviceTypes),
            estimatedMinFare = fare <&> (.estimatedMinFare),
            estimatedMaxFare = fare <&> (.estimatedMaxFare),
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now,
            legSearchId = Nothing,
            isDeleted = Just False,
            isSkipped = Just False,
            changedBusesInSequence = Nothing,
            finalBoardedBusNumber = Nothing,
            entrance = leg.entrance,
            exit = leg.exit,
            status = Nothing,
            osmEntrance = Nothing,
            osmExit = Nothing,
            straightLineEntrance = Nothing,
            straightLineExit = Nothing
          }
  getUpdatedLeg journeyLeg mbPrev mbNext merchantId merchantOpCityId
  where
    straightLineDistance = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)

    mkRouteDetail :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id DJL.JourneyLeg -> EMInterface.MultiModalRouteDetails -> m RouteDetails
    mkRouteDetail journeyLegId routeDetail = do
      now <- getCurrentTime
      newId <- generateGUID
      let fromStopDetails' = fromMaybe (EMInterface.MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.fromStopDetails)
          toStopDetails' = fromMaybe (EMInterface.MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.toStopDetails)
      return $
        RouteDetails
          { routeGtfsId = routeDetail.gtfsId <&> gtfsIdtoDomainCode,
            routeCode = routeDetail.gtfsId <&> gtfsIdtoDomainCode,
            id = newId,
            routeLongName = routeDetail.longName,
            routeShortName = routeDetail.shortName,
            routeColorName = routeDetail.shortName,
            routeColorCode = routeDetail.color,
            frequency = Nothing,
            alternateShortNames = routeDetail.alternateShortNames,
            journeyLegId = journeyLegId.getId,
            agencyGtfsId = routeDetail.gtfsId <&> gtfsIdtoDomainCode,
            agencyName = routeDetail.longName,
            subLegOrder = Just routeDetail.subLegOrder,
            --fromStopDetails:
            fromStopCode = fromStopDetails'.stopCode,
            fromStopName = fromStopDetails'.name,
            fromStopGtfsId = fromStopDetails'.gtfsId <&> gtfsIdtoDomainCode,
            fromStopPlatformCode = fromStopDetails'.platformCode,
            --toStopDetails:
            toStopCode = toStopDetails'.stopCode,
            toStopName = toStopDetails'.name,
            toStopGtfsId = toStopDetails'.gtfsId <&> gtfsIdtoDomainCode,
            toStopPlatformCode = toStopDetails'.platformCode,
            --Times --
            fromArrivalTime = routeDetail.fromArrivalTime,
            fromDepartureTime = routeDetail.fromDepartureTime,
            toArrivalTime = routeDetail.toArrivalTime,
            toDepartureTime = routeDetail.toDepartureTime,
            --startLocation:
            startLocationLat = routeDetail.startLocation.latLng.latitude,
            startLocationLon = routeDetail.startLocation.latLng.longitude,
            --endLocation:
            endLocationLat = routeDetail.endLocation.latLng.latitude,
            endLocationLon = routeDetail.endLocation.latLng.longitude,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            trackingStatus = Nothing,
            journeyStatus = Nothing, -- TODO :: Remove this field when `trackingStatus` is added to the database and consumed
            createdAt = now,
            updatedAt = now
          }

getServiceTypeFromProviderCode :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Text -> m Spec.ServiceTierType
getServiceTypeFromProviderCode merchantOperatingCityId providerCode = do
  serviceTiers <- QFRFSVehicleServiceTier.findByProviderCode providerCode merchantOperatingCityId
  return $ fromMaybe Spec.ORDINARY (listToMaybe serviceTiers <&> (._type))

sumHighPrecMoney :: [HighPrecMoney] -> HighPrecMoney
sumHighPrecMoney = HighPrecMoney . sum . map getHighPrecMoney

completedStatus :: [JourneyLegStatus]
completedStatus = [Completed, Cancelled]

allCompletedStatus :: [JourneyLegStatus]
allCompletedStatus = [Completed, Cancelled, Skipped]

cannotCancelStatus :: [JourneyLegStatus]
cannotCancelStatus = [Skipped, Ongoing, Finishing, Completed, Cancelled]

cannotCancelWalkStatus :: [JourneyLegStatus]
cannotCancelWalkStatus = [Skipped, Completed, Cancelled]

cannotSwitchStatus :: [JourneyLegStatus]
cannotSwitchStatus = [Booked, OnTheWay, Arriving, Arrived, Ongoing, Finishing, Completed, Cancelled]

cannotCompleteJourneyIfTaxiLegIsInThisStatus :: [JourneyLegStatus]
cannotCompleteJourneyIfTaxiLegIsInThisStatus = [Booked, OnTheWay, Arriving, Arrived, Ongoing, Finishing]

cannotCancelExtendStatus :: [JourneyLegStatus]
cannotCancelExtendStatus = [Ongoing, Finishing, Completed, Cancelled, Booked, OnTheWay, Arriving, Arrived]

data ExtendLegStartPoint
  = StartLocation StartLocationType
  | StartLegOrder Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StartLocationType = StartLocationType
  { location :: DLocation.LocationAPIEntity,
    legOrder :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GateFieldSpec = GateFieldSpec
  { getEntrance :: DJourneyLeg.JourneyLeg -> Maybe KEMIT.MultiModalLegGate,
    getExit :: DJourneyLeg.JourneyLeg -> Maybe KEMIT.MultiModalLegGate,
    setEntrance :: DJourneyLeg.JourneyLeg -> Maybe KEMIT.MultiModalLegGate -> DJourneyLeg.JourneyLeg,
    setExit :: DJourneyLeg.JourneyLeg -> Maybe KEMIT.MultiModalLegGate -> DJourneyLeg.JourneyLeg,
    otherMode :: DTrip.MultimodalTravelMode,
    label :: Text
  }

getUpdatedLeg ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
  DJourneyLeg.JourneyLeg ->
  Maybe KEMIT.MultiModalLeg ->
  Maybe KEMIT.MultiModalLeg ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m DJourneyLeg.JourneyLeg
getUpdatedLeg journeyLeg' mbPrev mbNext merchantId merchantOpCityId = do
  if journeyLeg'.mode `notElem` [DTrip.Walk, DTrip.Taxi]
    then pure journeyLeg'
    else do
      entrances <- case mbNext of
        Just nxt -> fetchStationGatesFromLeg nxt merchantOpCityId True
        Nothing -> pure Nothing
      exits <- case mbPrev of
        Just prev -> fetchStationGatesFromLeg prev merchantOpCityId False
        Nothing -> pure Nothing
      osmLeg <- updateGatesGeneric journeyLeg' mbPrev mbNext taxiSpec updateGateFromDomain merchantId merchantOpCityId entrances exits
      updateGatesGeneric osmLeg mbPrev mbNext walkSpec updateGateFromDomain merchantId merchantOpCityId entrances exits

updateGatesGeneric ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
  DJourneyLeg.JourneyLeg ->
  Maybe KEMIT.MultiModalLeg ->
  Maybe KEMIT.MultiModalLeg ->
  GateFieldSpec ->
  (Maybe KEMIT.MultiModalLegGate -> Maybe DStation.Gate -> Maybe KEMIT.MultiModalLegGate) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe [DStation.Gate] ->
  Maybe [DStation.Gate] ->
  m DJourneyLeg.JourneyLeg
updateGatesGeneric leg mbPrev mbNext spec transformGate merchantId merchantOpCityId entrances exits = do
  let oldEntrance = spec.getEntrance leg
  let oldExit = spec.getExit leg
  entrance <- case oldEntrance of
    Just _ -> pure oldEntrance
    Nothing -> do
      e <- getGate leg.mode (LatLong leg.startLocation.latitude leg.startLocation.longitude) mbNext spec.otherMode merchantId merchantOpCityId True entrances exits
      pure $ transformGate leg.entrance e
  exit <- case oldExit of
    Just _ -> pure oldExit
    Nothing -> do
      e <- getGate leg.mode (LatLong leg.endLocation.latitude leg.endLocation.longitude) mbPrev spec.otherMode merchantId merchantOpCityId False entrances exits
      pure $ transformGate leg.exit e
  let updatedLeg = spec.setEntrance (spec.setExit leg exit) entrance
  when (oldEntrance /= entrance || oldExit /= exit) $ do
    logDebug $
      "Updating " <> spec.label
        <> ": legId="
        <> show leg.id
        <> " mode="
        <> show leg.mode
        <> " oldEntrance="
        <> show oldEntrance
        <> " oldExit="
        <> show oldExit
        <> " entrance="
        <> show entrance
        <> " exit="
        <> show exit
  pure updatedLeg

taxiSpec :: GateFieldSpec
taxiSpec =
  GateFieldSpec
    { getEntrance = DJourneyLeg.osmEntrance,
      getExit = DJourneyLeg.osmExit,
      setEntrance = \leg g -> leg {DJourneyLeg.osmEntrance = g},
      setExit = \leg g -> leg {DJourneyLeg.osmExit = g},
      otherMode = DTrip.Walk,
      label = "OSM gates"
    }

walkSpec :: GateFieldSpec
walkSpec =
  GateFieldSpec
    { getEntrance = DJourneyLeg.straightLineEntrance,
      getExit = DJourneyLeg.straightLineExit,
      setEntrance = \leg g -> leg {DJourneyLeg.straightLineEntrance = g},
      setExit = \leg g -> leg {DJourneyLeg.straightLineExit = g},
      otherMode = DTrip.Taxi,
      label = "Straight line gates"
    }

generalVehicleTypeToBecknVehicleCategory :: KEMIT.GeneralVehicleType -> Maybe BecknSpec.VehicleCategory
generalVehicleTypeToBecknVehicleCategory = \case
  KEMIT.MetroRail -> Just BecknSpec.METRO
  KEMIT.Bus -> Just BecknSpec.BUS
  KEMIT.Subway -> Just BecknSpec.SUBWAY
  _ -> Nothing

generalVehicleTypeToMultiModalTravelMode :: KEMIT.GeneralVehicleType -> DTrip.MultimodalTravelMode
generalVehicleTypeToMultiModalTravelMode = \case
  KEMIT.MetroRail -> DTrip.Metro
  KEMIT.Bus -> DTrip.Bus
  KEMIT.Subway -> DTrip.Subway
  _ -> DTrip.Walk

getGate :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) => DTrip.MultimodalTravelMode -> LatLong -> Maybe KEMIT.MultiModalLeg -> DTrip.MultimodalTravelMode -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Maybe [DStation.Gate] -> Maybe [DStation.Gate] -> m (Maybe DStation.Gate)
getGate mode point mbOther otherMode merchantId merchantOpCityId isEntrance entrances exits = case mbOther of
  Just other | generalVehicleTypeToMultiModalTravelMode other.mode `notElem` [mode, otherMode] -> getNearestGateFromLeg mode point merchantId merchantOpCityId isEntrance entrances exits
  _ -> pure Nothing

getNearestGateFromLeg :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) => DTrip.MultimodalTravelMode -> LatLong -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Maybe [DStation.Gate] -> Maybe [DStation.Gate] -> m (Maybe DStation.Gate)
getNearestGateFromLeg mode point merchantId merchantOpCityId isEntrance entrances exits = do
  let mbGates = if isEntrance then entrances else exits
  case mbGates of
    Nothing -> pure Nothing
    Just gates ->
      if mode == DTrip.Taxi
        then getNearestOSMGate point gates merchantId merchantOpCityId
        else pure $ minimumByMay (compareDist point) gates
  where
    compareDist p g1 g2 =
      compare
        (distanceBetweenInMeters p (LatLong g1.lat g1.lon))
        (distanceBetweenInMeters p (LatLong g2.lat g2.lon))

fetchStationGatesFromLeg :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) => KEMIT.MultiModalLeg -> Id DMOC.MerchantOperatingCity -> Bool -> m (Maybe [DStation.Gate])
fetchStationGatesFromLeg leg merchantOpCityId isEntrance =
  if (generalVehicleTypeToMultiModalTravelMode leg.mode) `elem` [DTrip.Walk, DTrip.Taxi]
    then pure Nothing
    else runMaybeT $ do
      becknVehicleCategory <- hoistMaybe $ generalVehicleTypeToBecknVehicleCategory leg.mode
      let platformType = DTBC.MULTIMODAL
      integratedBPPConfigs <- lift $ SIBC.findAllIntegratedBPPConfig merchantOpCityId becknVehicleCategory platformType
      stopCode <-
        if isEntrance
          then hoistMaybe (leg.fromStopDetails >>= (.stopCode))
          else hoistMaybe (leg.toStopDetails >>= (.stopCode))
      mbStation <-
        MaybeT $
          SIBC.fetchFirstIntegratedBPPConfigRightResult integratedBPPConfigs (OTPRest.getStationByGtfsIdAndStopCode stopCode)
      station <- hoistMaybe mbStation
      gates <- hoistMaybe station.gates
      lift $ logDebug $ "Station found with gates: " <> show gates <> show stopCode
      pure gates

updateGateFromDomain :: Maybe KEMIT.MultiModalLegGate -> Maybe DStation.Gate -> Maybe KEMIT.MultiModalLegGate
updateGateFromDomain oldGate domainGate =
  case (oldGate, domainGate) of
    (Just g, Just d) ->
      Just
        g
          { KEMIT.lat = Just (d.lat),
            KEMIT.lon = Just (d.lon),
            KEMIT.streetName = Just (Text.pack d.gateName)
          }
    _ -> oldGate

getNearestOSMGate :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) => LatLong -> [DStation.Gate] -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m (Maybe DStation.Gate)
getNearestOSMGate point gates merchantId merchantOpCityId =
  case NE.nonEmpty gates of
    Nothing -> pure Nothing
    Just gates' -> runMaybeT $ do
      let req =
            GetDistancesReq
              { origins = pure point,
                destinations = gates',
                travelMode = Just Maps.CAR,
                sourceDestinationMapping = Nothing,
                distanceUnit = Meter
              }
      distances <- lift $ Maps.getDistances merchantId merchantOpCityId Nothing req
      nearest <- hoistMaybe $ minimumByMay (\r1 r2 -> compare r1.distance r2.distance) (toList distances)
      pure (nearest.destination)
