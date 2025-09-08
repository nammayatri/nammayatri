module Lib.JourneyModule.Types where

import API.Types.RiderPlatform.Management.FRFSTicket
import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Applicative (liftA2, (<|>))
import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as FRFSSR
import qualified Domain.Types.FRFSTicketBooking as DFRFSBooking
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.IntegratedBPPConfig as DTBC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RecentLocation as DRL
import qualified Domain.Types.Ride as DRide
import Domain.Types.RouteDetails
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.Station as DTS
import qualified Domain.Types.Station as DStation
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
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Types as YTypes
import SharedLogic.Booking (getfareBreakups)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.Ride as DARide
import qualified SharedLogic.Search as SLSearch
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
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
    HasField "isMetroTestTransaction" r Bool,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    m ~ Kernel.Types.Flow.FlowR AppEnv
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

type UpdateJourneyLeg leg m = leg -> m ()

type GetJourneyLegState leg m = leg -> m JourneyLegState

type GetJourneyLeg leg m = leg -> m (Maybe LegInfo)

class JourneyLeg leg m where
  search :: SearchRequestFlow m r c => SearchJourneyLeg leg m
  confirm :: ConfirmFlow m r c => ConfirmJourneyLeg leg m
  update :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => UpdateJourneyLeg leg m
  cancel :: CancelFlow m r c => CancelJourneyLeg leg m
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
    route_state :: Maybe CQMMB.RouteState,
    upcomingStops :: [NextStopDetails] -- List of upcoming stops for this vehicle
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegStateData = JourneyLegStateData
  { status :: JourneyLegStatus, -- TODO :: This field would be deprecated
    bookingStatus :: JMState.JourneyBookingStatus,
    trackingStatus :: JMState.TrackingStatus,
    userPosition :: Maybe LatLong,
    vehiclePositions :: [VehiclePosition], -- Uses the modified VehiclePosition
    subLegOrder :: Int,
    legOrder :: Int,
    mode :: DTrip.MultimodalTravelMode,
    fleetNo :: Maybe Text
    -- boardedVehicles field removed
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetFareResponse = GetFareResponse {estimatedMinFare :: HighPrecMoney, estimatedMaxFare :: HighPrecMoney, serviceTypes :: Maybe [Spec.ServiceTierType], possibleRoutes :: Maybe [AvailableRoutesByTier]}
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
    relevanceScore :: Maybe Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegInfo = LegInfo
  { journeyLegId :: Id DJL.JourneyLeg,
    skipBooking :: Bool,
    bookingAllowed :: Bool,
    pricingId :: Maybe Text,
    searchId :: Text,
    travelMode :: DTrip.MultimodalTravelMode,
    startTime :: UTCTime,
    order :: Int,
    status :: JourneyLegStatus, -- TODO :: To be Deprecated, remove this once UI starts consuming `legStatus` instead.
    bookingStatus :: JMState.JourneyBookingStatus,
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
    trackingStatus :: JMState.TrackingStatus,
    id :: Id DJourneyLeg.JourneyLeg
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TaxiLegExtraInfo = TaxiLegExtraInfo
  { origin :: Location,
    destination :: Location,
    driverName :: Maybe Text,
    vehicleNumber :: Maybe Text,
    otp :: Maybe Text,
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
    exoPhoneNumber :: Maybe Text,
    trackingStatus :: JMState.TrackingStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLegExtraInfo = MetroLegExtraInfo
  { routeInfo :: [LegRouteInfo],
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
    trackingStatus :: JMState.TrackingStatus,
    frequency :: Maybe Seconds,
    allAvailableRoutes :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BusLegExtraInfo = BusLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
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
    refund :: Maybe LegSplitInfo,
    trackingStatus :: JMState.TrackingStatus,
    fleetNo :: Maybe Text,
    discounts :: Maybe [FRFSTicketServiceAPI.FRFSDiscountRes]
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

getTaxiVehiclePosition :: GetStateFlow m r c => Maybe DRide.Ride -> m (Maybe LatLong)
getTaxiVehiclePosition mRide = do
  case mRide of
    Just ride -> do
      driverLocationResp <- try @_ @SomeException $ DARide.getDriverLoc ride.id
      case driverLocationResp of
        Left err -> do
          logError $ "location fetch failed: " <> show err
          return Nothing
        Right driverLocation -> return (Just $ LatLong driverLocation.lat driverLocation.lon)
    Nothing -> return Nothing

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

mkLegInfoFromBookingAndRide :: GetStateFlow m r c => DBooking.Booking -> Maybe DRide.Ride -> DJourneyLeg.JourneyLeg -> m LegInfo
mkLegInfoFromBookingAndRide booking mRide journeyLeg = do
  toLocation <- QTB.getToLocation booking.bookingDetails & fromMaybeM (InvalidRequest "To Location not found")
  (fareBreakups, estimatedFareBreakups) <- getfareBreakups booking mRide
  tollDifference <- getTollDifference fareBreakups estimatedFareBreakups
  batchConfig <- SharedRedisKeys.getBatchConfig booking.transactionId
  (oldStatus, bookingStatus, trackingStatus) <- JMStateUtils.getTaxiAllStatuses journeyLeg (Just booking) mRide Nothing
  return $
    LegInfo
      { journeyLegId = journeyLeg.id,
        skipBooking = False,
        bookingAllowed = True,
        searchId = booking.transactionId,
        pricingId = booking.quoteId <&> (.getId),
        travelMode = DTrip.Taxi,
        startTime = booking.startTime,
        order = journeyLeg.sequenceNumber,
        estimatedDuration = booking.estimatedDuration,
        estimatedMinFare = Just $ mkPriceAPIEntity booking.estimatedFare,
        estimatedChildFare = Nothing,
        estimatedMaxFare = Just $ mkPriceAPIEntity booking.estimatedFare,
        estimatedTotalFare = Just $ mkPriceAPIEntity booking.estimatedTotalFare,
        estimatedDistance = booking.estimatedDistance,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        personId = booking.riderId,
        status = oldStatus,
        bookingStatus = bookingStatus,
        legExtraInfo =
          Taxi $
            TaxiLegExtraInfo
              { origin = booking.fromLocation,
                destination = toLocation,
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
                exoPhoneNumber = Just booking.primaryExophone,
                trackingStatus = trackingStatus
              },
        actualDistance = mRide >>= (.traveledDistance),
        totalFare = mkPriceAPIEntity <$> (mRide >>= (.totalFare)),
        entrance = journeyLeg.osmEntrance,
        exit = journeyLeg.osmExit,
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

mkLegInfoFromSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DSR.SearchRequest -> DJourneyLeg.JourneyLeg -> m LegInfo
mkLegInfoFromSearchRequest DSR.SearchRequest {..} journeyLeg = do
  (mbFareRange, mbEstimate) <-
    case journeyLeg.legPricingId of
      Just estId -> do
        mbEst <- QEstimate.findById (Id estId)
        return $ (mbEst <&> (.totalFareRange), mbEst)
      Nothing -> return (Nothing, Nothing)
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  batchConfig <- SharedRedisKeys.getBatchConfig id.getId
  (oldStatus, bookingStatus, trackingStatus) <- JMStateUtils.getTaxiAllStatuses journeyLeg Nothing Nothing mbEstimate
  return $
    LegInfo
      { journeyLegId = journeyLeg.id,
        skipBooking = False, -- TODO :: To be deprecated from UI @Khuzema
        bookingAllowed = True,
        searchId = id.getId,
        pricingId = journeyLeg.legPricingId,
        travelMode = DTrip.Taxi,
        startTime = startTime,
        order = journeyLeg.sequenceNumber,
        estimatedDuration = estimatedRideDuration,
        estimatedChildFare = Nothing,
        estimatedMinFare = mkPriceAPIEntity <$> (mbFareRange <&> (.minFare)),
        estimatedMaxFare = mkPriceAPIEntity <$> (mbFareRange <&> (.maxFare)),
        estimatedTotalFare = Nothing,
        estimatedDistance = distance,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        personId = riderId,
        status = oldStatus,
        bookingStatus = bookingStatus,
        legExtraInfo =
          Taxi $
            TaxiLegExtraInfo
              { origin = fromLocation,
                destination = toLocation',
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
                exoPhoneNumber = Nothing,
                trackingStatus = trackingStatus
              },
        actualDistance = Nothing,
        totalFare = Nothing,
        entrance = journeyLeg.osmEntrance,
        exit = journeyLeg.osmExit,
        validTill = Nothing
      }

mkWalkLegInfoFromWalkLegData :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id DP.Person -> DJL.JourneyLeg -> m LegInfo
mkWalkLegInfoFromWalkLegData personId legData@DJL.JourneyLeg {..} = do
  let (oldStatus, trackingStatus) = JMStateUtils.getWalkAllStatuses legData
  now <- getCurrentTime
  return $
    LegInfo
      { journeyLegId = id,
        skipBooking = False,
        bookingAllowed = False,
        searchId = id.getId,
        pricingId = Just id.getId,
        travelMode = DTrip.Walk,
        startTime = fromMaybe now fromDepartureTime,
        order = sequenceNumber,
        estimatedDuration = duration,
        estimatedMinFare = Nothing,
        estimatedMaxFare = Nothing,
        estimatedTotalFare = Nothing,
        estimatedChildFare = Nothing,
        estimatedDistance = distance,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        personId = personId,
        status = oldStatus, -- TODO :: This field would be deprecated
        bookingStatus = JMState.Initial JMState.BOOKING_PENDING,
        legExtraInfo =
          Walk $
            WalkLegExtraInfo
              { origin = mkLocation now startLocation (fromStopDetails >>= (.name)),
                destination = mkLocation now endLocation (toStopDetails >>= (.name)),
                id = id,
                trackingStatus
              },
        actualDistance = distance,
        totalFare = Nothing,
        entrance = straightLineEntrance,
        exit = straightLineExit,
        validTill = Nothing
      }
  where
    mkLocation now location name =
      Location
        { id = "walk-leg-id", -- change reposne type to api entity
          lat = location.latitude,
          lon = location.longitude,
          address = mkAddress name,
          merchantId = Just merchantId,
          merchantOperatingCityId = Just merchantOperatingCityId,
          createdAt = now,
          updatedAt = now
        }
    mkAddress name =
      LocationAddress
        { street = Nothing,
          door = Nothing,
          city = Nothing,
          state = Nothing,
          country = Nothing,
          building = Nothing,
          areaCode = Nothing,
          area = name,
          ward = Nothing,
          placeId = Nothing,
          instructions = Nothing,
          title = name,
          extras = Nothing
        }

mkLegInfoFromFrfsBooking ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => DFRFSBooking.FRFSTicketBooking -> DJourneyLeg.JourneyLeg -> m LegInfo
mkLegInfoFromFrfsBooking booking journeyLeg = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
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

  now <- getCurrentTime

  let startTime = fromMaybe now booking.startTime
  let singleAdultPrice = roundToTwoDecimalPlaces . HighPrecMoney $ safeDiv (getHighPrecMoney booking.price.amount) (fromIntegral booking.quantity) -- TODO :: To be handled as single price cannot be obtained from this if more than 1 fare breakup (Child Quantity / Discounts)
      estimatedPrice =
        Price
          { amount = singleAdultPrice,
            amountInt = Money $ roundToIntegral singleAdultPrice,
            currency = booking.price.currency
          }

  (oldStatus, bookingStatus, trackingStatuses) <- JMStateUtils.getFRFSAllStatuses journeyLeg (Just booking)
  journeyLegInfo' <- getLegRouteInfo (zip journeyLeg.routeDetails (map snd trackingStatuses)) integratedBPPConfig
  legExtraInfo <- mkLegExtraInfo qrDataList qrValidity ticketsCreatedAt journeyLeg.routeDetails journeyLegInfo' ticketNo
  return $
    LegInfo
      { journeyLegId = journeyLeg.id,
        skipBooking = False, -- TODO :: To be deprecated from UI @Khuzema
        bookingAllowed = True,
        searchId = booking.searchId.getId,
        pricingId = Just booking.quoteId.getId, -- Just booking.id.getId,
        travelMode = castCategoryToMode booking.vehicleType,
        startTime = startTime,
        order = journeyLeg.sequenceNumber,
        estimatedDuration = journeyLeg.duration,
        estimatedMinFare = Just $ mkPriceAPIEntity estimatedPrice,
        estimatedChildFare = Nothing,
        estimatedMaxFare = Just $ mkPriceAPIEntity estimatedPrice,
        estimatedTotalFare = Nothing,
        estimatedDistance = journeyLeg.distance,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        personId = booking.riderId,
        status = oldStatus,
        bookingStatus,
        legExtraInfo = legExtraInfo,
        actualDistance = journeyLeg.distance,
        totalFare = mkPriceAPIEntity <$> (booking.finalPrice <|> Just booking.price),
        entrance = Nothing,
        exit = Nothing,
        validTill = (if null qrValidity then Nothing else Just $ maximum qrValidity) <|> Just booking.validTill
      }
  where
    mkLegExtraInfo qrDataList qrValidity ticketsCreatedAt journeyRouteDetails journeyLegInfo' ticketNo = do
      mbBookingPayment <- QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
      refundBloc <- case mbBookingPayment of
        Just bookingPayment -> do
          mbPaymentOrder <- QPaymentOrder.findById bookingPayment.paymentOrderId
          case mbPaymentOrder of
            Just paymentOrder -> do
              refundEntries <- QRefunds.findAllByOrderId paymentOrder.shortId
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
        Nothing -> return Nothing
      case booking.vehicleType of
        Spec.METRO -> do
          return $
            Metro $
              MetroLegExtraInfo
                { routeInfo = journeyLegInfo',
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
          journeyRouteDetail <- listToMaybe journeyRouteDetails & fromMaybeM (InternalError "Journey Route Detail not found")

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
                  routeCode = routeCode,
                  bookingId = Just booking.id,
                  tickets = Just qrDataList,
                  ticketValidity = Just qrValidity,
                  ticketsCreatedAt = Just ticketsCreatedAt,
                  providerName = Just booking.providerName,
                  routeName = journeyRouteDetail.routeColorName,
                  frequency = journeyRouteDetail.frequency,
                  alternateShortNames = journeyRouteDetail.alternateShortNames,
                  selectedServiceTier = mbSelectedServiceTier,
                  ticketNo = Just ticketNo,
                  adultTicketQuantity = Just booking.quantity,
                  childTicketQuantity = booking.childTicketQuantity,
                  refund = refundBloc,
                  trackingStatus = journeyLegDetail.trackingStatus,
                  fleetNo = journeyLeg.finalBoardedBusNumber,
                  discounts = mbQuote >>= (.discountsJson) >>= decodeFromText
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

getLegRouteInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => [(RouteDetails, JMState.TrackingStatus)] -> DIBC.IntegratedBPPConfig -> m [LegRouteInfo]
getLegRouteInfo journeyRouteDetailsWithTrackingStatuses integratedBPPConfig = do
  mapM transformJourneyRouteDetails journeyRouteDetailsWithTrackingStatuses
  where
    transformJourneyRouteDetails :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => (RouteDetails, JMState.TrackingStatus) -> m LegRouteInfo
    transformJourneyRouteDetails (journeyRouteDetail, trackingStatus) = do
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
            journeyStatus = Just $ JMStateUtils.castTrackingStatusToJourneyLegStatus trackingStatus,
            trackingStatus,
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

mkLegInfoFromFrfsSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => FRFSSR.FRFSSearch -> DJourneyLeg.JourneyLeg -> m LegInfo
mkLegInfoFromFrfsSearchRequest frfsSearch@FRFSSR.FRFSSearch {..} journeyLeg = do
  let fallbackFare = journeyLeg.estimatedMinFare
  let distance = journeyLeg.distance
  let duration = journeyLeg.duration
  let startTime = journeyLeg.fromDepartureTime

  integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity frfsSearch
  mRiderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing
  person <- QPerson.findById riderId >>= fromMaybeM (PersonNotFound riderId.getId)
  let isPTBookingAllowedForUser = ("PTBookingAllowed#Yes" `elem` (maybe [] (map YTypes.getTagNameValueExpiry) person.customerNammaTags))
  let isSearchFailed = fromMaybe False onSearchFailed
  let bookingAllowed =
        case vehicleType of
          Spec.METRO -> not isSearchFailed && (fromMaybe False (mRiderConfig >>= (.metroBookingAllowed)) || isPTBookingAllowedForUser)
          Spec.SUBWAY -> not isSearchFailed && (fromMaybe False (mRiderConfig >>= (.suburbanBookingAllowed)) || isPTBookingAllowedForUser)
          Spec.BUS -> not isSearchFailed && (fromMaybe False (mRiderConfig >>= (.busBookingAllowed)) || isPTBookingAllowedForUser)
  now <- getCurrentTime
  (oldStatus, bookingStatus, trackingStatuses) <- JMStateUtils.getFRFSAllStatuses journeyLeg Nothing
  (mbEstimatedFare, mbQuote) <-
    case journeyLeg.legPricingId of
      Just quoteId -> do
        mbQuote <- QFRFSQuote.findById (Id quoteId)
        return $ (mkPriceAPIEntity <$> (mbQuote <&> (.price)), mbQuote)
      Nothing -> do
        if bookingAllowed
          then do return (Nothing, Nothing)
          else return $ (mkPriceAPIEntity <$> (mkPrice Nothing <$> fallbackFare), Nothing)

  journeyLegRouteInfo' <- getLegRouteInfo (zip journeyLeg.routeDetails (map snd trackingStatuses)) integratedBPPConfig
  legExtraInfo <- mkLegExtraInfo mbQuote journeyLegRouteInfo'

  return $
    LegInfo
      { journeyLegId = journeyLeg.id,
        skipBooking = False, -- TODO :: To be deprecated from UI @Khuzema
        bookingAllowed,
        searchId = id.getId,
        pricingId = journeyLeg.legPricingId,
        travelMode = castCategoryToMode vehicleType,
        startTime = fromMaybe now startTime,
        order = journeyLeg.sequenceNumber,
        estimatedDuration = duration,
        estimatedMinFare = mbEstimatedFare,
        estimatedChildFare = mkPriceAPIEntity <$> (mbQuote >>= (.childPrice)),
        estimatedMaxFare = mbEstimatedFare,
        estimatedDistance = distance,
        estimatedTotalFare = Nothing,
        merchantId = merchantId,
        merchantOperatingCityId,
        personId = riderId,
        status = oldStatus,
        bookingStatus,
        legExtraInfo = legExtraInfo,
        actualDistance = Nothing,
        totalFare = Nothing,
        entrance = Nothing,
        exit = Nothing,
        validTill = (mbQuote <&> (.validTill)) <|> (frfsSearch.validTill)
      }
  where
    mkLegExtraInfo mbQuote journeyLegInfo' = do
      case vehicleType of
        Spec.METRO -> do
          return $
            Metro $
              MetroLegExtraInfo
                { routeInfo = journeyLegInfo',
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
          journeyRouteDetail <- listToMaybe journeyLeg.routeDetails & fromMaybeM (InternalError "Journey Route Detail not found")
          let fromStation = journeyLegDetail.originStop
          let toStation = journeyLegDetail.destinationStop
          let routeCode' = journeyLegDetail.routeCode

          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Bus $
              BusLegExtraInfo
                { originStop = fromStation,
                  destinationStop = toStation,
                  routeCode = routeCode',
                  bookingId = Nothing,
                  tickets = Nothing,
                  ticketValidity = Nothing,
                  ticketsCreatedAt = Nothing,
                  providerName = Nothing,
                  selectedServiceTier = mbSelectedServiceTier,
                  alternateShortNames = journeyRouteDetail.alternateShortNames,
                  routeName = journeyRouteDetail.routeColorName,
                  frequency = journeyRouteDetail.frequency,
                  ticketNo = Nothing,
                  adultTicketQuantity = mbQuote <&> (.quantity),
                  childTicketQuantity = mbQuote >>= (.childTicketQuantity),
                  refund = Nothing,
                  trackingStatus = journeyLegDetail.trackingStatus,
                  fleetNo = journeyLeg.finalBoardedBusNumber,
                  discounts = mbQuote >>= (.discountsJson) >>= decodeFromText
                }
        Spec.SUBWAY -> do
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Subway $
              SubwayLegExtraInfo
                { routeInfo = journeyLegInfo',
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

mkJourney :: MonadFlow m => Id DP.Person -> Maybe UTCTime -> Maybe UTCTime -> Distance -> Seconds -> Id DJ.Journey -> Id DSR.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [EMInterface.MultiModalLeg] -> Meters -> Maybe (Id DRL.RecentLocation) -> Maybe Double -> Bool -> Bool -> Location -> Maybe Location -> m DJ.Journey
mkJourney riderId startTime endTime estimatedDistance estiamtedDuration journeyId parentSearchId merchantId merchantOperatingCityId legs maximumWalkDistance mbRecentLocationId relevanceScore hasUserPreferredServiceTier hasUserPreferredTransitModes fromLocation toLocation = do
  let journeyLegsCount = length legs
      modes = map (\x -> convertMultiModalModeToTripMode x.mode (straightLineDistance x) maximumWalkDistance) legs
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
        hasStartedTrackingWithoutBooking = Nothing,
        ..
      }
  where
    straightLineDistance leg = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)

mkJourneyLegGroupCode :: Id DSR.SearchRequest -> DTrip.MultimodalTravelMode -> Maybe EMInterface.MultiModalStopDetails -> Maybe EMInterface.MultiModalStopDetails -> Maybe Text
mkJourneyLegGroupCode multimodalSearchRequestId mode mbFromStopDetails mbToStopDetails =
  case mode of
    DTrip.Walk -> Nothing
    DTrip.Taxi -> Nothing
    _ -> Just $ multimodalSearchRequestId.getId <> "-" <> show mode <> "-" <> fromMaybe "" (mbFromStopDetails >>= (.stopCode)) <> "-" <> fromMaybe "" (mbToStopDetails >>= (.stopCode))

mkJourneyLeg ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasShortDurationRetryCfg r c
  ) =>
  Int ->
  (Maybe KEMIT.MultiModalLeg, KEMIT.MultiModalLeg, Maybe KEMIT.MultiModalLeg) ->
  Location ->
  Maybe Location ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DJ.Journey ->
  Id DSR.SearchRequest ->
  Meters ->
  Maybe GetFareResponse ->
  Maybe Gates ->
  m DJL.JourneyLeg
mkJourneyLeg idx (mbPrev, leg, mbNext) journeyStartLocation journeyEndLocation merchantId merchantOpCityId journeyId multimodalSearchRequestId maximumWalkDistance fare mbGates = do
  now <- getCurrentTime
  journeyLegId <- generateGUID
  routeDetails <- mapM (mkRouteDetail journeyLegId fare) leg.routeDetails
  let travelMode = convertMultiModalModeToTripMode leg.mode straightLineDistance maximumWalkDistance
  gates <- maybe (getGates (mbPrev, leg, mbNext) merchantId merchantOpCityId) (pure . Just) mbGates
  let (fromStopDetails, toStopDetails) =
        case travelMode of
          DTrip.Walk -> do
            let fromStopDetails' = mkStopDetails (gates >>= (.straightLineExit) >>= (.streetName)) (mbPrev >>= (.toStopDetails)) (Just journeyStartLocation.address)
            let toStopDetails' = mkStopDetails (gates >>= (.straightLineEntrance) >>= (.streetName)) (mbNext >>= (.fromStopDetails)) (journeyEndLocation <&> (.address))
            (fromStopDetails', toStopDetails')
          _ -> (leg.fromStopDetails, leg.toStopDetails)
  let groupCode = mkJourneyLegGroupCode multimodalSearchRequestId travelMode fromStopDetails toStopDetails
  return $
    DJL.JourneyLeg
      { agency = leg.agency,
        distance = Just leg.distance,
        duration = Just leg.duration,
        groupCode,
        endLocation = mkLocationWithGate (gates >>= (.straightLineEntrance)) leg.endLocation.latLng,
        fromArrivalTime = leg.fromArrivalTime,
        fromDepartureTime = leg.fromDepartureTime,
        fromStopDetails = fromStopDetails,
        id = journeyLegId,
        mode = travelMode,
        routeDetails,
        startLocation = mkLocationWithGate (gates >>= (.straightLineExit)) leg.startLocation.latLng,
        toArrivalTime = leg.toArrivalTime,
        toDepartureTime = leg.toDepartureTime,
        toStopDetails = toStopDetails,
        serviceTypes = fare >>= (.serviceTypes),
        estimatedMinFare = fare <&> (.estimatedMinFare),
        estimatedMaxFare = fare <&> (.estimatedMaxFare),
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        createdAt = now,
        updatedAt = now,
        legSearchId = Nothing,
        legPricingId = Nothing,
        changedBusesInSequence = Nothing,
        finalBoardedBusNumber = Nothing,
        osmEntrance = chooseGate (gates >>= (.osmEntrance)) (leg.entrance),
        osmExit = chooseGate (gates >>= (.osmExit)) (leg.exit),
        straightLineEntrance = chooseGate (gates >>= (.straightLineEntrance)) (leg.entrance),
        straightLineExit = chooseGate (gates >>= (.straightLineExit)) (leg.exit),
        journeyId = journeyId,
        isDeleted = Just False,
        sequenceNumber = idx,
        multimodalSearchRequestId = Just multimodalSearchRequestId.getId
      }
  where
    straightLineDistance = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)

    mkLocationWithGate :: Maybe KEMIT.MultiModalLegGate -> Maps.LatLngV2 -> Maps.LatLngV2
    mkLocationWithGate mGate fallbackLoc =
      case (mGate >>= (.lat), mGate >>= (.lon)) of
        (Just lat, Just lon) -> Maps.LatLngV2 lat lon
        _ -> fallbackLoc

    mkStopDetails :: Maybe Text -> Maybe EMInterface.MultiModalStopDetails -> Maybe LocationAddress -> Maybe EMInterface.MultiModalStopDetails
    mkStopDetails mbGateName (Just stopDetails) _ =
      Just $
        EMInterface.MultiModalStopDetails
          { stopCode = Nothing,
            platformCode = Nothing,
            name = case (mbGateName, stopDetails.name) of
              (Just gName, Just sName) -> Just $ gName <> ", " <> sName
              (Just gName, Nothing) -> Just gName
              (Nothing, Just sName) -> Just sName
              (Nothing, Nothing) -> Nothing,
            gtfsId = Nothing
          }
    mkStopDetails _ _ (Just parentAddress) =
      Just $
        EMInterface.MultiModalStopDetails
          { stopCode = Nothing,
            platformCode = Nothing,
            name = case (parentAddress.title, parentAddress.area) of
              (Just title, Just area) -> Just $ title <> ", " <> area
              (Just title, Nothing) -> Just title
              (Nothing, Just area) -> Just area
              (Nothing, Nothing) -> Nothing,
            gtfsId = Nothing
          }
    mkStopDetails _ _ _ = Nothing

    mkRouteDetail :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id DJL.JourneyLeg -> Maybe GetFareResponse -> EMInterface.MultiModalRouteDetails -> m RouteDetails
    mkRouteDetail journeyLegId fare' routeDetail = do
      now <- getCurrentTime
      newId <- generateGUID
      let fromStopDetails' = fromMaybe (EMInterface.MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.fromStopDetails)
          toStopDetails' = fromMaybe (EMInterface.MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.toStopDetails)
      let tierRoutes = maybe [] (concatMap (.availableRoutes)) (fare' >>= (.possibleRoutes))
      let alternateShortNames = if null tierRoutes then routeDetail.alternateShortNames else nub tierRoutes
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
            alternateShortNames = alternateShortNames,
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
            createdAt = now,
            updatedAt = now
          }

    chooseGate :: Maybe KEMIT.MultiModalLegGate -> Maybe KEMIT.MultiModalLegGate -> Maybe KEMIT.MultiModalLegGate
    chooseGate fromGates fromLeg = fromGates <|> fromLeg

getServiceTypeFromProviderCode :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Text -> m Spec.ServiceTierType
getServiceTypeFromProviderCode merchantOperatingCityId providerCode = do
  serviceTiers <- QFRFSVehicleServiceTier.findByProviderCode providerCode merchantOperatingCityId
  return $ fromMaybe Spec.ORDINARY (listToMaybe serviceTiers <&> (._type))

sumHighPrecMoney :: [HighPrecMoney] -> HighPrecMoney
sumHighPrecMoney = HighPrecMoney . sum . map getHighPrecMoney

allCompletedStatus :: [JourneyLegStatus]
allCompletedStatus = [Completed, Cancelled, Skipped]

data ExtendLegStartPoint
  = StartLocation StartLocationType
  | StartLegOrder Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StartLocationType = StartLocationType
  { location :: LocationAPIEntity,
    legOrder :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Gates = Gates
  { straightLineEntrance :: Maybe KEMIT.MultiModalLegGate,
    straightLineExit :: Maybe KEMIT.MultiModalLegGate,
    osmEntrance :: Maybe KEMIT.MultiModalLegGate,
    osmExit :: Maybe KEMIT.MultiModalLegGate
  }

getGates ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
  (Maybe KEMIT.MultiModalLeg, KEMIT.MultiModalLeg, Maybe KEMIT.MultiModalLeg) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe Gates)
getGates (mbPrev, currentLeg, mbNext) merchantId merchantOpCityId = do
  mbEntrances <- maybe (pure Nothing) (fetchStationGatesFromLeg True) mbNext
  mbExits <- maybe (pure Nothing) (fetchStationGatesFromLeg False) mbPrev
  case (mbEntrances, mbExits) of
    (Just entrances, Just exits) -> do
      (osmEntrance, straightLineEntrance) <- getEntranceGates entrances
      (osmExit, straightLineExit) <- getExitGates exits
      return $ Just $ Gates {straightLineEntrance = straightLineEntrance, straightLineExit = straightLineExit, osmEntrance = osmEntrance, osmExit = osmExit}
    (Just entrances, Nothing) -> do
      (osmEntrance, straightLineEntrance) <- getEntranceGates entrances
      return $ Just $ Gates {straightLineEntrance = straightLineEntrance, straightLineExit = Nothing, osmEntrance = osmEntrance, osmExit = Nothing}
    (Nothing, Just exits) -> do
      (osmExit, straightLineExit) <- getExitGates exits
      return $ Just $ Gates {straightLineEntrance = Nothing, straightLineExit = straightLineExit, osmEntrance = Nothing, osmExit = osmExit}
    (Nothing, Nothing) -> return Nothing
  where
    getEntranceGates entrances =
      getNearestGateFromLeg (LatLong currentLeg.startLocation.latLng.latitude currentLeg.startLocation.latLng.longitude) merchantId merchantOpCityId entrances
    getExitGates exits =
      getNearestGateFromLeg (LatLong currentLeg.endLocation.latLng.latitude currentLeg.endLocation.latLng.longitude) merchantId merchantOpCityId exits

    fetchStationGatesFromLeg :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) => Bool -> KEMIT.MultiModalLeg -> m (Maybe [DStation.Gate])
    fetchStationGatesFromLeg isEntrance leg =
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

getNearestGateFromLeg ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) =>
  LatLong ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  [DStation.Gate] ->
  m (Maybe KEMIT.MultiModalLegGate, Maybe KEMIT.MultiModalLegGate)
getNearestGateFromLeg point merchantId merchantOpCityId gates = do
  osmGate <- getNearestOSMGate
  let straightLineGate = minimumByMay (compareDist point) gates
  return (transformGate osmGate, transformGate straightLineGate)
  where
    compareDist p g1 g2 =
      compare
        (distanceBetweenInMeters p (LatLong g1.lat g1.lon))
        (distanceBetweenInMeters p (LatLong g2.lat g2.lon))

    getNearestOSMGate :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasShortDurationRetryCfg r c) => m (Maybe DStation.Gate)
    getNearestOSMGate =
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
          distances <- lift $ Maps.getMultimodalJourneyDistances merchantId merchantOpCityId Nothing req
          nearest <- hoistMaybe $ minimumByMay (\r1 r2 -> compare r1.distance r2.distance) (toList distances)
          pure (nearest.destination)

    transformGate :: Maybe DStation.Gate -> Maybe KEMIT.MultiModalLegGate
    transformGate domainGate =
      domainGate <&> \gate ->
        KEMIT.MultiModalLegGate
          { distance = Nothing,
            lon = Just gate.lon,
            lat = Just gate.lat,
            isEntrance = Nothing,
            absoluteDirection = Nothing,
            streetName = Just (Text.pack gate.gateName),
            exit = Nothing,
            stayOn = Nothing,
            area = Nothing,
            bogusName = Nothing,
            walkingBike = Nothing
          }

safeTail :: [a] -> Maybe a
safeTail [] = Nothing
safeTail [_] = Nothing
safeTail xs = Just (last xs)
