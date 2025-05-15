module Lib.JourneyModule.Types where

import API.Types.RiderPlatform.Management.FRFSTicket
import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Ride as DARide
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as FRFSSR
import qualified Domain.Types.FRFSTicketBooking as DFRFSBooking
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import Domain.Types.Location
import qualified Domain.Types.Location as DLocation
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RecentLocation as DRL
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.Station as DTS
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Environment
import EulerHS.Prelude (safeHead)
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface as EMInterface
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
import Kernel.Types.Registry
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.JourneyLeg.Types
import Lib.JourneyModule.Utils
import Lib.Payment.Storage.Beam.BeamFlow
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Booking (getfareBreakups)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Search
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.Station as QStation
import qualified Storage.Queries.Transformers.Booking as QTB
import Tools.Metrics.BAPMetrics.Types
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
    Registry m,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nyGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["ondcGatewayUrl" ::: BaseUrl],
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["collectRouteData" ::: Bool],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasField "hotSpotExpiry" r Seconds,
    HasField "ondcRegistryUrl" r BaseUrl,
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
    HasField "ltsHedisEnv" r Hedis.HedisEnv
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

type GetFareJourneyLeg leg m = leg -> m (Maybe GetFareResponse)

type ConfirmJourneyLeg leg m = leg -> m ()

type CancelJourneyLeg leg m = leg -> m ()

type IsCancellableJourneyLeg leg m = leg -> m IsCancellableResponse

type UpdateJourneyLeg leg m = leg -> m ()

type GetJourneyLegState leg m = leg -> m JourneyLegState

type GetJourneyLeg leg m = leg -> m LegInfo

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
    travelDistance :: Maybe Meters
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehiclePosition = VehiclePosition
  { position :: LatLong,
    vehicleId :: Text,
    nextStop :: Maybe NextStopDetails
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegStateData = JourneyLegStateData
  { status :: JourneyLegStatus,
    statusChanged :: Bool,
    userPosition :: Maybe LatLong,
    vehiclePositions :: [VehiclePosition],
    subLegOrder :: Int,
    legOrder :: Int,
    mode :: DTrip.MultimodalTravelMode
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
  { skipBooking :: Bool,
    bookingAllowed :: Bool,
    pricingId :: Maybe Text,
    searchId :: Text,
    travelMode :: DTrip.MultimodalTravelMode,
    startTime :: UTCTime,
    order :: Int,
    status :: JourneyLegStatus,
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
    totalFare :: Maybe PriceAPIEntity
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegExtraInfo = Walk WalkLegExtraInfo | Taxi TaxiLegExtraInfo | Metro MetroLegExtraInfo | Bus BusLegExtraInfo | Subway SubwayLegExtraInfo
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WalkLegExtraInfo = WalkLegExtraInfo
  { origin :: Location,
    destination :: Location
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
    exoPhoneNumber :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLegExtraInfo = MetroLegExtraInfo
  { routeInfo :: [MetroLegRouteInfo],
    tickets :: Maybe [Text],
    providerName :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLegRouteInfo = MetroLegRouteInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    routeCode :: Text,
    subOrder :: Maybe Int,
    platformNumber :: Maybe Text,
    lineColor :: Maybe Text,
    lineColorCode :: Maybe Text,
    journeyStatus :: Maybe JourneyLegStatus,
    frequency :: Maybe Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubwayLegExtraInfo = SubwayLegExtraInfo
  { routeInfo :: [SubwayLegRouteInfo],
    tickets :: Maybe [Text],
    providerName :: Maybe Text,
    sdkToken :: Maybe Text,
    providerRouteId :: Maybe Text,
    deviceId :: Maybe Text,
    ticketTypeCode :: Maybe Text,
    selectedServiceTier :: Maybe LegServiceTier
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubwayLegRouteInfo = SubwayLegRouteInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    routeCode :: Text,
    subOrder :: Maybe Int,
    platformNumber :: Maybe Text,
    lineColor :: Maybe Text,
    lineColorCode :: Maybe Text,
    trainNumber :: Maybe Text,
    journeyStatus :: Maybe JourneyLegStatus,
    frequency :: Maybe Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BusLegExtraInfo = BusLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    routeCode :: Text,
    tickets :: Maybe [Text],
    routeName :: Maybe Text,
    providerName :: Maybe Text,
    selectedServiceTier :: Maybe LegServiceTier,
    frequency :: Maybe Seconds,
    alternateShortNames :: [Text]
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
  { ticketData :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UnifiedTicketQR = UnifiedTicketQR
  { version :: Text,
    cmrl :: [BookingData],
    mtc :: [BookingData],
    cris :: [BookingData]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Provider = CMRL | MTC | DIRECT | CRIS
  deriving (Eq, Show)

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

getTaxiLegStatusFromBooking :: GetStateFlow m r c => DBooking.Booking -> Maybe DRide.Ride -> m (JourneyLegStatus, Maybe LatLong)
getTaxiLegStatusFromBooking booking mRide = do
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

getTaxiLegStatusFromSearch :: JourneySearchData -> Maybe DEstimate.EstimateStatus -> JourneyLegStatus
getTaxiLegStatusFromSearch journeyLegInfo mbEstimateStatus =
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

mkLegInfoFromBookingAndRide :: GetStateFlow m r c => DBooking.Booking -> Maybe DRide.Ride -> m LegInfo
mkLegInfoFromBookingAndRide booking mRide = do
  toLocation <- QTB.getToLocation booking.bookingDetails & fromMaybeM (InvalidRequest "To Location not found")
  let skipBooking = fromMaybe False booking.isSkipped
  (status, _) <- getTaxiLegStatusFromBooking booking mRide
  (fareBreakups, estimatedFareBreakups) <- getfareBreakups booking mRide
  tollDifference <- getTollDifference fareBreakups estimatedFareBreakups
  batchConfig <- SharedRedisKeys.getBatchConfig booking.transactionId
  return $
    LegInfo
      { skipBooking,
        bookingAllowed = True,
        searchId = booking.transactionId,
        pricingId = Just booking.id.getId,
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
        totalFare = mkPriceAPIEntity <$> (mRide >>= (.totalFare))
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

mkLegInfoFromSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DSR.SearchRequest -> m LegInfo
mkLegInfoFromSearchRequest DSR.SearchRequest {..} = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  (mbFareRange, mbEstimateStatus, mbEstimate) <-
    case journeyLegInfo'.pricingId of
      Just estId -> do
        mbEst <- QEstimate.findById (Id estId)
        return $ (mbEst <&> (.totalFareRange), mbEst <&> (.status), mbEst)
      Nothing -> return (Nothing, Nothing, Nothing)
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  batchConfig <- SharedRedisKeys.getBatchConfig id.getId
  return $
    LegInfo
      { skipBooking = journeyLegInfo'.skipBooking,
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
        status = getTaxiLegStatusFromSearch journeyLegInfo' mbEstimateStatus,
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
                vehicleIconUrl = Nothing,
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
        totalFare = Nothing
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

mkWalkLegInfoFromWalkLegData :: MonadFlow m => DWalkLeg.WalkLegMultimodal -> m LegInfo
mkWalkLegInfoFromWalkLegData legData@DWalkLeg.WalkLegMultimodal {..} = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal walk search as no journeyLegInfo found")
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  return $
    LegInfo
      { skipBooking = journeyLegInfo'.skipBooking,
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
        legExtraInfo = Walk $ WalkLegExtraInfo {origin = fromLocation, destination = toLocation'},
        actualDistance = estimatedDistance,
        totalFare = Nothing
      }

getFRFSLegStatusFromBooking :: DFRFSBooking.FRFSTicketBooking -> JourneyLegStatus
getFRFSLegStatusFromBooking booking = case booking.status of
  DFRFSBooking.NEW -> Assigning
  DFRFSBooking.APPROVED -> Booked
  DFRFSBooking.PAYMENT_PENDING -> InPlan
  DFRFSBooking.CONFIRMING -> Assigning
  DFRFSBooking.FAILED -> InPlan
  DFRFSBooking.CONFIRMED -> Booked
  DFRFSBooking.CANCELLED -> InPlan
  DFRFSBooking.COUNTER_CANCELLED -> InPlan
  DFRFSBooking.CANCEL_INITIATED -> InPlan
  DFRFSBooking.TECHNICAL_CANCEL_REJECTED -> InPlan

mkLegInfoFromFrfsBooking ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DFRFSBooking.FRFSTicketBooking -> Maybe Distance -> Maybe Seconds -> m LegInfo
mkLegInfoFromFrfsBooking booking distance duration = do
  let journeyRouteDetails' = booking.journeyRouteDetails
  ticketsData <- QFRFSTicket.findAllByTicketBookingId (booking.id)
  let qrDataList = ticketsData <&> (.qrData)

  metroRouteInfo' <- getMetroLegRouteInfo journeyRouteDetails'
  subwayRouteInfo' <- getSubwayLegRouteInfo journeyRouteDetails'

  now <- getCurrentTime
  legOrder <- fromMaybeM (InternalError "Leg Order is Nothing") (booking.journeyLegOrder)
  let startTime = fromMaybe now booking.startTime
  let legStatus =
        case booking.journeyLegStatus of
          Nothing -> getFRFSLegStatusFromBooking booking
          Just InPlan -> getFRFSLegStatusFromBooking booking
          Just status -> status
  let skipBooking = fromMaybe False booking.isSkipped
  legExtraInfo <- mkLegExtraInfo qrDataList journeyRouteDetails' metroRouteInfo' subwayRouteInfo'
  return $
    LegInfo
      { skipBooking,
        bookingAllowed = True,
        searchId = booking.searchId.getId,
        pricingId = Just booking.id.getId,
        travelMode = castCategoryToMode booking.vehicleType,
        startTime = startTime,
        order = legOrder,
        estimatedDuration = duration,
        estimatedMinFare = Just $ mkPriceAPIEntity booking.estimatedPrice,
        estimatedChildFare = Nothing,
        estimatedMaxFare = Just $ mkPriceAPIEntity booking.estimatedPrice,
        estimatedTotalFare = Nothing,
        estimatedDistance = distance,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        personId = booking.riderId,
        status = legStatus,
        legExtraInfo = legExtraInfo,
        actualDistance = distance,
        totalFare = mkPriceAPIEntity <$> booking.finalPrice
      }
  where
    mkLegExtraInfo qrDataList journeyRouteDetails' metroRouteInfo' subwayRouteInfo' = do
      case booking.vehicleType of
        Spec.METRO -> do
          return $
            Metro $
              MetroLegExtraInfo
                { routeInfo = metroRouteInfo',
                  tickets = Just qrDataList,
                  providerName = Just booking.providerName
                }
        Spec.BUS -> do
          journeyRouteDetail <- listToMaybe journeyRouteDetails' & fromMaybeM (InternalError "Journey Route Detail not found")

          fromStationId' <- fromMaybeM (InternalError "FromStationId is missing") journeyRouteDetail.fromStationId
          toStationId' <- fromMaybeM (InternalError "ToStationId is missing") journeyRouteDetail.toStationId
          routeId' <- fromMaybeM (InternalError "Route is missing") journeyRouteDetail.routeId

          fromStation <- QStation.findById fromStationId' >>= fromMaybeM (InternalError "From Station not found")
          toStation <- QStation.findById toStationId' >>= fromMaybeM (InternalError "To Station not found")
          route <- QRoute.findByRouteId routeId' >>= fromMaybeM (InternalError "Route not found")
          mbQuote <- QFRFSQuote.findById booking.quoteId
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Bus $
              BusLegExtraInfo
                { originStop = stationToStationAPI fromStation,
                  destinationStop = stationToStationAPI toStation,
                  routeCode = route.code,
                  tickets = Just qrDataList,
                  providerName = Just booking.providerName,
                  routeName = listToMaybe $ catMaybes $ map (.lineColor) journeyRouteDetails',
                  frequency = listToMaybe $ catMaybes $ map (.frequency) journeyRouteDetails',
                  alternateShortNames = journeyRouteDetail.alternateShortNames,
                  selectedServiceTier = mbSelectedServiceTier
                }
        Spec.SUBWAY -> do
          mbQuote <- QFRFSQuote.findById booking.quoteId
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          mbPerson <- QPerson.findById booking.riderId
          imeiNumber <- decrypt `mapM` (mbPerson >>= (.imeiNumber))
          return $
            Subway $
              SubwayLegExtraInfo
                { routeInfo = subwayRouteInfo',
                  tickets = Just qrDataList,
                  providerName = Just booking.providerName,
                  sdkToken = mbQuote >>= (.fareDetails) <&> (.sdkToken), -- required for show cris ticket
                  deviceId = imeiNumber, -- required for show cris ticket
                  providerRouteId = mbQuote >>= (.fareDetails) <&> (.providerRouteId), -- not required for show cris ticket but still sending for future use
                  ticketTypeCode = mbQuote >>= (.fareDetails) <&> (.ticketTypeCode), -- not required for show cris ticket but still sending for future use
                  selectedServiceTier = mbSelectedServiceTier
                }

getMetroLegRouteInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => [MultiModalJourneyRouteDetails] -> m [MetroLegRouteInfo]
getMetroLegRouteInfo journeyRouteDetails = do
  mapM transformJourneyRouteDetails journeyRouteDetails
  where
    transformJourneyRouteDetails :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => MultiModalJourneyRouteDetails -> m MetroLegRouteInfo
    transformJourneyRouteDetails journeyRouteDetail = do
      fromStationId' <- fromMaybeM (InternalError "FromStationId is missing") (journeyRouteDetail.fromStationId)
      toStationId' <- fromMaybeM (InternalError "ToStationId is missing") (journeyRouteDetail.toStationId)
      routeId' <- fromMaybeM (InternalError "Route is missing") (journeyRouteDetail.routeId)

      fromStation <- QStation.findById fromStationId' >>= fromMaybeM (InternalError "From Station not found")
      toStation <- QStation.findById toStationId' >>= fromMaybeM (InternalError "To Station not found")
      route <- QRoute.findByRouteId routeId' >>= fromMaybeM (InternalError "Route not found")

      return
        MetroLegRouteInfo
          { originStop = stationToStationAPI fromStation,
            destinationStop = stationToStationAPI toStation,
            routeCode = route.code,
            subOrder = journeyRouteDetail.subLegOrder,
            journeyStatus = journeyRouteDetail.journeyStatus,
            platformNumber = journeyRouteDetail.platformNumber,
            lineColor = journeyRouteDetail.lineColor,
            lineColorCode = journeyRouteDetail.lineColorCode,
            frequency = journeyRouteDetail.frequency
          }

getSubwayLegRouteInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => [MultiModalJourneyRouteDetails] -> m [SubwayLegRouteInfo]
getSubwayLegRouteInfo journeyRouteDetails = do
  mapM transformJourneyRouteDetails journeyRouteDetails
  where
    transformJourneyRouteDetails :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => MultiModalJourneyRouteDetails -> m SubwayLegRouteInfo
    transformJourneyRouteDetails journeyRouteDetail = do
      fromStationId' <- fromMaybeM (InternalError "FromStationId is missing") (journeyRouteDetail.fromStationId)
      toStationId' <- fromMaybeM (InternalError "ToStationId is missing") (journeyRouteDetail.toStationId)
      routeId' <- fromMaybeM (InternalError "Route is missing") (journeyRouteDetail.routeId)

      fromStation <- QStation.findById fromStationId' >>= fromMaybeM (InternalError "From Station not found")
      toStation <- QStation.findById toStationId' >>= fromMaybeM (InternalError "To Station not found")
      route <- QRoute.findByRouteId routeId' >>= fromMaybeM (InternalError "Route not found")

      return
        SubwayLegRouteInfo
          { originStop = stationToStationAPI fromStation,
            destinationStop = stationToStationAPI toStation,
            routeCode = route.code,
            subOrder = journeyRouteDetail.subLegOrder,
            platformNumber = journeyRouteDetail.platformNumber,
            journeyStatus = journeyRouteDetail.journeyStatus,
            lineColor = journeyRouteDetail.lineColor,
            lineColorCode = journeyRouteDetail.lineColorCode,
            trainNumber = Just route.shortName,
            frequency = journeyRouteDetail.frequency
          }

castCategoryToMode :: Spec.VehicleCategory -> DTrip.MultimodalTravelMode
castCategoryToMode Spec.METRO = DTrip.Metro
castCategoryToMode Spec.SUBWAY = DTrip.Subway
castCategoryToMode Spec.BUS = DTrip.Bus

mkLegInfoFromFrfsSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => FRFSSR.FRFSSearch -> Maybe HighPrecMoney -> Maybe Distance -> Maybe Seconds -> m LegInfo
mkLegInfoFromFrfsSearchRequest FRFSSR.FRFSSearch {..} fallbackFare distance duration = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  mRiderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing
  let bookingAllowed =
        case vehicleType of
          Spec.METRO -> fromMaybe False (mRiderConfig >>= (.metroBookingAllowed))
          Spec.SUBWAY -> fromMaybe False (mRiderConfig >>= (.suburbanBookingAllowed))
          _ -> True
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

  metroRouteInfo' <- getMetroLegRouteInfo journeyRouteDetails
  subwayRouteInfo' <- getSubwayLegRouteInfo journeyRouteDetails

  legExtraInfo <- mkLegExtraInfo mbQuote metroRouteInfo' subwayRouteInfo'

  return $
    LegInfo
      { skipBooking = journeyLegInfo'.skipBooking,
        bookingAllowed,
        searchId = id.getId,
        pricingId = journeyLegInfo'.pricingId,
        travelMode = castCategoryToMode vehicleType,
        startTime = now,
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
        totalFare = Nothing
      }
  where
    mkLegExtraInfo mbQuote metroRouteInfo' subwayRouteInfo' = do
      case vehicleType of
        Spec.METRO -> do
          return $
            Metro $
              MetroLegExtraInfo
                { routeInfo = metroRouteInfo',
                  tickets = Nothing,
                  providerName = Nothing
                }
        Spec.BUS -> do
          journeyRouteDetail <- listToMaybe journeyRouteDetails & fromMaybeM (InternalError "Journey Route Detail not found")
          fromStationId' <- fromMaybeM (InternalError "FromStationId is missing") (journeyRouteDetail.fromStationId)
          toStationId' <- fromMaybeM (InternalError "ToStationId is missing") (journeyRouteDetail.toStationId)
          routeId' <- fromMaybeM (InternalError "Route is missing") (journeyRouteDetail.routeId)

          fromStation <- QStation.findById fromStationId' >>= fromMaybeM (InternalError "From Station not found")
          toStation <- QStation.findById toStationId' >>= fromMaybeM (InternalError "To Station not found")
          route <- QRoute.findByRouteId routeId' >>= fromMaybeM (InternalError "Route not found")
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Bus $
              BusLegExtraInfo
                { originStop = stationToStationAPI fromStation,
                  destinationStop = stationToStationAPI toStation,
                  routeCode = route.code,
                  tickets = Nothing,
                  providerName = Nothing,
                  selectedServiceTier = mbSelectedServiceTier,
                  alternateShortNames = journeyRouteDetail.alternateShortNames,
                  routeName = listToMaybe $ catMaybes $ map (.lineColor) journeyRouteDetails,
                  frequency = listToMaybe $ catMaybes $ map (.frequency) journeyRouteDetails
                }
        Spec.SUBWAY -> do
          let mbSelectedServiceTier = getServiceTierFromQuote =<< mbQuote
          return $
            Subway $
              SubwayLegExtraInfo
                { routeInfo = subwayRouteInfo',
                  tickets = Nothing,
                  providerName = Nothing,
                  sdkToken = mbQuote >>= (.fareDetails) <&> (.sdkToken), -- required for cris sdk initiation
                  deviceId = Nothing, -- not required for cris sdk initiation
                  providerRouteId = mbQuote >>= (.fareDetails) <&> (.providerRouteId), -- required for cris sdk initiation
                  ticketTypeCode = mbQuote >>= (.fareDetails) <&> (.ticketTypeCode), -- required for cris sdk initiation
                  selectedServiceTier = mbSelectedServiceTier
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
      hindiName = station.hindiName
    }

mkSearchReqLocation :: LocationAddress -> Maps.LatLngV2 -> SearchReqLocation
mkSearchReqLocation address latLng = do
  SearchReqLocation
    { gps = LatLong {lat = latLng.latitude, lon = latLng.longitude},
      address = address
    }

mkJourney :: MonadFlow m => Id DP.Person -> Maybe UTCTime -> Maybe UTCTime -> Distance -> Seconds -> Id DJ.Journey -> Id DSR.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [EMInterface.MultiModalLeg] -> Meters -> Meters -> Maybe (Id DRL.RecentLocation) -> Maybe Double -> Bool -> Bool -> Text -> Maybe Text -> m DJ.Journey
mkJourney riderId startTime endTime estimatedDistance estiamtedDuration journeyId parentSearchId merchantId merchantOperatingCityId legs maximumWalkDistance straightLineThreshold mbRecentLocationId relevanceScore hasUserPreferredServiceTier hasUserPreferredTransitModes fromLocationAddress toLocationAddress = do
  let journeyLegsCount = length legs
      modes = map (\x -> convertMultiModalModeToTripMode x.mode (straightLineDistance x) (distanceToMeters x.distance) maximumWalkDistance straightLineThreshold) legs
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
        searchRequestId = parentSearchId,
        merchantId = Just merchantId,
        status = DJ.NEW,
        riderId,
        startTime,
        endTime,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        DJ.recentLocationId = mbRecentLocationId, -- Fully qualify the field name
        relevanceScore,
        hasPreferredServiceTier = Just hasUserPreferredServiceTier,
        hasPreferredTransitModes = Just hasUserPreferredTransitModes,
        fromLocationAddress = Just fromLocationAddress,
        ..
      }
  where
    straightLineDistance leg = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)

mkJourneyLeg :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Int -> EMInterface.MultiModalLeg -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DJ.Journey -> Meters -> Meters -> Maybe GetFareResponse -> m DJL.JourneyLeg
mkJourneyLeg idx leg merchantId merchantOpCityId journeyId maximumWalkDistance straightLineThreshold fare = do
  now <- getCurrentTime
  journeyLegId <- generateGUID
  return $
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
        routeDetails = leg.routeDetails,
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
        isSkipped = Just False
      }
  where
    straightLineDistance = highPrecMetersToMeters $ distanceBetweenInMeters (LatLong leg.startLocation.latLng.latitude leg.startLocation.latLng.longitude) (LatLong leg.endLocation.latLng.latitude leg.endLocation.latLng.longitude)

getServiceTypeFromProviderCode :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Text -> m Spec.ServiceTierType
getServiceTypeFromProviderCode merchantOperatingCityId providerCode = do
  serviceTiers <- QFRFSVehicleServiceTier.findByProviderCode providerCode merchantOperatingCityId
  return $ fromMaybe Spec.ORDINARY (listToMaybe serviceTiers <&> (._type))

sumHighPrecMoney :: [HighPrecMoney] -> HighPrecMoney
sumHighPrecMoney = HighPrecMoney . sum . map getHighPrecMoney

completedStatus :: [JourneyLegStatus]
completedStatus = [Completed, Cancelled]

cannotCancelStatus :: [JourneyLegStatus]
cannotCancelStatus = [Skipped, Ongoing, Finishing, Completed, Cancelled]

cannotCancelWalkStatus :: [JourneyLegStatus]
cannotCancelWalkStatus = [Skipped, Completed, Cancelled]

cannotSwitchStatus :: [JourneyLegStatus]
cannotSwitchStatus = [Skipped, Booked, OnTheWay, Arriving, Arrived, Ongoing, Finishing, Completed, Cancelled]

cannotCancelExtendStatus :: [JourneyLegStatus]
cannotCancelExtendStatus = [Skipped, Ongoing, Finishing, Completed, Cancelled, Booked, OnTheWay, Arriving, Arrived]

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
