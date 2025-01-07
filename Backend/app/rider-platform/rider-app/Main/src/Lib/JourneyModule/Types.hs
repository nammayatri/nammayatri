module Lib.JourneyModule.Types where

import API.Types.RiderPlatform.Management.FRFSTicket
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.FRFSSearch as FRFSSR
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import qualified Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface as EMInterface
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Types
import Lib.JourneyModule.Utils
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Search
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.Station as QStation
import qualified Storage.Queries.Transformers.Booking as QTB
import Tools.Metrics.BAPMetrics.Types
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
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  )

type ConfirmFlow m r c =
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EventStreamFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasCoreMetrics r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    Redis.HedisFlow m r
  )

type GetFareFlow m r =
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  )

type SearchJourneyLeg leg m = leg -> m ()

type GetFareJourneyLeg leg m = leg -> m (Maybe GetFareResponse)

type ConfirmJourneyLeg leg m = leg -> m ()

type CancelJourneyLeg leg m = leg -> m ()

type UpdateJourneyLeg leg m = leg -> m ()

type GetJourneyLegState leg m = leg -> m JourneyLegState

type GetJourneyLeg leg m = leg -> m LegInfo

class JourneyLeg leg m where
  search :: SearchRequestFlow m r c => SearchJourneyLeg leg m
  confirm :: ConfirmFlow m r c => ConfirmJourneyLeg leg m
  update :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => UpdateJourneyLeg leg m
  cancel :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => CancelJourneyLeg leg m
  getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => GetJourneyLegState leg m
  getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => GetJourneyLeg leg m
  getFare :: GetFareFlow m r => GetFareJourneyLeg leg m

data JourneyLegState = JourneyLegState
  { status :: JourneyLegStatus,
    currentPosition :: Maybe LatLong
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetFareResponse = GetFareResponse {estimatedMinFare :: HighPrecMoney, estimatedMaxFare :: HighPrecMoney}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegStatus
  = InPlan
  | -- | Booking
    -- | RetryBooking
    Assigning
  | -- | ReAssigning
    Booked
  | OnTime
  | AtRiskOfMissing
  | Departed
  | Missed
  | Delayed
  | Arriving
  | Skipped -- we might need this
  | Ongoing
  | Finishing
  | Cancelled
  | Completed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInitData = JourneyInitData
  { legs :: [EMInterface.MultiModalLeg],
    parentSearchId :: Id DSR.SearchRequest,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    estimatedDistance :: Distance,
    estimatedDuration :: Seconds,
    maximumWalkDistance :: Meters
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegInfo = LegInfo
  { skipBooking :: Bool,
    legId :: Maybe Text,
    travelMode :: DTrip.TravelMode,
    startTime :: UTCTime,
    order :: Int,
    status :: JourneyLegStatus,
    estimatedDuration :: Maybe Seconds,
    estimatedFare :: Maybe PriceAPIEntity,
    estimatedDistance :: Maybe Distance,
    legExtraInfo :: LegExtraInfo,
    merchantId :: Id DM.Merchant,
    personId :: Id DP.Person
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegExtraInfo = Walk WalkLegExtraInfo | Taxi TaxiLegExtraInfo | Metro MetroLegExtraInfo
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
    destination :: Location
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLegExtraInfo = MetroLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    lineColor :: Maybe Text,
    frequency :: Maybe Int -- make it Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateJourneyReq = UpdateJourneyReq
  { fare :: Maybe Price,
    legsDone :: Maybe Int,
    modes :: Maybe [DTrip.TravelMode],
    totalLegs :: Maybe Int,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

getTexiLegStatusFromBooking :: DBooking.Booking -> Maybe DRide.Ride -> JourneyLegStatus
getTexiLegStatusFromBooking booking mRide = do
  case mRide of
    Just ride -> mapTaxiRideStatusToJourneyLegStatus ride.status
    Nothing -> mapTaxiBookingStatusToJourneyLegStatus booking.status

getTexiLegStatusFromSearch :: JourneySearchData -> JourneyLegStatus
getTexiLegStatusFromSearch journeyLegInfo =
  if journeyLegInfo.skipBooking
    then Skipped
    else InPlan

mkLegInfoFromBookingAndRide :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DBooking.Booking -> Maybe DRide.Ride -> m LegInfo
mkLegInfoFromBookingAndRide booking mRide = do
  toLocation <- QTB.getToLocation booking.bookingDetails & fromMaybeM (InvalidRequest "To Location not found")
  return $
    LegInfo
      { skipBooking = False,
        legId = Just booking.id.getId,
        travelMode = DTrip.Taxi,
        startTime = booking.startTime,
        order = 0, -- booking.journeyLegOrder, FIX THIS @hkmangla
        estimatedDuration = booking.estimatedDuration,
        estimatedFare = Just $ mkPriceAPIEntity booking.estimatedFare,
        estimatedDistance = booking.estimatedDistance,
        merchantId = booking.merchantId,
        personId = booking.riderId,
        status = getTexiLegStatusFromBooking booking mRide,
        legExtraInfo =
          Taxi $
            TaxiLegExtraInfo
              { origin = booking.fromLocation,
                destination = toLocation
              }
      }

mkLegInfoFromSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DSR.SearchRequest -> m LegInfo
mkLegInfoFromSearchRequest DSR.SearchRequest {..} = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  mbEstimatedFare <-
    case journeyLegInfo'.pricingId of
      Just estId -> do
        mbEst <- QEstimate.findById (Id estId)
        return $ mkPriceAPIEntity <$> (mbEst <&> (.estimatedFare))
      Nothing -> return Nothing
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  return $
    LegInfo
      { skipBooking = journeyLegInfo'.skipBooking,
        legId = journeyLegInfo'.pricingId,
        travelMode = DTrip.Taxi,
        startTime = startTime,
        order = journeyLegInfo'.journeyLegOrder,
        estimatedDuration = estimatedRideDuration,
        estimatedFare = mbEstimatedFare,
        estimatedDistance = distance,
        merchantId = merchantId,
        personId = riderId,
        status = getTexiLegStatusFromSearch journeyLegInfo',
        legExtraInfo = Taxi $ TaxiLegExtraInfo {origin = fromLocation, destination = toLocation'}
      }

getWalkLegStatusFromWalkLeg :: DWalkLeg.WalkLegMultimodal -> JourneySearchData -> JourneyLegStatus
getWalkLegStatusFromWalkLeg legData journeyLegInfo = do
  if journeyLegInfo.skipBooking
    then Skipped
    else castWalkLegStatus legData.status
  where
    castWalkLegStatus :: DWalkLeg.WalkLegStatus -> JourneyLegStatus
    castWalkLegStatus DWalkLeg.InPlan = InPlan
    castWalkLegStatus DWalkLeg.Ongoing = Ongoing
    castWalkLegStatus DWalkLeg.Completed = Completed

mkWalkLegInfoFromWalkLegData :: MonadFlow m => DWalkLeg.WalkLegMultimodal -> m LegInfo
mkWalkLegInfoFromWalkLegData legData@DWalkLeg.WalkLegMultimodal {..} = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  return $
    LegInfo
      { skipBooking = journeyLegInfo'.skipBooking,
        legId = journeyLegInfo'.pricingId,
        travelMode = DTrip.Walk,
        startTime = startTime,
        order = journeyLegInfo'.journeyLegOrder,
        estimatedDuration = estimatedDuration,
        estimatedFare = Nothing,
        estimatedDistance = Just estimatedDistance,
        merchantId = merchantId,
        personId = riderId,
        status = getWalkLegStatusFromWalkLeg legData journeyLegInfo',
        legExtraInfo = Walk $ WalkLegExtraInfo {origin = fromLocation, destination = toLocation'}
      }

mkLegInfoFromFrfsSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => FRFSSR.FRFSSearch -> m LegInfo
mkLegInfoFromFrfsSearchRequest FRFSSR.FRFSSearch {..} = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  now <- getCurrentTime
  mbEstimatedFare <-
    case journeyLegInfo'.pricingId of
      Just quoteId -> do
        mbQuote <- QFRFSQuote.findById (Id quoteId)
        return $ mkPriceAPIEntity <$> (mbQuote <&> (.price))
      Nothing -> return Nothing
  fromStation <- QStation.findById fromStationId >>= fromMaybeM (InternalError "From Station not found")
  toStation <- QStation.findById toStationId >>= fromMaybeM (InternalError "To Station not found")
  return $
    LegInfo
      { skipBooking = journeyLegInfo'.skipBooking,
        legId = journeyLegInfo'.pricingId,
        travelMode = DTrip.Metro,
        startTime = now,
        order = journeyLegInfo'.journeyLegOrder,
        estimatedDuration = Nothing, -- check with hemant if we can store estimatedDuration in frfsSearch table  --journeyLeg.duration,
        estimatedFare = mbEstimatedFare,
        estimatedDistance = Nothing, -- check with hemant if we can store estimatedDistance in frfsSearch table --journeyLeg.distance,
        merchantId = merchantId,
        personId = riderId,
        status = InPlan,
        legExtraInfo =
          Metro $
            MetroLegExtraInfo
              { originStop = stationToStationAPI fromStation,
                destinationStop = stationToStationAPI toStation,
                lineColor = lineColor,
                frequency = frequency
              }
      }
  where
    stationToStationAPI station =
      FRFSStationAPI
        { name = station.name,
          code = station.code,
          lat = station.lat,
          lon = station.lon,
          address = station.address
        }

mkSearchReqLocation :: LocationAddress -> Maps.LatLngV2 -> SearchReqLocation
mkSearchReqLocation address latLng = do
  SearchReqLocation
    { gps = LatLong {lat = latLng.latitude, lon = latLng.longitude},
      address = address
    }

mkJourney :: MonadFlow m => Distance -> Seconds -> Id DJ.Journey -> Id DSR.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [GetFareResponse] -> [EMInterface.MultiModalLeg] -> Meters -> m DJ.Journey
mkJourney estimatedDistance estiamtedDuration journeyId parentSearchId merchantId merchantOperatingCityId totalFares legs maximumWalkDistance = do
  let journeyLegsCount = length legs
      modes = map (\x -> convertMultiModalModeToTripMode x.mode (distanceToMeters x.distance) maximumWalkDistance) legs
  now <- getCurrentTime
  return $
    DJ.Journey
      { convenienceCost = 0,
        estimatedDistance = estimatedDistance,
        estimatedDuration = Just estiamtedDuration,
        estimatedFare = Nothing,
        fare = Nothing,
        id = journeyId,
        legsDone = 0,
        totalLegs = journeyLegsCount,
        modes = modes,
        searchRequestId = parentSearchId,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        estimatedMinFare = Just $ sumHighPrecMoney $ totalFares <&> (.estimatedMinFare),
        estimatedMaxFare = Just $ sumHighPrecMoney $ totalFares <&> (.estimatedMaxFare)
      }

mkJourneyLeg :: MonadFlow m => Int -> EMInterface.MultiModalLeg -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DJ.Journey -> Meters -> m DJL.JourneyLeg
mkJourneyLeg idx leg merchantId merchantOpCityId journeyId maximumWalkDistance = do
  now <- getCurrentTime
  journeyLegId <- generateGUID
  return $
    DJL.JourneyLeg
      { agency = leg.agency,
        distance = leg.distance,
        duration = leg.duration,
        endLocation = leg.endLocation.latLng,
        fromArrivalTime = leg.fromArrivalTime,
        fromDepartureTime = leg.fromDepartureTime,
        fromStopDetails = leg.fromStopDetails,
        id = journeyLegId,
        journeyId,
        mode = convertMultiModalModeToTripMode leg.mode (distanceToMeters leg.distance) maximumWalkDistance,
        -- polylinePoints = leg.polyline.encodedPolyline,
        routeDetails = leg.routeDetails,
        sequenceNumber = idx,
        startLocation = leg.startLocation.latLng,
        toArrivalTime = leg.toArrivalTime,
        toDepartureTime = leg.toDepartureTime,
        toStopDetails = leg.toStopDetails,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOpCityId,
        createdAt = now,
        updatedAt = now,
        legId = Nothing
      }

sumHighPrecMoney :: [HighPrecMoney] -> HighPrecMoney
sumHighPrecMoney = HighPrecMoney . sum . map getHighPrecMoney

completedStatus :: [JourneyLegStatus]
completedStatus = [Completed, Cancelled]
