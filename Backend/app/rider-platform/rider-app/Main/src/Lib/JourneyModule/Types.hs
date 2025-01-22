module Lib.JourneyModule.Types where

import API.Types.RiderPlatform.Management.FRFSTicket
import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.FRFSSearch as FRFSSR
import qualified Domain.Types.FRFSTicketBooking as DFRFSBooking
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
import Kernel.External.Types (ServiceFlow)
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
import Lib.Payment.Storage.Beam.BeamFlow
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Search
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.RiderConfig as QRiderConfig
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
    Redis.HedisFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  )

type GetFareFlow m r =
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
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
  cancel :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => CancelJourneyLeg leg m
  isCancellable :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => IsCancellableJourneyLeg leg m
  getState :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => GetJourneyLegState leg m
  getInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => GetJourneyLeg leg m
  getFare :: GetFareFlow m r => GetFareJourneyLeg leg m

newtype SearchResponse = SearchResponse
  { id :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegState = JourneyLegState
  { status :: JourneyLegStatus,
    statusChanged :: Bool,
    currentPosition :: Maybe LatLong,
    legOrder :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetFareResponse = GetFareResponse {estimatedMinFare :: HighPrecMoney, estimatedMaxFare :: HighPrecMoney}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInitData = JourneyInitData
  { legs :: [EMInterface.MultiModalLeg],
    parentSearchId :: Id DSR.SearchRequest,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    estimatedDistance :: Distance,
    estimatedDuration :: Seconds,
    startTime :: Maybe UTCTime,
    endTime :: Maybe UTCTime,
    maximumWalkDistance :: Meters
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
    estimatedDistance :: Maybe Distance,
    legExtraInfo :: LegExtraInfo,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    personId :: Id DP.Person
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegExtraInfo = Walk WalkLegExtraInfo | Taxi TaxiLegExtraInfo | Metro MetroLegExtraInfo | Bus BusLegExtraInfo
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
    otp :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroLegExtraInfo = MetroLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    lineColor :: Maybe Text,
    lineColorCode :: Maybe Text,
    tickets :: Maybe [Text],
    providerName :: Maybe Text,
    frequency :: Maybe Int -- make it Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BusLegExtraInfo = BusLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    tickets :: Maybe [Text],
    routeName :: Maybe Text,
    providerName :: Maybe Text
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

data UnifiedTicketQR = UnifiedTicketQR
  { version :: Text,
    txnId :: Text,
    createdAt :: UTCTime,
    cmrl :: [BookingData],
    mtc :: [BookingData]
  }

data Provider = CMRL | MTC
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

getTexiLegStatusFromBooking :: DBooking.Booking -> Maybe DRide.Ride -> JourneyLegStatus
getTexiLegStatusFromBooking booking mRide = do
  case mRide of
    Just ride -> mapTaxiRideStatusToJourneyLegStatus ride.status
    Nothing -> mapTaxiBookingStatusToJourneyLegStatus booking.status

getTexiLegStatusFromSearch :: JourneySearchData -> Maybe DEstimate.EstimateStatus -> JourneyLegStatus
getTexiLegStatusFromSearch journeyLegInfo mbEstimateStatus =
  if journeyLegInfo.skipBooking
    then Skipped
    else case mbEstimateStatus of
      Nothing -> InPlan
      Just DEstimate.NEW -> InPlan
      Just DEstimate.COMPLETED -> Booked
      Just DEstimate.CANCELLED -> Cancelled
      _ -> Assigning

mkLegInfoFromBookingAndRide :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DBooking.Booking -> Maybe DRide.Ride -> m LegInfo
mkLegInfoFromBookingAndRide booking mRide = do
  toLocation <- QTB.getToLocation booking.bookingDetails & fromMaybeM (InvalidRequest "To Location not found")
  return $
    LegInfo
      { skipBooking = False,
        bookingAllowed = True,
        searchId = booking.transactionId,
        pricingId = Just booking.id.getId,
        travelMode = DTrip.Taxi,
        startTime = booking.startTime,
        order = 0, -- booking.journeyLegOrder, FIX THIS @hkmangla
        estimatedDuration = booking.estimatedDuration,
        estimatedMinFare = Just $ mkPriceAPIEntity booking.estimatedFare,
        estimatedMaxFare = Just $ mkPriceAPIEntity booking.estimatedFare,
        estimatedDistance = booking.estimatedDistance,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        personId = booking.riderId,
        status = getTexiLegStatusFromBooking booking mRide,
        legExtraInfo =
          Taxi $
            TaxiLegExtraInfo
              { origin = booking.fromLocation,
                destination = toLocation,
                driverName = mRide <&> (.driverName),
                vehicleNumber = mRide <&> (.vehicleNumber),
                otp = mRide <&> (.otp)
              }
      }

mkLegInfoFromSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DSR.SearchRequest -> m LegInfo
mkLegInfoFromSearchRequest DSR.SearchRequest {..} = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  (mbFareRange, mbEstimateStatus) <-
    case journeyLegInfo'.pricingId of
      Just estId -> do
        mbEst <- QEstimate.findById (Id estId)
        return $ (mbEst <&> (.totalFareRange), mbEst <&> (.status))
      Nothing -> return (Nothing, Nothing)
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
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
        estimatedMinFare = mkPriceAPIEntity <$> (mbFareRange <&> (.minFare)),
        estimatedMaxFare = mkPriceAPIEntity <$> (mbFareRange <&> (.maxFare)),
        estimatedDistance = distance,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        personId = riderId,
        status = getTexiLegStatusFromSearch journeyLegInfo' mbEstimateStatus,
        legExtraInfo =
          Taxi $
            TaxiLegExtraInfo
              { origin = fromLocation,
                destination = toLocation',
                driverName = Nothing,
                vehicleNumber = Nothing,
                otp = Nothing
              }
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
        estimatedDistance = Just estimatedDistance,
        merchantId = merchantId,
        merchantOperatingCityId,
        personId = riderId,
        status = getWalkLegStatusFromWalkLeg legData journeyLegInfo',
        legExtraInfo = Walk $ WalkLegExtraInfo {origin = fromLocation, destination = toLocation'}
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
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DFRFSBooking.FRFSTicketBooking -> m LegInfo
mkLegInfoFromFrfsBooking booking = do
  fromStation <- QStation.findById booking.fromStationId >>= fromMaybeM (InternalError "From Station not found")
  toStation <- QStation.findById booking.toStationId >>= fromMaybeM (InternalError "To Station not found")
  ticketsData <- QFRFSTicket.findAllByTicketBookingId (booking.id)
  let qrDataList = ticketsData <&> (.qrData)
  now <- getCurrentTime
  legOrder <- fromMaybeM (InternalError "Leg Order is Nothing") (booking.journeyLegOrder)
  let startTime = fromMaybe now booking.startTime
  let legStatus =
        case booking.journeyLegStatus of
          Nothing -> getFRFSLegStatusFromBooking booking
          Just InPlan -> getFRFSLegStatusFromBooking booking
          Just status -> status
  return $
    LegInfo
      { skipBooking = False,
        bookingAllowed = True,
        searchId = booking.searchId.getId,
        pricingId = Just booking.id.getId,
        travelMode = if booking.vehicleType == Spec.METRO then DTrip.Metro else DTrip.Bus,
        startTime = startTime,
        order = legOrder,
        estimatedDuration = Nothing, -------------- TODO : Should be changed
        estimatedMinFare = Just $ mkPriceAPIEntity booking.estimatedPrice,
        estimatedMaxFare = Just $ mkPriceAPIEntity booking.estimatedPrice,
        estimatedDistance = Nothing, --------------- TODO : Should be changed
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        personId = booking.riderId,
        status = legStatus,
        legExtraInfo = mkLegExtraInfo fromStation toStation qrDataList
      }
  where
    mkLegExtraInfo fromStation toStation qrDataList = do
      if booking.vehicleType == Spec.METRO
        then do
          Metro $
            MetroLegExtraInfo
              { originStop = stationToStationAPI fromStation,
                destinationStop = stationToStationAPI toStation,
                lineColor = booking.lineColor,
                lineColorCode = booking.lineColorCode,
                tickets = Just qrDataList,
                providerName = Just booking.providerName,
                frequency = booking.frequency
              }
        else do
          Bus $
            BusLegExtraInfo
              { originStop = stationToStationAPI fromStation,
                destinationStop = stationToStationAPI toStation,
                tickets = Just qrDataList,
                providerName = Nothing,
                routeName = booking.lineColor
              }

    stationToStationAPI station =
      FRFSStationAPI
        { name = station.name,
          code = station.code,
          lat = station.lat,
          lon = station.lon,
          address = station.address
        }

mkLegInfoFromFrfsSearchRequest :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => FRFSSR.FRFSSearch -> Maybe HighPrecMoney -> m LegInfo
mkLegInfoFromFrfsSearchRequest FRFSSR.FRFSSearch {..} fallbackFare = do
  journeyLegInfo' <- journeyLegInfo & fromMaybeM (InvalidRequest "Not a valid mulimodal search as no journeyLegInfo found")
  mRiderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId
  let bookingAllowed = fromMaybe False (mRiderConfig >>= (.metroBookingAllowed))
  now <- getCurrentTime
  mbEstimatedFare <-
    case journeyLegInfo'.pricingId of
      Just quoteId -> do
        mbQuote <- QFRFSQuote.findById (Id quoteId)
        return $ mkPriceAPIEntity <$> (mbQuote <&> (.price))
      Nothing -> do
        if bookingAllowed && not journeyLegInfo'.skipBooking
          then do return Nothing
          else return $ mkPriceAPIEntity <$> (mkPrice Nothing <$> fallbackFare)
  fromStation <- QStation.findById fromStationId >>= fromMaybeM (InternalError "From Station not found")
  toStation <- QStation.findById toStationId >>= fromMaybeM (InternalError "To Station not found")
  return $
    LegInfo
      { skipBooking = journeyLegInfo'.skipBooking,
        bookingAllowed,
        searchId = id.getId,
        pricingId = journeyLegInfo'.pricingId,
        travelMode = if vehicleType == Spec.METRO then DTrip.Metro else DTrip.Bus,
        startTime = now,
        order = journeyLegInfo'.journeyLegOrder,
        estimatedDuration = Nothing, -- check with hemant if we can store estimatedDuration in frfsSearch table  --journeyLeg.duration,
        estimatedMinFare = mbEstimatedFare,
        estimatedMaxFare = mbEstimatedFare,
        estimatedDistance = Nothing, -- check with hemant if we can store estimatedDistance in frfsSearch table --journeyLeg.distance,
        merchantId = merchantId,
        merchantOperatingCityId,
        personId = riderId,
        status = fromMaybe InPlan journeyLegStatus,
        legExtraInfo = mkLegExtraInfo fromStation toStation
      }
  where
    mkLegExtraInfo fromStation toStation = do
      if vehicleType == Spec.METRO
        then
          Metro $
            MetroLegExtraInfo
              { originStop = stationToStationAPI fromStation,
                destinationStop = stationToStationAPI toStation,
                lineColor = lineColor,
                lineColorCode = lineColorCode,
                tickets = Nothing,
                providerName = Nothing,
                frequency = frequency
              }
        else
          Bus $
            BusLegExtraInfo
              { originStop = stationToStationAPI fromStation,
                destinationStop = stationToStationAPI toStation,
                tickets = Nothing,
                providerName = Nothing,
                routeName = lineColor
              }

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

mkJourney :: MonadFlow m => Maybe UTCTime -> Maybe UTCTime -> Distance -> Seconds -> Id DJ.Journey -> Id DSR.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [EMInterface.MultiModalLeg] -> Meters -> m DJ.Journey
mkJourney startTime endTime estimatedDistance estiamtedDuration journeyId parentSearchId merchantId merchantOperatingCityId legs maximumWalkDistance = do
  let journeyLegsCount = length legs
      modes = map (\x -> convertMultiModalModeToTripMode x.mode (distanceToMeters x.distance) maximumWalkDistance) legs
  now <- getCurrentTime
  return $
    DJ.Journey
      { convenienceCost = 0,
        estimatedDistance = estimatedDistance,
        estimatedDuration = Just estiamtedDuration,
        id = journeyId,
        totalLegs = journeyLegsCount,
        modes = modes,
        searchRequestId = parentSearchId,
        merchantId = Just merchantId,
        startTime,
        endTime,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

mkJourneyLeg :: MonadFlow m => Int -> EMInterface.MultiModalLeg -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DJ.Journey -> Meters -> GetFareResponse -> m DJL.JourneyLeg
mkJourneyLeg idx leg merchantId merchantOpCityId journeyId maximumWalkDistance fare = do
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
        estimatedMinFare = Just fare.estimatedMinFare,
        estimatedMaxFare = Just fare.estimatedMaxFare,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOpCityId,
        createdAt = now,
        updatedAt = now,
        legSearchId = Nothing
      }

sumHighPrecMoney :: [HighPrecMoney] -> HighPrecMoney
sumHighPrecMoney = HighPrecMoney . sum . map getHighPrecMoney

completedStatus :: [JourneyLegStatus]
completedStatus = [Completed, Cancelled]
