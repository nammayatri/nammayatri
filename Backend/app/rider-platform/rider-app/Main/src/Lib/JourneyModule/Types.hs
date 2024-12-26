module Lib.JourneyModule.Types where

import Kernel.Prelude
import Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SearchRequest as DSR
import Domain.Types.Ride as DRide
import Domain.Types.Booking as DBooking
import Kernel.External.MultiModal.Interface
import Kernel.Types.Id
import Kernel.Utils.Common
import API.Types.RiderPlatform.Management.FRFSTicket
import qualified Storage.Queries.Transformers.Booking as QTB
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg

type SearchJourneyLeg leg m = leg -> m ()
type GetFareJourneyLeg leg m =  leg -> m GetFareResponse
type ConfirmJourneyLeg leg m = leg -> m ()
type CancelJourneyLeg leg m = leg -> m ()
type UpdateJourneyLeg leg m = leg -> m ()
type GetJourneyLegState leg m = leg -> m JourneyLegState
type GetJourneyLeg leg m = leg -> m LegInfo

class JourneyLeg leg m where
  search :: SearchJourneyLeg leg m
  confirm :: ConfirmJourneyLeg leg m
  update :: UpdateJourneyLeg leg m
  cancel :: CancelJourneyLeg leg m
  getState :: GetJourneyLegState leg m
  getInfo :: GetJourneyLeg leg m
  getFare :: GetFareJourneyLeg leg m

data JourneyLegState = JourneyLegState
  { status :: JourneyLegStatus
  , currentPosition :: LatLong
  }

data GetFareResponse = GetFareResponse {estimatedMinFare :: HighPrecMoney, estimatedMaxFare :: HighPrecMoney}

data JourneyLegStatus =
    InPlan
  -- | Booking
  -- | RetryBooking
  | Assigning
  -- | ReAssigning
  | Booked
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
  deriving (Eq, Show)

data JourneyInitData = JourneyInitData
  { legs :: [MultiModalLeg],
    parentSearchId :: Id DSR.SearchRequest,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    estimatedDistance :: HighPrecDistance,
    estimatedDuration :: Seconds,
    maximumWalkDistance :: Meters
  }

data LegInfo = LegInfo
  { skipBooking :: Bool
  , legId :: Text
  , travelMode :: Trip.TravelMode
  , startTime :: UTCTime
  , order :: Int
  , status :: JourneyLegStatus
  , estimatedDuration :: Maybe Seconds
  , estimatedFare :: Maybe PriceAPIEntity
  , estimatedDistance :: Maybe Distance
  , legExtraInfo :: LegExtraInfo
  , merchantId: Id DM.Merchant,
  , personId: Id DP.Person
  }

data LegExtraInfo = Walk WalkLegExtraInfo | Taxi TaxiLegExtraInfo | Metro MetroLegExtraInfo

data WalkLegExtraInfo = WalkLegExtraInfo
  { origin :: Locaton
  , destination :: Location
  }

data TaxiLegExtraInfo = TaxiLegExtraInfo
  { origin :: Locaton
  , destination :: Location
  }

data MetroLegExtraInfo = MetroLegExtraInfo
  { originStop :: FRFSStationAPI,
    destinationStop :: FRFSStationAPI,
    lineColor :: Text,
    frequency :: Seconds
  }

data UpdateJourneyReq = UpdateJourneyReq
  { fare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    legsDone :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    modes :: Kernel.Prelude.Maybe [Domain.Types.Common.TravelMode],
    totalLegs :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }

mapTaxiRideStatusToJourneyLegStatus :: DRide.RideStatus -> JourneyLegStatus
mapTaxiRideStatusToJourneyLegStatus status = case status of
  DRide.UPCOMING    -> InPlan
  DRide.NEW         -> Booked
  DRide.INPROGRESS  -> Ongoing
  DRide.COMPLETED   -> Completed
  DRide.CANCELLED   -> Cancelled

mapTaxiBookingStatusToJourneyLegStatus :: DBooking..BookingStatus -> JourneyLegStatus
mapTaxiBookingStatusToJourneyLegStatus status = case status of
  NEW -> InPlan
  CONFIRMED -> InPlan
  AWAITING_REASSIGNMENT -> Assigning
  REALLOCATED ->Completed
  COMPLETED -> Completed
  CANCELLED -> Completed
  TRIP_ASSIGNED -> Booked

getTexiLegStatusFromBooking :: DBooking.Booking -> Maybe DRide.Ride -> JourneyLegStatus
getTexiLegStatusFromBooking booking mRide = do
  case mbRide of
    Just ride -> mapTaxiRideStatusToJourneyLegStatus ride.status
    Nothing -> mapTaxiBookingStatusToJourneyLegStatus booking.status

getTexiLegStatusFromSearch :: SR.SearchRequest -> JourneyLegStatus
getTexiLegStatusFromSearch searchReq =
  if searchReq.journeyLegInfo.skipBooking
    then Skipped
    else InPlan

mkLegInfoFromBookingAndRide :: DBooking.Booking -> Maybe DRide.Ride -> m LegInfo
mkLegInfoFromBookingAndRide booking mRide = do
  toLocation <- getToLocation bookingDetails
  return $
    LegInfo
      { skipBooking = False
      , legId = booking.id
      , travelMode = Trip.Taxi
      , startTime = booking.startTime
      , order = booking.journeyLegOrder
      , estimatedDuration = booking.estimatedDistance
      , estimatedFare = booking.estimatedFare
      , estimatedDistance = booking.estimatedDistance
      , merchantId = booking.merchantId
      , personId = booking.riderId
      , status = getTexiLegStatusFromBooking booking mRide
      , legExtraInfo =
          Taxi $ TaxiLegExtraInfo
            { origin = booking.fromLocation
            , destination = QTB.getToLocation booking.bookingDetails
            }
      }

mkLegInfoFromSearchRequest :: SR.SearchRequest -> m LegInfo
mkLegInfoFromSearchRequest searchReq@SR.SearchRequest {..} = do
  mbEstimatedFare <-
    case journeyLegInfo.pricingId of
      Just estId -> do
        mbEst <- QEstimate.findById estId
        return $ mkPriceAPIEntity <$> (mbEst <&> (.estimatedFare))
      Nothing -> return Nothing
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  return $
    LegInfo
      { skipBooking = journeyLegInfo.skipBooking
      , legId = journeyLegInfo.pricingId
      , travelMode = Trip.Taxi
      , startTime = startTime
      , order = journeyLegInfo.journeyLegOrder
      , estimatedDuration = estimatedRideDuration
      , estimatedFare = mbEstimatedFare
      , estimatedDistance = distance
      , merchantId = merchantId
      , personId = riderId
      , status = getTexiLegStatusFromSearch searchReq
      , legExtraInfo = Taxi $ TaxiLegExtraInfo { origin = fromLocation, destination = toLocation'}
      }


getWalkLegStatusFromWalkLeg :: DWalkLeg.WalkLegMultimodal -> JourneyLegStatus
getWalkLegStatusFromWalkLeg legData =
  if legData.isSkipped
    then Skipped
    else castWalkLegStatus legData.status
  where
    castWalkLegStatus :: DWalkLeg.WalkLegStatus -> JT.JourneyLegStatus
    castWalkLegStatus DWalkLeg.InPlan -> JT.InPlan
    castWalkLegStatus DWalkLeg.Ongoing -> JT.Ongoing
    castWalkLegStatus DWalkLeg.Completed -> JT.Completed

mkWalkLegInfoFromWalkLegData :: DWalkLeg.WalkLegMultimodal -> m LegInfo
mkWalkLegInfoFromWalkLegData DWalkLeg.WalkLegMultimodal {..} = do
  toLocation' <- toLocation & fromMaybeM (InvalidRequest "To location not found") -- make it proper
  return $
    LegInfo
      { skipBooking = journeyLegInfo.skipBooking
      , legId = journeyLegInfo.pricingId
      , travelMode = Trip.Walk
      , startTime = startTime
      , order = journeyLegInfo.journeyLegOrder
      , estimatedDuration = estimatedDuration
      , estimatedFare = Nothing
      , estimatedDistance = estimatedDistance
      , merchantId = merchantId
      , personId = riderId
      , status = getWalkLegStatusFromWalkLeg legData
      , legExtraInfo = Walk $ WalkLegExtraInfo { origin = fromLocation, destination = toLocation'}
      }

mkLegInfoFromFrfsSearchRequest :: FRFSSR.FRFSSearch -> m LegInfo
mkLegInfoFromFrfsSearchRequest FRFSSR.FRFSSearch {..} = do
  now <- getCurrentTime
  mbEstimatedFare <-
    case journeyLegInfo.pricingId of
      Just quoteId -> do
        mbQuote <- QFRFSQuote.findById quoteId
        return $ mkPriceAPIEntity <$> (mbQuote <&> (.price))
      Nothing -> return Nothing
  fromStation <- QStation.findById fromStationId >>= fromMaybeM (InternalError "From Station not found")
  toStation <- QStation.findById toStationId >>= fromMaybeM (InternalError "To Station not found")
  return $
    LegInfo
      { skipBooking = journeyLegInfo.skipBooking
      , legId = journeyLegInfo.pricingId
      , travelMode = Trip.Metro
      , startTime = now
      , order = journeyLegInfo.journeyLegOrder
      , estimatedDuration = Nothing -- Fix this @kavya
      , estimatedFare = mbEstimatedFare
      , estimatedDistance = Nothing -- Fix this @kavya
      , merchantId = merchantId
      , personId = riderId
      , status = InPlan
      , legExtraInfo = Metro $ MetroLegExtraInfo
          { originStop = stationToStationAPI fromStation,
            destinationStop = stationToStationAPI toStation,
            lineColor = "Green", -- fix this @kavya
            frequency = 300 -- fix this @kavya
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

mkTaxiLegConfirmReq :: LegInfo -> TaxiLegRequest
mkTaxiLegConfirmReq LegInfo {..} =
  TaxiLegRequestConfirm $ TaxiLegRequestConfirmData
    { skipBooking = skipBooking
    , estimateId = legId
    , personId
    , merchantId
    }

mkConfirmReq :: JourneyLeg a => LegInfo -> a
mkConfirmReq legInfo =
  case legInfo.travelMode of
    Trip.Taxi -> mkTaxiLegConfirmReq legInfo
    Trip.Metro -> MetroLegRequestConfirm MetroLegRequestConfirmData
    Trip.Bus -> BusLegRequestConfirm BusLegRequestConfirmData
    Trip.Walk -> WalkLegRequestConfirm WalkLegRequestConfirmData

mkTaxiSearchReq :: SearchRequest -> DJourenyLeg.JourneyLeg -> SearchReqLocation -> [SearchReqLocation] -> TaxiLegRequest
mkTaxiSearchReq parentSearchReq journeyLegData origin stops = TaxiLegRequestSearch $ TaxiLegRequestSearchData {..}

mkSearchReqLocation :: LocationAddress -> LatLngV2 -> SearchReqLocation
mkSearchReqLocation address latLng = do
  SearchReqLocation
    { gps = LatLong {lat = latLng.latitude, lon = latLng.longitude},
      address = address
    },

mkCancelReq :: JourneyLeg a => LegInfo -> a
mkCancelReq legInfo =
  case legInfo.travelMode of
      Trip.Taxi -> TaxiLegCancelRequest {searchId = legInfo.legId}
      _ -> MetroLegCancelRequest req {searchId = legInfo.legId}

mkJourney :: Distance -> Seconds -> Id DJ.Journey -> Id DSR.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> [GetFareResponse] -> [MultiModalLeg] -> Meters -> DJ.Journey
mkJourney estimatedDistance estiamtedDuration journeyId parentSearchId merchantId merchantOperatingCityId totalFares legs maximumWalkDistance = do
  let journeyLegsCount = length legs
      modes = map (\x -> Utils.convertMultiModalModeToTripMode x.mode (distanceToMeters x.distance) maximumWalkDistance) legs
  retrun $
    DJourney.Journey
      { convenienceCost = 0,
        estimatedDistance = estimatedDistance,
        estimatedDuration = Just estiamtedDuration,
        estimatedFare = Nothing,
        fare = Nothing,
        id = Id journeyId,
        legsDone = 0,
        totalLegs = journeyLegsCount,
        modes = modes,
        searchRequestId = parentSearchId,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        estimatedMinFare = sumHighPrecMoney $ totalFares <&> (.estimatedMinFare),
        estimatedMaxFare = sumHighPrecMoney $ totalFares <&> (.estimatedMaxFare)
      }

mkJourneyLeg :: MultiModalLeg -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DJ.Journey -> Meters -> DJL.JourneyLeg
mkJourneyLeg leg merchantId merchantOpCityId journeyId maximumWalkDistance = do
  now <- getCurrentTime
  return $
    DJL.JourneyLeg
      { agency = leg.agency,
        distance = leg.distance,
        duration = leg.duration,
        endLocation = leg.endLocation,
        fromArrivalTime = leg.fromArrivalTime,
        fromDepartureTime = leg.fromDepartureTime,
        fromStopDetails = leg.fromStopDetails,
        id = journeyLegId,
        journeyId,
        mode = convertMultiModalModeToTripMode leg.mode leg.distance maximumWalkDistance,
        -- polylinePoints = leg.polyline.encodedPolyline,
        routeDetails = leg.routeDetails,
        sequenceNumber = idx,
        startLocation = leg.startLocation.latLng,
        toArrivalTime = leg.toArrivalTime,
        toDepartureTime = leg.toDepartureTime,
        toStopDetails = leg.toStopDetails.latLng,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        legId = Nothing
      }

sumHighPrecMoney :: [HighPrecMoney] -> HighPrecMoney
sumHighPrecMoney = HighPrecMoney . sum . map getHighPrecMoney

mkGetFareReq :: JourneyLeg a -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTrip.TravelMode -> MultiModalLeg -> a
mkGetFareReq merchantId merchantOperatingCityId tripMode leg =
  case tripMode of
    DTrip.Taxi -> do
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
      merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
      TaxiLegRequestGetFare $ TaxiLegRequestGetFareData
        { startLocation = leg.startLocation,
          endLocation = leg.endLocation,
          distance = leg.distance,
          duration = leg.duration,
          merchant,
          merchantOpCity
        }
    Dtrip.Bus ->
      BusLegRequestGetFare $ BusLegRequestGetFareData
        { startLocation = leg.startLocation,
          endLocation = leg.endLocation
        }
    Dtrip.Metro ->
      MetroLegRequestGetFare $ MetroLegRequestGetFareData
        { startLocation = leg.startLocation,
          endLocation = leg.endLocation
        }
    Dtrip.Walk ->
      WalkLegRequestGetFare $ WalkLegRequestGetFareData
        { startLocation = leg.startLocation,
          endLocation = leg.endLocation
        }

completedStatus :: [JourneyLegStatus]
completedStatus = [Completed, Cancelled]