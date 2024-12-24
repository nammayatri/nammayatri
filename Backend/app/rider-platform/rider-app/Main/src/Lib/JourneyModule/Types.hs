module Lib.JourneyModule.Types where

import Domain.Types.Merchant as DM
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
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.JourneyLeg as DJL
import qualified Lib.JourneyLeg.Types as JLT

data JourneyInitData = JourneyInitData
  { legs :: [MultiModalLeg],
    parentSearchId :: Id DSR.SearchRequest,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    estimatedDistance :: HighPrecDistance,
    estimatedDuration :: Seconds
    maximumWalkDistance :: Meters
  }

data LegInfo = LegInfo
  { skipBooking :: Bool
  , legId :: Text
  , travelMode :: Trip.TravelMode
  , startTime :: UTCTime
  , order :: Int
  , estimatedDuration :: Maybe Seconds
  , estimatedFare :: Maybe PriceAPIEntity
  , estimatedDistance :: Maybe Distance
  , legExtraInfo :: LegExtraInfo
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

mkLegInfoFromBookingAndRide :: DBooking.Booking -> Maybe DRide.Ride -> m LegInfo
mkLegInfoFromBookingAndRide booking _mRide = do
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
      , legExtraInfo = 
          Taxi $ TaxiLegExtraInfo 
            { origin = booking.fromLocation
            , destination = QTB.getToLocation booking.bookingDetails
            }
      }
  
mkLegInfoFromSearchRequest :: SR.SearchRequest -> m LegInfo
mkLegInfoFromSearchRequest SR.SearchRequest {..} = do
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
      , legExtraInfo = Taxi $ TaxiLegExtraInfo { origin = fromLocation, destination = toLocation'}
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
  TaxiLegConfirm $ TaxiLegConfirmRequest
    { skipBooking = skipBooking
    , estimateId = legId
    }

mkConfirmReq :: JourneyLeg a => LegInfo -> a
mkConfirmReq legInfo =
  case legInfo.travelMode of
    Trip.Taxi -> mkTaxiLegConfirmReq legInfo
    _ -> MetroLegConfirm MetroLegConfirmRequest
    -- handle other cases

mkTaxiSearchReq :: SearchRequest -> DJourenyLeg.JourneyLeg -> TaxiSearchRequestData -> TaxiLegRequest
mkTaxiSearchReq sr jl tsrd = TaxiLegRequestSearch sr jl tsrd

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


-- mkUpdateReq :: LegInfo -> TaxiLegRequest
-- mkUpdateReq legInfo =
--   case legInfo.travelMode of
--       Trip.Taxi -> TaxiLegUpdateRequest req
--       _ -> MetroLegUpdateRequest req
-- -- handle other cases


mkJourney :: Distance -> Seconds -> Id DJ.Journey -> Id DSR.SearchRequest -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> JLT.GetFareResponse -> [MultiModalLeg] -> Meters -> DJ.Journey
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
        estimatedMinFare = sumHighPrecMoney $ totalFares.estimatedMinFare,
        estimatedMaxFare = sumHighPrecMoney $ totalFares.estimatedMaxFare
      }

mkJourneyLeg :: MultiModalLeg -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DJ.Journey -> DJL.JourneyLeg
mkJounreyleg leg merchantId merchantOpCityId journeyId = do
  currentTime <- getCurrentTime
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
        mode = leg.mode,
        polylinePoints = leg.polyline.encodedPolyline,
        routeDetails = leg.routeDetails,
        sequenceNumber = idx,
        startLocation = leg.startLocation.latLng,
        toArrivalTime = leg.toArrivalTime,
        toDepartureTime = leg.toDepartureTime,
        toStopDetails = leg.toStopDetails.latLng,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = currentTime,
        updatedAt = currentTime,
        legId = Nothing
      }

sumHighPrecMoney :: [HighPrecMoney] -> HighPrecMoney
sumHighPrecMoney = HighPrecMoney . sum . map getHighPrecMoney