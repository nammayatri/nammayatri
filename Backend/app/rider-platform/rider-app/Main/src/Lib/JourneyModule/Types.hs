module Lib.JourneyModule.Types where

import Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SearchRequest as DSR
import Kernel.External.MultiModal.Interface
import Kernel.Types.Id
import Kernel.Utils.Common
import API.Types.RiderPlatform.Management.FRFSTicket

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