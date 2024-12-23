module Lib.JourneyModule.Types where

import Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SearchRequest as DSR
import Kernel.External.MultiModal.Interface
import Kernel.Types.Id
import Kernel.Utils.Common

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
  }

mkLegInfoFromSearchRequest :: SR.SearchRequest -> m LegInfo
mkLegInfoFromSearchRequest SR.SearchRequest {..} = do
  return $
    LegInfo
      { skipBooking = journeyLegInfo.skipBooking
      , legId = journeyLegInfo.pricingId
      , travelMode = Trip.Taxi
      , startTime = startTime
      , order = journeyLegInfo.journeyLegOrder
      }

mkLegInfoFromFrfsSearchRequest :: FRFSSR.FRFSSearch -> m LegInfo
mkLegInfoFromFrfsSearchRequest FRFSSR.FRFSSearch {..} = do
  now <- getCurrentTime
  return $
    LegInfo
      { skipBooking = journeyLegInfo.skipBooking
      , legId = journeyLegInfo.pricingId
      , travelMode = Trip.Metro
      , startTime = now
      , order = journeyLegInfo.journeyLegOrder
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