module Lib.JourneyModule.Types where

import qualified Domain.Types.SearchRequest as SR
import qualified Domain.Types.FRFSSearch as FRFSSR
import qualified Domain.Types.Trip as Trip

data JourneyInitData leg = JourneyInitData
  { legs: [leg]
  , parentSearchId :: Id SearchRequest
  , merchantId :: Id Merchant
  , merchantOperatingCityId :: Id MerchantOperatingCity
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

mkConfirmReq :: LegInfo -> TaxiLegRequest
mkConfirmReq legInfo = 
  case legInfo.travelMode of
    Trip.Taxi -> mkTaxiLegConfirmReq legInfo
    _ -> MetroLegConfirm MetroLegConfirmRequest
    -- handle other cases
