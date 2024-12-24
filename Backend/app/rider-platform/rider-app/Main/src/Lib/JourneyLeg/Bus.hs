module Lib.JourneyLeg.Bus where

import qualified Lib.JourneyLeg.Types as JLT


data BusLegUpdateData = BusLegUpdateData
  { id :: Id LegID,
    userLocation :: Location,
    busLocation :: Location,
    startTime :: Maybe UTCTime
  }

data BusLegRequest = BusLegRequestSearch MultiModalLeg BusSearchData | BusLegRequestConfirm BusConfirmData | BusLegRequestUpdate BusLegUpdateData | BusLegGetFareRequest BusGetFareData

data BusGetFareData = BusGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }

instance JourneyLeg BusLeg m where
  search (BusLegRequestSearch multimodalLeg busLegSearchData) = do
    FRFSSearch.create multimodalLeg busLegSearchData
    void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.BUS frfsSearchReq
  confirm (BusLegRequestConfirm busLegConfirmData) = do
    frfsSearchReq <- QFRFSSearch.findById taxiLegConfirmData.searchReqId
    quoteId <- searchReq.journeySearchData.pricingId
    case busLegConfirmData.skippedBooking of
      True ->
         -- subtract price from total estimate
        QFRFSSearch.updateIsSkipped True
      False ->
        void $ FRFSTicketService.postFrfsQuoteConfirm (personId, merchantId) (Id quoteId)
  update (BusLegRequest $ BusLegRequestUpdate busLegUpdateRequest) =
  -- let customerLocation = get eta between user location and bus station
  -- let busLocation = get eta between bus and bus station
  -- let threshold = 50
      -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
      -- mark status with respect to user -  AtRiskOfMissing, Missed
    return ()
  cancel (BusLeg _legData) = return ()
  getState (BusLeg _legData) = return ()
  get (BusLeg _legData) = return ()

  getFare (BusLegGetFareRequest _busGetFareData) =  do
    return JLT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 20}, estimatedMaxFare =  HighPrecMoney {getHighPrecMoney = 20}}
