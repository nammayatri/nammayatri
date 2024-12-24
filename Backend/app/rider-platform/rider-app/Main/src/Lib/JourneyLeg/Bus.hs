module Lib.JourneyLeg.Bus where

import qualified Lib.JourneyModule.Types as JT

data BusLegRequestSearchData

data BusLegRequestConfirmData

data BusLegRequestUpdateData

data BusLegRequestCancelData

data BusLegRequestGetInfoData

data BusLegRequestGetStateData

data BusLegRequestGetFareData = BusLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }

data BusLegRequest
  = BusLegRequestSearch BusLegRequestSearchData
  | BusLegRequestConfirm BusLegRequestConfirmData
  | BusLegRequestUpdate BusLegRequestUpdateData
  | BusLegRequestCancel BusLegRequestCancelData
  | BusLegRequestGetFare BusLegRequestGetFareData
  | BusLegRequestGetState BusLegRequestGetStateData
  | BusLegRequestGetInfo BusLegRequestGetInfoData

instance JourneyLeg BusLeg m where
  search (BusLegRequestSearch _) = do
    -- FRFSSearch.create multimodalLeg busLegSearchData
    -- void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.BUS frfsSearchReq
    return ()

  confirm (BusLegRequestConfirm _) = do
    -- frfsSearchReq <- QFRFSSearch.findById taxiLegConfirmData.searchReqId
    -- quoteId <- searchReq.journeySearchData.pricingId
    -- case busLegConfirmData.skippedBooking of
    --   True ->
    --      -- subtract price from total estimate
    --     QFRFSSearch.updateIsSkipped True
    --   False ->
    --     void $ FRFSTicketService.postFrfsQuoteConfirm (personId, merchantId) (Id quoteId)
    return ()

  update (BusLegRequestUpdate _) = do
    -- let customerLocation = get eta between user location and bus station
    -- let busLocation = get eta between bus and bus station
    -- let threshold = 50
    -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
    -- mark status with respect to user -  AtRiskOfMissing, Missed
    return ()

  cancel (BusLegRequestCancel _) = return ()

  getState (BusLegRequestGetState _) = return ()

  getInfo (BusLegRequestGetInfo _) = return ()

  getFare (BusLegRequestGetFare _) = do
    return JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 20}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 20}}
