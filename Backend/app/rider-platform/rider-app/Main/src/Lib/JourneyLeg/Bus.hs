module Lib.JourneyLeg.Bus where


data BusLegUpdateData = BusLegUpdateData
{ id : Id LegID
  userLocation : Location
  busLocation : Location
  startTime : Maybe UTCTime
}

data BusLegRequest = BusLegRequestSearch MultiModalLeg BusSearchData | BusLegRequestConfirm BusConfirmData | BusLegRequestUpdate BusLegUpdateData

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
  getState (BusLeg _legData) = return InPlan
  get (BusLeg _legData) = return _legData
