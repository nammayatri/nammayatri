module Lib.JourneyLeg.Metro where

mapServiceStatusToJourneyLegStatus :: ServiceStatus -> JourneyLegStatus
mapServiceStatusToJourneyLegStatus status = case status of
  Pending -> InPlan -- Indicates the service is yet to start planning.
  Failed -> FailedStatus -- Indicates a failure in the service.
  Confirmed -> Booked -- Indicates the service has been confirmed/booked.
  Verified -> Assigning -- Indicates the service is verified and assigning resources.
  Cancelled -> Cancelled -- Indicates the service has been cancelled.

data MetroLegRequest = MetroLegRequestSearch MultiModalLeg MetroSearchData | MetroLegRequestConfirm MetroConfirmData

instance JourneyLeg MetroLeg m where
  search (MetroLegRequestSearch multimodalLeg metroLegSearchData) = do
    FRFSSearch.create multimodalLeg metroLegSearchData
    void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.METRO frfsSearchReq
  confirm (MetroLegRequestConfirm metroLegConfirmData) = do
    frfsSearchReq <- QFRFSSearch.findById taxiLegConfirmData.searchReqId
    quoteId <- searchReq.journeySearchData.pricingId
    case metroLegConfirmData.skippedBooking of
      True ->
        -- subtract price from total estimate
        QFRFSSearch.updateIsSkipped True
      False ->
        void $ FRFSTicketService.postFrfsQuoteConfirm (personId, merchantId) (Id quoteId)
  update (MetroLeg _legData) = return ()
  cancel (MetroLeg _legData) = return ()
  getState (MetroLeg _legData) = return InPlan
  get (MetroLeg _legData) = return _legData
