module Lib.JourneyLeg.Metro where

mapServiceStatusToJourneyLegStatus :: ServiceStatus -> JourneyLegStatus
mapServiceStatusToJourneyLegStatus status = case status of
  Pending -> InPlan -- Indicates the service is yet to start planning.
  Failed -> FailedStatus -- Indicates a failure in the service.
  Confirmed -> Booked -- Indicates the service has been confirmed/booked.
  Verified -> Assigning -- Indicates the service is verified and assigning resources.
  Cancelled -> Cancelled -- Indicates the service has been cancelled.

data MetroLegConfirmRequest

data MetroLegRequest
  = MetroLegRequestSearch MultiModalLeg MetroSearchData
  | MetroLegConfirm MetroLegConfirmRequest

instance JourneyLeg MetroLeg m where
  search (MetroLegRequestSearch multimodalLeg metroLegSearchData) = do
    FRFSSearch.create multimodalLeg metroLegSearchData
    void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.METRO frfsSearchReq
  confirm (MetroLegConfirm req) = return ()
  update (MetroLeg _legData) = return ()
  cancel (MetroLeg _legData) = return ()
  getState (MetroLeg _legData) = return InPlan
  get (MetroLeg _legData) = return _legData
