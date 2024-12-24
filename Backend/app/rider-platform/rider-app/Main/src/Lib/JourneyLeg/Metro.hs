module Lib.JourneyLeg.Metro where
  
import qualified Lib.JourneyLeg.Types as JLT

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
  | MetroLegGetState (Id Frfs.FRFSSearch)
  | MetroLegGetFareRequest MetroGetFareData

data MetroGetFareData = MetroGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }
instance JourneyLeg MetroLeg m where
  search (MetroLegRequestSearch multimodalLeg metroLegSearchData) = do
    FRFSSearch.create multimodalLeg metroLegSearchData
    void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.METRO frfsSearchReq
  confirm (MetroLegConfirm req) = return ()
  update (MetroLeg _legData) = return ()
  cancel (MetroLeg _legData) = return ()
  getState (MetroLeg _legData) = return InPlan
  
  get (MetroLegGetState srId) = do
    -- TODO: No booking for metro, we can handle for bookings once booking is there
    searchReq <- QFRFSSearch.findById srId
    return $ mkLegInfoFromFrfsSearchRequest searchReq

  getFare (MetroLegGetFareRequest _metroGetFareData) =  do
    return JLT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare =  HighPrecMoney {getHighPrecMoney = 10}}
