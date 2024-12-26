module Lib.JourneyLeg.Metro where

import qualified Domain.Types.FRFSSearch as FRFSSearch
import qualified Lib.JourneyModule.Types as JT

mapServiceStatusToJourneyLegStatus :: ServiceStatus -> JourneyLegStatus
mapServiceStatusToJourneyLegStatus status = case status of
  Pending -> InPlan -- Indicates the service is yet to start planning.
  Failed -> FailedStatus -- Indicates a failure in the service.
  Confirmed -> Booked -- Indicates the service has been confirmed/booked.
  Verified -> Assigning -- Indicates the service is verified and assigning resources.
  Cancelled -> Cancelled -- Indicates the service has been cancelled.

data MetroLegRequestSearchData

data MetroLegRequestUpdateData

data MetroLegRequestConfirmData

data MetroLegRequestCancelData

data MetroLegRequestGetStateData

newtype MetroLegRequestGetInfoData = MetroLegRequestGetInfoData
  { searchId :: Id FRFSSearch.FRFSSearch
  }

data MetroLegCancelRequest

data MetroLegRequest
  = MetroLegRequestSearch MetroLegRequestSearchData
  | MetroLegRequestConfirm MetroLegRequestConfirmData
  | MetroLegRequestUpdate MetroLegRequestUpdateData
  | MetroLegRequestCancel MetroLegRequestCancelData
  | MetroLegRequestGetFare MetroLegRequestGetFareData
  | MetroLegRequestGetState MetroLegRequestGetStateData
  | MetroLegRequestGetInfo MetroLegRequestGetInfoData

data MetroLegRequestGetFareData = MetroLegRequestGetFareData
  { startLocation :: LatLngV2,
    endLocation :: LatLngV2
  }

instance JourneyLeg MetroLeg m where
  search (MetroLegRequestSearch _) = return () -- TODO: Implement this

  confirm (MetroLegRequestConfirm _) = return ()

  update (MetroLegRequestUpdate _) = return ()

  cancel (MetroLegRequestCancel _) = return ()

  getState (MetroLegRequestGetState _) = return ()

  getInfo (MetroLegRequestGetInfo req) = do
    -- TODO: No booking for metro, we can handle for bookings once booking is there
    searchReq <- QFRFSSearch.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
    return $ mkLegInfoFromFrfsSearchRequest searchReq

  getFare (MetroLegRequestGetFare _) = do
    return JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}}
