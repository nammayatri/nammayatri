{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Metro where

import Kernel.Prelude
import Lib.JourneyLeg.Types.Metro
import qualified Lib.JourneyModule.Types as JT
import qualified Domain.Types.TicketBookingService as TBS
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSSearch as QFRFSSearch

mapServiceStatusToJourneyLegStatus :: TBS.ServiceStatus -> JT.JourneyLegStatus
mapServiceStatusToJourneyLegStatus status = case status of
  TBS.Pending -> JT.InPlan -- Indicates the service is yet to start planning.
  TBS.Failed -> JT.Cancelled -- Indicates a failure in the service.
  TBS.Confirmed -> JT.Booked -- Indicates the service has been confirmed/booked.
  TBS.Verified -> JT.Assigning -- Indicates the service is verified and assigning resources.
  TBS.Cancelled -> JT.Cancelled -- Indicates the service has been cancelled.

instance JT.JourneyLeg MetroLegRequest m where
  search (MetroLegRequestSearch _) = return () -- TODO: Implement this
  search _ = throwError (InternalError "Not supported")

  confirm (MetroLegRequestConfirm _) = return ()
  confirm _ = throwError (InternalError "Not supported")

  update (MetroLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (MetroLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")

  getState (MetroLegRequestGetState _) = return $ JT.JourneyLegState { status = JT.InPlan, currentPosition = Nothing } 
  getState _ = throwError (InternalError "Not supported")

  getInfo (MetroLegRequestGetInfo req) = do
    -- TODO: No booking for metro, we can handle for bookings once booking is there
    searchReq <- QFRFSSearch.findById req.searchId >>= fromMaybeM (SearchRequestNotFound req.searchId.getId)
    JT.mkLegInfoFromFrfsSearchRequest searchReq
  getInfo _ = throwError (InternalError "Not supported")
  
  getFare (MetroLegRequestGetFare _) = do
    return JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}}
  getFare _ = throwError (InternalError "Not supported")