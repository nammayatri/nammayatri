{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Bus where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.Trip as DTrip
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import Lib.JourneyLeg.Types.Bus
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg BusLegRequest m where
  search (BusLegRequestSearch BusLegRequestSearchData {..}) = CFRFS.search Spec.BUS personId merchantId quantity city journeyLeg
  search _ = throwError (InternalError "Not supported")

  confirm (BusLegRequestConfirm BusLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId searchId quoteId skipBooking bookingAllowed
  confirm _ = throwError (InternalError "Not supported")

  update (BusLegRequestUpdate _) = do
    -- let customerLocation = get eta between user location and bus station
    -- let busLocation = get eta between bus and bus station
    -- let threshold = 50
    -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
    -- mark status with respect to user -  AtRiskOfMissing, Missed
    throwError (InternalError "Not supported")
  update _ = throwError (InternalError "Not supported")

  cancel (BusLegRequestCancel legData) = CFRFS.cancel legData.searchId legData.cancellationType legData.isSkipped
  cancel _ = throwError (InternalError "Not supported")

  isCancellable (BusLegRequestIsCancellable legData) = CFRFS.isCancellable legData.searchId
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (BusLegRequestGetState req) = CFRFS.getState DTrip.Bus req.searchId req.riderLastPoints req.isLastCompleted
  getState _ = throwError (InternalError "Not supported")

  getInfo (BusLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare req.distance req.duration
  getInfo _ = throwError (InternalError "Not supported")

  getFare (BusLegRequestGetFare BusLegRequestGetFareData {..}) = CFRFS.getFare merchant merchantOpCity Spec.BUS routeDetails
  getFare _ = throwError (InternalError "Not supported")
