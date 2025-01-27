{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Bus where

import qualified BecknV2.FRFS.Enums as Spec
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import Lib.JourneyLeg.Types.Bus
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg BusLegRequest m where
  search (BusLegRequestSearch BusLegRequestSearchData {..}) = CFRFS.search Spec.BUS personId merchantId quantity city journeyLeg
  search _ = throwError (InternalError "Not supported")

  confirm (BusLegRequestConfirm BusLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId quoteId skipBooking bookingAllowed
  confirm _ = throwError (InternalError "Not supported")

  update (BusLegRequestUpdate _) = do
    -- let customerLocation = get eta between user location and bus station
    -- let busLocation = get eta between bus and bus station
    -- let threshold = 50
    -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
    -- mark status with respect to user -  AtRiskOfMissing, Missed
    throwError (InternalError "Not supported")
  update _ = throwError (InternalError "Not supported")

  cancel (BusLegRequestCancel _) = throwError (InternalError "Not supported")
  cancel _ = throwError (InternalError "Not supported")

  isCancellable ((BusLegRequestIsCancellable _legData)) = return $ JT.IsCancellableResponse {canCancel = False}
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (BusLegRequestGetState req) = CFRFS.getState req.searchId req.riderLastPoints req.isLastJustCompleted
  getState _ = throwError (InternalError "Not supported")

  getInfo (BusLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare
  getInfo _ = throwError (InternalError "Not supported")

  getFare (BusLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 20}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 20}})
  getFare _ = throwError (InternalError "Not supported")
