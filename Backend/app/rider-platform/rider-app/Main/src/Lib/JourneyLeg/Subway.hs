{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Subway where

import qualified BecknV2.FRFS.Enums as Spec
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import Lib.JourneyLeg.Types.Subway
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg SubwayLegRequest m where
  search (SubwayLegRequestSearch SubwayLegRequestSearchData {..}) = CFRFS.search Spec.METRO personId merchantId quantity city journeyLeg
  search _ = throwError (InternalError "Not supported")

  confirm (SubwayLegRequestConfirm SubwayLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId quoteId skipBooking bookingAllowed
  confirm _ = throwError (InternalError "Not supported")

  update (SubwayLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (SubwayLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")

  isCancellable ((SubwayLegRequestIsCancellable _legData)) = return $ JT.IsCancellableResponse {canCancel = False}
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (SubwayLegRequestGetState req) = CFRFS.getState req.searchId req.riderLastPoints req.isLastJustCompleted
  getState _ = throwError (InternalError "Not supported")

  getInfo (SubwayLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare
  getInfo _ = throwError (InternalError "Not supported")

  getFare (SubwayLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}})
  getFare _ = throwError (InternalError "Not supported")
