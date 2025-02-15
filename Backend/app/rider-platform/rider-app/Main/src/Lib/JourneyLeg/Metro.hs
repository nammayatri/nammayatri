{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Metro where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.Trip as DTrip
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import Lib.JourneyLeg.Types.Metro
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg MetroLegRequest m where
  search (MetroLegRequestSearch MetroLegRequestSearchData {..}) = CFRFS.search Spec.METRO personId merchantId quantity city journeyLeg
  search _ = throwError (InternalError "Not supported")

  confirm (MetroLegRequestConfirm MetroLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId searchId quoteId skipBooking bookingAllowed
  confirm _ = throwError (InternalError "Not supported")

  update (MetroLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (MetroLegRequestCancel legData) = CFRFS.cancel legData.searchId legData.cancellationType legData.isSkipped
  cancel _ = throwError (InternalError "Not supported")

  isCancellable (MetroLegRequestIsCancellable legData) = CFRFS.isCancellable legData.searchId
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (MetroLegRequestGetState req) = CFRFS.getState DTrip.Metro req.searchId req.riderLastPoints req.isLastCompleted
  getState _ = throwError (InternalError "Not supported")

  getInfo (MetroLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare req.distance req.duration
  getInfo _ = throwError (InternalError "Not supported")

  getFare (MetroLegRequestGetFare MetroLegRequestGetFareData {..}) = CFRFS.getFare merchant merchantOpCity Spec.METRO routeDetails
  getFare _ = throwError (InternalError "Not supported")
