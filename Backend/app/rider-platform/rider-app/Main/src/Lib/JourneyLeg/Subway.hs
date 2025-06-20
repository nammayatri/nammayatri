{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Subway where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.Trip as DTrip
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import Lib.JourneyLeg.Types.Subway
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg SubwayLegRequest m where
  search (SubwayLegRequestSearch SubwayLegRequestSearchData {..}) = CFRFS.search Spec.SUBWAY personId merchantId quantity city journeyLeg recentLocationId
  search _ = throwError (InternalError "Not supported")

  confirm (SubwayLegRequestConfirm SubwayLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId searchId quoteId quantity childTicketQuantity skipBooking bookingAllowed crisSdkResponse Spec.SUBWAY
  confirm _ = throwError (InternalError "Not supported")

  update (SubwayLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (SubwayLegRequestCancel legData) = CFRFS.cancel legData.searchId legData.cancellationType legData.isSkipped
  cancel _ = throwError (InternalError "Not supported")

  isCancellable ((SubwayLegRequestIsCancellable _legData)) = return $ JT.IsCancellableResponse {canCancel = False}
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (SubwayLegRequestGetState req) = CFRFS.getState DTrip.Subway req.searchId req.riderLastPoints req.isLastCompleted False Nothing
  getState _ = throwError (InternalError "Not supported")

  getInfo (SubwayLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare req.distance req.duration req.journeyLeg.entrance req.journeyLeg.exit
  getInfo _ = throwError (InternalError "Not supported")

  getFare (SubwayLegRequestGetFare SubwayLegRequestGetFareData {..}) = CFRFS.getFare riderId merchant merchantOpCity Spec.SUBWAY routeDetails fromArrivalTime
  getFare _ = throwError (InternalError "Not supported")
