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
  search (SubwayLegRequestSearch SubwayLegRequestSearchData {..}) = CFRFS.search Spec.SUBWAY personId merchantId quantity city journeyLeg recentLocationId multimodalSearchRequestId upsertJourneyLegAction
  search _ = throwError (InternalError "Not supported")

  confirm (SubwayLegRequestConfirm SubwayLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId quoteId bookLater bookingAllowed crisSdkResponse Spec.SUBWAY categorySelectionReq isSingleMode mbEnableOffer
  confirm _ = throwError (InternalError "Not supported")

  update (SubwayLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (SubwayLegRequestCancel legData) = CFRFS.cancel legData.searchId legData.cancellationType
  cancel _ = throwError (InternalError "Not supported")

  getState (SubwayLegRequestGetState req) = CFRFS.getState DTrip.Subway req.searchId req.riderLastPoints False Nothing req.journeyLeg
  getState _ = throwError (InternalError "Not supported")

  getInfo (SubwayLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.journeyLeg req.journeyLegs
  getInfo _ = throwError (InternalError "Not supported")

  getFare (SubwayLegRequestGetFare SubwayLegRequestGetFareData {..}) = CFRFS.getFare riderId merchant merchantOpCity Spec.SUBWAY Nothing routeDetails fromArrivalTime agencyGtfsId searchReqId
  getFare _ = throwError (InternalError "Not supported")
