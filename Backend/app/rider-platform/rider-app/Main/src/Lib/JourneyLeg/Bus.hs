{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Bus where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.Trip as DTrip
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import Lib.JourneyLeg.Types.Bus
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg BusLegRequest m where
  search (BusLegRequestSearch BusLegRequestSearchData {..}) = CFRFS.search Spec.BUS personId merchantId quantity city journeyLeg recentLocationId multimodalSearchRequestId serviceTier upsertJourneyLegAction blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode
  search _ = throwError (InternalError "Not supported")

  confirm (BusLegRequestConfirm BusLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId quoteId bookLater bookingAllowed Nothing Spec.BUS categorySelectionReq isSingleMode mbEnableOffer mbIsMockPayment
  confirm _ = throwError (InternalError "Not supported")

  update (BusLegRequestUpdate _) = do
    -- let customerLocation = get eta between user location and bus station
    -- let busLocation = get eta between bus and bus station
    -- let threshold = 50
    -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
    -- mark status with respect to user -  AtRiskOfMissing, Missed
    throwError (InternalError "Not supported")
  update _ = throwError (InternalError "Not supported")

  cancel (BusLegRequestCancel legData) = CFRFS.cancel legData.searchId legData.cancellationType
  cancel _ = throwError (InternalError "Not supported")

  getState (BusLegRequestGetState req) = CFRFS.getState DTrip.Bus req.searchId req.riderLastPoints req.movementDetected req.routeCodeForDetailedTracking req.journeyLeg
  getState _ = throwError (InternalError "Not supported")

  getInfo (BusLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.journeyLeg req.journeyLegs
  getInfo _ = throwError (InternalError "Not supported")

  getFare (BusLegRequestGetFare BusLegRequestGetFareData {..}) = CFRFS.getFare riderId merchant merchantOpCity Spec.BUS serviceType routeDetails fromArrivalTime agencyGtfsId Nothing blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode
  getFare _ = throwError (InternalError "Not supported")
