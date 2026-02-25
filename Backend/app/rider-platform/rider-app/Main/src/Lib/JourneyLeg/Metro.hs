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
  search (MetroLegRequestSearch MetroLegRequestSearchData {..}) = CFRFS.search Spec.METRO personId merchantId quantity city journeyLeg recentLocationId multimodalSearchRequestId Nothing upsertJourneyLegAction blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode
  search _ = throwError (InternalError "Not supported")

  confirm (MetroLegRequestConfirm MetroLegRequestConfirmData {..}) = CFRFS.confirm personId merchantId quoteId bookLater bookingAllowed Nothing Spec.METRO categorySelectionReq isSingleMode mbEnableOffer mbIsMockPayment Nothing
  confirm _ = throwError (InternalError "Not supported")

  update (MetroLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (MetroLegRequestCancel legData) = CFRFS.cancel legData.searchId legData.cancellationType
  cancel _ = throwError (InternalError "Not supported")

  getState (MetroLegRequestGetState req) = CFRFS.getState DTrip.Metro req.searchId req.riderLastPoints False Nothing req.journeyLeg
  getState _ = throwError (InternalError "Not supported")

  getInfo (MetroLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.journeyLeg req.journeyLegs
  getInfo _ = throwError (InternalError "Not supported")

  getFare (MetroLegRequestGetFare MetroLegRequestGetFareData {..}) = CFRFS.getFare riderId merchant merchantOpCity Spec.METRO Nothing routeDetails fromArrivalTime agencyGtfsId Nothing blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode Nothing
  getFare _ = throwError (InternalError "Not supported")
