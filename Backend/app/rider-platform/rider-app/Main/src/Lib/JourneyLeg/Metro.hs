{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Metro where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Common.FRFS as CFRFS
import Lib.JourneyLeg.Types.Metro
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg MetroLegRequest m where
  search (MetroLegRequestSearch MetroLegRequestSearchData {..}) = CFRFS.search Spec.METRO personId merchantId quantity city journeyLeg
  search _ = throwError (InternalError "Not supported")

  confirm (MetroLegRequestConfirm req) = do
    when (not req.skipBooking && req.bookingAllowed) $ do
      quoteId <- req.quoteId & fromMaybeM (InvalidRequest "You can't confrim metro before getting the fare")
      void $ FRFSTicketService.postFrfsQuoteConfirm (Just req.personId, req.merchantId) quoteId
  confirm _ = throwError (InternalError "Not supported")

  update (MetroLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (MetroLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")

  isCancellable ((MetroLegRequestIsCancellable _legData)) = return $ JT.IsCancellableResponse {canCancel = False}
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (MetroLegRequestGetState req) = CFRFS.getState req.searchId req.riderLastPoints req.isLastJustCompleted
  getState _ = throwError (InternalError "Not supported")

  getInfo (MetroLegRequestGetInfo req) = CFRFS.getInfo req.searchId req.fallbackFare
  getInfo _ = throwError (InternalError "Not supported")

  getFare (MetroLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 10}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 10}})
  getFare _ = throwError (InternalError "Not supported")
