{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Bus where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Bus
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg BusLegRequest m where
  search (BusLegRequestSearch _) = do
    -- FRFSSearch.create multimodalLeg busLegSearchData
    -- void $ FRFSTicketService.postFrfsSearch (Just personId, merchantId) (Just originCity) Spec.BUS frfsSearchReq
    return ()
  search _ = throwError (InternalError "Not supported")

  confirm (BusLegRequestConfirm _) = do
    -- frfsSearchReq <- QFRFSSearch.findById taxiLegConfirmData.searchReqId
    -- quoteId <- searchReq.journeySearchData.pricingId
    -- case busLegConfirmData.skippedBooking of
    --   True ->
    --      -- subtract price from total estimate
    --     QFRFSSearch.updateIsSkipped True
    --   False ->
    --     void $ FRFSTicketService.postFrfsQuoteConfirm (personId, merchantId) (Id quoteId)
    throwError (InternalError "Not supported")
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

  getState (BusLegRequestGetState _) = throwError (InternalError "Not supported")
  getState _ = throwError (InternalError "Not supported")

  getInfo (BusLegRequestGetInfo _) = throwError (InternalError "Not supported")
  getInfo _ = throwError (InternalError "Not supported")

  getFare (BusLegRequestGetFare _) = do
    return (Just $ JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 20}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 20}})
  getFare _ = throwError (InternalError "Not supported")
