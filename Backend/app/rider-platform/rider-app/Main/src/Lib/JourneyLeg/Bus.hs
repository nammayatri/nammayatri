{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Bus where

import Lib.JourneyLeg.Types.Bus
import Kernel.Utils.Common
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
    return ()
  confirm _ = throwError (InternalError "Not supported")

  update (BusLegRequestUpdate _) = do
    -- let customerLocation = get eta between user location and bus station
    -- let busLocation = get eta between bus and bus station
    -- let threshold = 50
    -- mark status with respect to bus -  Ontime, Departed, Delayed, Arriving, Finishing, Completed
    -- mark status with respect to user -  AtRiskOfMissing, Missed
    return ()
  update _ = throwError (InternalError "Not supported")

  cancel (BusLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")

  getState (BusLegRequestGetState _) = return ()
  getState _ = throwError (InternalError "Not supported")

  getInfo (BusLegRequestGetInfo _) = return ()
  getInfo _ = throwError (InternalError "Not supported")

  getFare (BusLegRequestGetFare _) = do
    return JT.GetFareResponse {estimatedMinFare = HighPrecMoney {getHighPrecMoney = 20}, estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 20}}
  getFare _ = throwError (InternalError "Not supported")
