{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Walk where

import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Walk
import Kernel.Prelude
import qualified Lib.JourneyLeg.Types as JLT
import qualified Lib.JourneyModule.Types as JT
import SharedLogic.Search
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg
import Kernel.Types.Error

instance JT.JourneyLeg WalkLegRequest m where
  search (WalkLegRequestSearch WalkLegRequestSearchData {..}) = do
    fromLocation <- buildSearchReqLoc merchantId merchantOperatingCityId origin
    toLocation <- buildSearchReqLoc merchantId merchantOperatingCityId destination
    now <- getCurrentTime
    id <- generateGUID
    let journeySearchData =
          JLT.JourneySearchData
            { journeyId = journeyLegData.journeyId.getId,
              journeyLegOrder = journeyLegData.sequenceNumber,
              agency = journeyLegData.agency <&> (.name),
              skipBooking = False,
              convenienceCost = 0,
              pricingId = Nothing
            }
    let walkLeg =
          DWalkLeg.WalkLegMultimodal
            { id,
              estimatedDistance = journeyLegData.distance,
              estimatedDuration = Just journeyLegData.duration,
              fromLocation = fromLocation,
              toLocation = Just toLocation,
              journeyLegInfo = Just journeySearchData,
              riderId = parentSearchReq.riderId,
              startTime = fromMaybe now journeyLegData.fromArrivalTime,
              merchantId = parentSearchReq.merchantId,
              status = DWalkLeg.InPlan,
              merchantOperatingCityId = Just parentSearchReq.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }
    QWalkLeg.create walkLeg
  search _ = throwError (InternalError "Not supported")  

  confirm (WalkLegRequestConfirm _) = return () -- return nothing
  confirm _ = throwError (InternalError "Not supported")  

  update (WalkLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")  

  cancel (WalkLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")  

  getState (WalkLegRequestGetState req) = do
    legData <- QWalkLeg.findById req.walkLegId >>= fromMaybeM (InvalidRequest "WalkLeg Data not found")
    journeyLegInfo <- legData.journeyLegInfo & fromMaybeM (InvalidRequest "WalkLeg journey legInfo data missing")
    let status = JT.getWalkLegStatusFromWalkLeg legData journeyLegInfo
    return $ JT.JourneyLegState {status = status, currentPosition = Nothing}
  getState _ = throwError (InternalError "Not supported")  

  getInfo (WalkLegRequestGetInfo req) = do
    legData <- QWalkLeg.findById req.walkLegId >>= fromMaybeM (InvalidRequest "WalkLeg Data not found")
    JT.mkWalkLegInfoFromWalkLegData legData
  getInfo _ = throwError (InternalError "Not supported")

  getFare (WalkLegRequestGetFare _) = do
    return $
      JT.GetFareResponse
        { estimatedMinFare = HighPrecMoney {getHighPrecMoney = 0},
          estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 0}
        }
  getFare _ = throwError (InternalError "Not supported")
