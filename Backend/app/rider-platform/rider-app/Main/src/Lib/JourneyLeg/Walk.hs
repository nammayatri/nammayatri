{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Walk where

import Domain.Types.Trip as DTrip
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JLT
import Lib.JourneyLeg.Types.Walk
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import qualified Lib.JourneyModule.Types as JT
import SharedLogic.Search
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg

instance JT.JourneyLeg WalkLegRequest m where
  search _ = throwError (InternalError "Not supported")

  confirm (WalkLegRequestConfirm _) = return ()
  confirm _ = throwError (InternalError "Not supported")

  update (WalkLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (WalkLegRequestCancel legData) = do
    QJourneyLeg.updateIsDeleted (Just True) (Just legData.walkLegId.getId)
  cancel _ = throwError (InternalError "Not supported")

  isCancellable ((WalkLegRequestIsCancellable legData)) = return $ JT.IsCancellableResponse {canCancel = True}
  isCancellable _ = throwError (InternalError "Not Supported")

  getState (WalkLegRequestGetState req) = do
    let (oldStatus, trackingStatus) = JMStateUtils.getWalkAllStatuses req.journeyLeg
    return $
      JT.Single $
        JT.JourneyLegStateData
          { status = oldStatus,
            legStatus = JT.WalkStatusElement $ JT.JourneyWalkLegStatus {trackingStatus = trackingStatus},
            userPosition = (.latLong) <$> listToMaybe req.riderLastPoints,
            vehiclePositions = [],
            legOrder = journeyLegInfo.journeyLegOrder,
            subLegOrder = 1,
            mode = DTrip.Walk
          }
  getState _ = throwError (InternalError "Not supported")

  getInfo (WalkLegRequestGetInfo req) = do
    Just <$> JT.mkWalkLegInfoFromWalkLegData req.journeyLeg
  getInfo _ = throwError (InternalError "Not supported")

  getFare (WalkLegRequestGetFare _) = do
    return $
      ( True,
        Just $
          JT.GetFareResponse
            { estimatedMinFare = HighPrecMoney {getHighPrecMoney = 0},
              estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 0},
              serviceTypes = Nothing
            }
      )
  getFare _ = throwError (InternalError "Not supported")
