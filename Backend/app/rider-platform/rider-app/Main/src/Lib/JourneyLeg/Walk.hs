{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Walk where

import Control.Lens ((^?), _head)
import Domain.Types.Trip as DTrip
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Walk
import qualified Lib.JourneyModule.State.Types as JMStateTypes
import qualified Lib.JourneyModule.State.Utils as JMStateUtils
import qualified Lib.JourneyModule.Types as JT

instance JT.JourneyLeg WalkLegRequest m where
  search _ = throwError (InternalError "Not supported")

  confirm (WalkLegRequestConfirm _) = return ()
  confirm _ = throwError (InternalError "Not supported")

  update (WalkLegRequestUpdate _) = return ()
  update _ = throwError (InternalError "Not supported")

  cancel (WalkLegRequestCancel _) = return ()
  cancel _ = throwError (InternalError "Not supported")

  getState (WalkLegRequestGetState req) = do
    let (oldStatus, trackingStatus, trackingStatusLastUpdatedAt) = JMStateUtils.getWalkAllStatuses req.journeyLeg
    now <- getCurrentTime
    return $
      JT.Single $
        JT.JourneyLegStateData
          { status = oldStatus,
            bookingStatus = JMStateTypes.Initial JMStateTypes.BOOKING_PENDING,
            trackingStatus = trackingStatus,
            trackingStatusLastUpdatedAt = fromMaybe now trackingStatusLastUpdatedAt,
            userPosition = req.riderLastPoints ^? _head <&> (.latLong),
            vehiclePositions = [],
            legOrder = req.journeyLeg.sequenceNumber,
            subLegOrder = 1,
            mode = DTrip.Walk,
            fleetNo = Nothing
          }
  getState _ = throwError (InternalError "Not supported")

  getInfo (WalkLegRequestGetInfo req) = do
    Just <$> JT.mkWalkLegInfoFromWalkLegData req.personId req.journeyLeg
  getInfo _ = throwError (InternalError "Not supported")

  getFare (WalkLegRequestGetFare _) = do
    return $
      ( True,
        Just $
          JT.GetFareResponse
            { estimatedMinFare = HighPrecMoney {getHighPrecMoney = 0},
              estimatedMaxFare = HighPrecMoney {getHighPrecMoney = 0},
              liveVehicleAvailableServiceTypes = Nothing,
              possibleRoutes = Nothing
            }
      )
  getFare _ = throwError (InternalError "Not supported")
