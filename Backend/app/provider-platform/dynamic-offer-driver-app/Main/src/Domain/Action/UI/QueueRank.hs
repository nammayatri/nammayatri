module Domain.Action.UI.QueueRank where

import qualified Domain.Action.UI.SpecialZoneQueue as SZQ
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import qualified SharedLogic.External.LocationTrackingService.Types as Fields
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import Tools.Error

data QueueRankResponse = QueueRankResponse
  { queuePositionRange :: Maybe (Int, Int),
    queueSize :: Int,
    isBlockedForAirport :: Bool,
    isPrioritized :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

-- Driver is prioritized if they have an active fleet association.
checkDriverProgramMembership :: Id DP.Person -> Environment.Flow (Maybe Bool)
checkDriverProgramMembership driverId = do
  mbAssoc <- QFDA.findByDriverId driverId True
  pure $ Just $ isJust mbAssoc

getQueuePosition ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Text ->
  Text ->
  Environment.Flow QueueRankResponse
getQueuePosition (driverId, merchantId, merchantOpCityId) specialLocationId vehicleType = do
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  now <- getCurrentTime
  enableForAirport <- QDI.resolveAirportRestriction now driverInfo
  if enableForAirport == DI.BLOCKED || driverInfo.blocked
    then
      pure $
        QueueRankResponse
          { queuePositionRange = Nothing,
            queueSize = 0,
            isBlockedForAirport = True,
            isPrioritized = Nothing
          }
    else do
      mbIsPrioritized <- checkDriverProgramMembership driverId
      case mbIsPrioritized of
        Just True -> do
          assigned <- SZQ.assignPickupGateForPrioritizedDriver driverId merchantId merchantOpCityId specialLocationId vehicleType
          if assigned
            then
              pure $
                QueueRankResponse
                  { queuePositionRange = Nothing,
                    queueSize = 0,
                    isBlockedForAirport = False,
                    isPrioritized = Just True
                  }
            else do
              ltsResp <- LTSFlow.getQueueDriverPosition specialLocationId vehicleType driverId
              pure $
                QueueRankResponse
                  { queuePositionRange = ltsResp.queuePositionRange,
                    queueSize = ltsResp.queueSize,
                    isBlockedForAirport = False,
                    isPrioritized = Just False
                  }
        _ -> do
          ltsResp <- LTSFlow.getQueueDriverPosition specialLocationId vehicleType driverId
          pure $
            QueueRankResponse
              { queuePositionRange = ltsResp.queuePositionRange,
                queueSize = ltsResp.queueSize,
                isBlockedForAirport = False,
                isPrioritized = Just False
              }
