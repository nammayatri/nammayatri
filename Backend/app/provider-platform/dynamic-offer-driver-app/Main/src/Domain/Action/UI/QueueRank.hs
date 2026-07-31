module Domain.Action.UI.QueueRank where

import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import qualified Storage.Queries.DriverInformation as QDI
import Tools.Error

data QueueRankResponse = QueueRankResponse
  { queuePositionRange :: Maybe (Int, Int),
    queueSize :: Int,
    isBlockedForAirport :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

getQueuePosition ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Text ->
  Text ->
  Environment.Flow QueueRankResponse
getQueuePosition (driverId, _merchantId, _merchantOpCityId) specialLocationId vehicleType = do
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  now <- getCurrentTime
  enableForAirport <- QDI.resolveAirportRestriction now driverInfo
  if enableForAirport == DI.BLOCKED
    then
      pure $
        QueueRankResponse
          { queuePositionRange = Nothing,
            queueSize = 0,
            isBlockedForAirport = True
          }
    else do
      ltsResp <- LTSFlow.getQueueDriverPosition specialLocationId vehicleType driverId
      pure $
        QueueRankResponse
          { queuePositionRange = ltsResp.queuePositionRange,
            queueSize = ltsResp.queueSize,
            isBlockedForAirport = False
          }
