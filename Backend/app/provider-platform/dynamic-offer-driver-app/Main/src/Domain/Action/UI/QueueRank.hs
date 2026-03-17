module Domain.Action.UI.QueueRank where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow

data QueueRankResponse = QueueRankResponse
  { queuePositionRange :: Maybe (Int, Int),
    queueSize :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

getQueuePosition ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Text ->
  Environment.Flow QueueRankResponse
getQueuePosition (driverId, _merchantId, _merchantOpCityId) specialLocationId = do
  ltsResp <- LTSFlow.getQueueDriverPosition specialLocationId driverId
  pure $
    QueueRankResponse
      { queuePositionRange = ltsResp.queuePositionRange,
        queueSize = ltsResp.queueSize
      }
