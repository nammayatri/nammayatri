module SharedLogic.Allocator where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (Meters, Money)
import Beckn.Types.Id
import Beckn.Utils.Dhall (FromDhall)
import qualified Domain.Types.SearchRequest as DSR
import Lib.Scheduler

data JobType = SendSearchRequestToDriver
  deriving (Generic, FromDhall, Eq, Ord, Show, FromJSON, ToJSON)

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { requestId :: Id DSR.SearchRequest,
    baseFare :: Money,
    estimatedRideDistance :: Meters,
    driverMinExtraFee :: Money,
    driverMaxExtraFee :: Money
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

createAllocatorSendSearchRequestToDriverJob :: NominalDiffTime -> SendSearchRequestToDriverJobData -> Esq.SqlDB ()
createAllocatorSendSearchRequestToDriverJob inTime jobData = do
  void $
    createJobIn inTime $
      JobEntry
        { jobType = SendSearchRequestToDriver,
          jobData = jobData,
          maxErrors = 5
        }
