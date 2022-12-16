module SharedLogic.Allocator where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (Money)
import Beckn.Types.Id
import Beckn.Utils.Dhall (FromDhall)
import qualified Domain.Types.SearchRequest as DSR
import Lib.Scheduler

data JobType = SendSearchRequestToDriver
  deriving (Generic, FromDhall, Eq, Ord, Show, FromJSON, ToJSON)

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { requestId :: Id DSR.SearchRequest,
    baseFare :: Money
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

createAllocatorSendSearchRequestToDriverJob :: SendSearchRequestToDriverJobData -> Esq.SqlDB ()
createAllocatorSendSearchRequestToDriverJob jobData =
  void $
    createJobIn 0 $
      JobEntry
        { jobType = SendSearchRequestToDriver,
          jobData = jobData,
          maxErrors = 5
        }
