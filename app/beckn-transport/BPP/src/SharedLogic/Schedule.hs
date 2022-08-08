module SharedLogic.Schedule where

import Beckn.Prelude
import Beckn.Scheduler
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Dhall (FromDhall)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Organization as DOrg

data JobType = AllocateRental | FakeType
  deriving (Generic, FromDhall, Eq, Ord, Show, FromJSON, ToJSON)

data AllocateRentalJobData = AllocateRentalJobData
  { bookingId :: Id DRB.Booking,
    shortOrgId :: ShortId DOrg.Organization
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

createScheduleRentalRideRequestJob :: UTCTime -> AllocateRentalJobData -> Esq.SqlDB ()
createScheduleRentalRideRequestJob scheduledAt jobData =
  void $
    createJobByTime scheduledAt $
      JobEntry
        { jobType = AllocateRental,
          jobData = jobData,
          maxErrors = 5
        }
