module Domain.Types.DriverReferral where

import Data.Time
import Domain.Types.Person (Person)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data DriverReferral = DriverReferral
  { referralCode :: Id Text,
    driverId :: Id Person,
    linkedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
