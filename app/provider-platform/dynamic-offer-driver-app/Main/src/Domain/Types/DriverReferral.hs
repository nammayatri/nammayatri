module Domain.Types.DriverReferral where

import Domain.Types.Person (Person)
import Kernel.Types.Id
import EulerHS.Prelude hiding (id)
import Data.Time
data DriverReferral = DriverReferral
  { 
    referralCode :: Id Text,
    driverId :: Id Person,
    linkedAt :: Maybe UTCTime
  }
  deriving (Generic, Show, Eq)
