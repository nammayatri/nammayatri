module Domain.Types.DriverInformation where

import Beckn.External.Encryption
import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import EulerHS.Prelude

data DriverInformationE e = DriverInformation
  { driverId :: Id Person,
    adminId :: Maybe (Id Person),
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    verified :: Bool,
    referralCode :: Maybe (EncryptedHashedField e Text),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type DriverInformation = DriverInformationE 'AsEncrypted
