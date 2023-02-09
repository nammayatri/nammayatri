{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.DriverInformation where

import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.Types.Id

data DriverInformationE e = DriverInformation
  { driverId :: Id Person,
    adminId :: Maybe (Id Person),
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    verified :: Bool,
    referralCode :: Maybe (EncryptedHashedField e Text),
    lastEnabledOn :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type DriverInformation = DriverInformationE 'AsEncrypted

instance FromJSON (EncryptedHashed Text)

instance ToJSON (EncryptedHashed Text)

instance FromJSON DriverInformation

instance ToJSON DriverInformation
