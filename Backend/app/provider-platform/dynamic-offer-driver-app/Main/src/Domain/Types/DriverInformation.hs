{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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
