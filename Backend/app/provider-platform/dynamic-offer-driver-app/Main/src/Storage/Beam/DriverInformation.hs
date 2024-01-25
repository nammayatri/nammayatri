{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.DriverInformation where

import qualified Database.Beam as B
import qualified Domain.Types.DriverInformation as Domain
import Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverInformationT f = DriverInformationT
  { driverId :: B.C f Text,
    adminId :: B.C f (Maybe Text),
    merchantId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f (Maybe Text),
    active :: B.C f Bool,
    onRide :: B.C f Bool,
    enabled :: B.C f Bool,
    blocked :: B.C f Bool,
    numOfLocks :: B.C f Int,
    verified :: B.C f Bool,
    subscribed :: B.C f Bool,
    paymentPending :: B.C f Bool,
    aadhaarVerified :: B.C f Bool,
    lastEnabledOn :: B.C f (Maybe UTCTime),
    referralCode :: B.C f (Maybe Text),
    referredByDriverId :: B.C f (Maybe Text),
    totalReferred :: B.C f (Maybe Int),
    canDowngradeToSedan :: B.C f Bool,
    canDowngradeToHatchback :: B.C f Bool,
    canDowngradeToTaxi :: B.C f Bool,
    canSwitchToRental :: B.C f (Maybe Bool),
    blockedReason :: B.C f (Maybe Text),
    blockExpiryTime :: B.C f (Maybe UTCTime),
    mode :: B.C f (Maybe Domain.DriverMode),
    autoPayStatus :: B.C f (Maybe Domain.DriverAutoPayStatus),
    payerVpa :: B.C f (Maybe Text),
    blockStateModifier :: B.C f (Maybe Text),
    enabledAt :: B.C f (Maybe UTCTime),
    compAadhaarImagePath :: B.C f (Maybe Text),
    availableUpiApps :: B.C f (Maybe Text),
    driverDob :: B.C f (Maybe UTCTime),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverInformationT where
  data PrimaryKey DriverInformationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type DriverInformation = DriverInformationT Identity

$(enableKVPG ''DriverInformationT ['driverId] [])

$(mkTableInstances ''DriverInformationT "driver_information")
