{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.DriverInformation where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Time (UTCTime)
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import EulerHS.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common (Money)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

data DriverMode
  = ONLINE
  | OFFLINE
  | SILENT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable DriverMode

derivePersistField "DriverMode"

$(mkBeamInstancesForEnum ''DriverMode)

$(mkHttpInstancesForEnum ''DriverMode)

data DriverAutoPayStatus
  = PENDING
  | ACTIVE
  | SUSPENDED
  | PAUSED_PSP
  | CANCELLED_PSP
  | MANDATE_FAILED
  | MANDATE_EXPIRED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable DriverAutoPayStatus

derivePersistField "DriverAutoPayStatus"

$(mkBeamInstancesForEnum ''DriverAutoPayStatus)

$(mkHttpInstancesForEnum ''DriverAutoPayStatus)

data DriverInformation = DriverInformation
  { driverId :: Id Person,
    adminId :: Maybe (Id Person),
    merchantId :: Maybe (Id DMerchant.Merchant),
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    numOfLocks :: Int,
    verified :: Bool,
    subscribed :: Bool,
    paymentPending :: Bool,
    referralCode :: Maybe Text,
    referredByDriverId :: Maybe (Id Person),
    totalReferred :: Maybe Int,
    lastEnabledOn :: Maybe UTCTime,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    canSwitchToRental :: Bool,
    mode :: Maybe DriverMode,
    aadhaarVerified :: Bool,
    autoPayStatus :: Maybe DriverAutoPayStatus,
    blockedReason :: Maybe Text,
    blockExpiryTime :: Maybe UTCTime,
    payerVpa :: Maybe Text,
    enabledAt :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    compAadhaarImagePath :: Maybe Text,
    availableUpiApps :: Maybe Text,
    blockStateModifier :: Maybe Text,
    driverDob :: Maybe UTCTime,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }
  deriving (Generic)

data DriverSummary = DriverSummary
  { totalEarnings :: Money,
    bonusEarned :: Money,
    totalCompletedTrips :: Int,
    lateNightTrips :: Int,
    lastRegistered :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data DriverMissedOpp = DriverMissedOpp
  { cancellationRate :: Int,
    ridesCancelled :: Int,
    totalRides :: Int,
    missedEarnings :: Money
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype DriverBadges = DriverBadges
  { driverBadges :: [Badges]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data Badges = Badges
  { badgeName :: Text,
    badgeCount :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

instance FromJSON DriverInformation

instance ToJSON DriverInformation
