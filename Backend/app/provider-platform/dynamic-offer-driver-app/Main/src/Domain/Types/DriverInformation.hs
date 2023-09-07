{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.DriverInformation where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time (UTCTime)
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person (Person)
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Common (Money)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Servant.API
import Tools.Beam.UtilsTH

data DriverMode
  = ONLINE
  | OFFLINE
  | SILENT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable DriverMode

derivePersistField "DriverMode"

$(mkBeamInstancesForEnum ''DriverMode)

instance FromHttpApiData DriverMode where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData DriverMode where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data DriverAutoPayStatus
  = PENDING
  | ACTIVE
  | SUSPENDED
  | PAUSED_PSP
  | CANCELLED_PSP
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable DriverAutoPayStatus

derivePersistField "DriverAutoPayStatus"

$(mkBeamInstancesForEnum ''DriverAutoPayStatus)

instance FromHttpApiData DriverAutoPayStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData DriverAutoPayStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data DriverInformationE e = DriverInformation
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
    referralCode :: Maybe (EncryptedHashedField e Text),
    lastEnabledOn :: Maybe UTCTime,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    mode :: Maybe DriverMode,
    aadhaarVerified :: Bool,
    autoPayStatus :: Maybe DriverAutoPayStatus,
    blockedReason :: Maybe Text,
    blockExpiryTime :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    compAadhaarImagePath :: Maybe Text
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

type DriverInformation = DriverInformationE 'AsEncrypted

instance FromJSON (EncryptedHashed Text)

instance ToJSON (EncryptedHashed Text)

instance FromJSON DriverInformation

instance ToJSON DriverInformation
