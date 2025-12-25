{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Dashboard.Common.Driver
  ( module Dashboard.Common.Driver,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Id
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import qualified Text.Show

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data DriverEndpoint
  = UpdateSubscriptionDriverFeeAndInvoiceEndpoint
  | FleetUnlinkVehicleEndpoint
  | SendMessageToDriverViaDashboardEndPoint
  | UpdateRCInvalidStatusEndPoint
  | SendFleetJoiningOtpEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "DriverEndpoint"

newtype DriverIds = EnableDriversRequest
  { driverIds :: [Id Driver]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- documents info ---------------------------------------

data DriverDocumentsInfoRes = DriverDocumentsInfoRes
  { registered :: !Int,
    verified :: !Int,
    enabled :: !Int,
    blocked :: !Int,
    subscribed :: !Int,
    validDocuments :: !DocumentsByStateInfo,
    invalidDocuments :: !DocumentsByStateInfo,
    verificationPending :: !DocumentsByStateInfo,
    verificationFailed :: !DocumentsByStateInfo,
    verificationLimitExceeded :: !DocumentsByStateInfo,
    docsExpiringInMonth :: !DocumentsByStateInfo,
    onboardingDate :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentsByStateInfo = DocumentsByStateInfo
  { driverLicense :: !Int,
    vehicleRegistrationCertificate :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

emptyDocumentsByStateInfo :: DocumentsByStateInfo
emptyDocumentsByStateInfo = DocumentsByStateInfo 0 0

emptyInfo :: DriverDocumentsInfoRes
emptyInfo =
  DriverDocumentsInfoRes
    { registered = 0,
      verified = 0,
      enabled = 0,
      blocked = 0,
      subscribed = 0,
      validDocuments = emptyDocumentsByStateInfo,
      invalidDocuments = emptyDocumentsByStateInfo,
      verificationPending = emptyDocumentsByStateInfo,
      verificationFailed = emptyDocumentsByStateInfo,
      verificationLimitExceeded = emptyDocumentsByStateInfo,
      docsExpiringInMonth = emptyDocumentsByStateInfo,
      onboardingDate = Nothing
    }

---------------------------------------------------------
-- driver activity --------------------------------------

data DriverActivityRes = DriverActivityRes
  { activeDriversInApp :: !Int,
    --    activeDriversInLastHour :: !Int,
    inactiveDrivers :: !Int
    --    inactiveDriversSinceTwoDays :: !Int,
    --    trendFrequency :: !Seconds,
    --    trend :: ![ActivityTrendItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

{-
data ActivityTrendItem = ActivityTrendItem
  { timestamp :: UTCTime,
    active :: Int,
    inactive :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
oneHour :: Seconds
oneHour = 3600
-}

mkDriverActivityRes :: (Int, Int) -> DriverActivityRes
mkDriverActivityRes (active, inactive) =
  DriverActivityRes
    { activeDriversInApp = active,
      --      activeDriversInLastHour = 0,
      inactiveDrivers = inactive
      --      inactiveDriversSinceTwoDays = 0,
      --      trendFrequency = oneHour,
      --      trend = []
    }

data DriverLicense

data VehicleRegistrationCertificate

---------------------------------------------------------
-- Get Route driver ids ---------------------------------------

----------- update driver fees ---------------

data ReasonForDisablingServiceCharge = OUT_SICK | VEHICLE_UNDER_MAINTENANCE | EXITED_INITIATIVE | SWITCH_VEHICLE | PROMOTIIONAL_ACTIVITY | NOTIFIED_LEAVE | OTHER
  deriving (Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Read)

instance Show ReasonForDisablingServiceCharge where
  show OUT_SICK = "sickness"
  show VEHICLE_UNDER_MAINTENANCE = "vehicle under maintainence"
  show EXITED_INITIATIVE = "initiative exited from intiative"
  show SWITCH_VEHICLE = "switching of vehicle"
  show PROMOTIIONAL_ACTIVITY = "attending promotional activity"
  show NOTIFIED_LEAVE = "notified leave"
  show OTHER = "miscellaneous"

$(mkHttpInstancesForEnum ''ReasonForDisablingServiceCharge)

data DriverFeeStatus
  = ONGOING
  | PAYMENT_PENDING
  | PAYMENT_OVERDUE
  | CLEARED
  | EXEMPTED
  | COLLECTED_CASH
  | INACTIVE
  | CLEARED_BY_YATRI_COINS
  | MANUAL_REVIEW_NEEDED
  | REFUND_PENDING
  | REFUNDED
  | REFUND_FAILED
  | REFUND_MANUAL_REVIEW_REQUIRED
  | ONE_TIME_SECURITY_ADJUSTED
  | SETTLED
  | IN_DISPUTE_WINDOW
  | ADDED_TO_INVOICE
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance CH.ClickhouseValue DriverFeeStatus

-- to avoid naming conflict
type StripeAddress = Stripe.Address
