{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.TollChargeApproval
  ( RequestTollChargeApprovalReq (..),
    TollChargeApprovalModeRes (..),
    PendingTollChargeApproval (..),
    TollChargeApprovalParam (..),
    pendingTollChargeApprovalKey,
    pendingTollChargeApprovalRedisTtlSec,
    requestTollChargeApproval,
    getTollChargeApprovalMode,
  )
where

import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRideStatus
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Notification as Notification
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Version (textToVersion)
import Kernel.Utils.Common hiding (id)
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

data RequestTollChargeApprovalReq = RequestTollChargeApprovalReq
  { bppRideId :: Text,
    tollNames :: Maybe [Text],
    amount :: HighPrecMoney,
    currency :: Currency,
    approvalTimeoutSeconds :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype TollChargeApprovalModeRes = TollChargeApprovalModeRes
  { customerApprovalSupported :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data PendingTollChargeApproval = PendingTollChargeApproval
  { tollNames :: Maybe [Text],
    amount :: HighPrecMoney,
    currency :: Currency,
    requestedAt :: UTCTime,
    approvalTimeoutSeconds :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TollChargeApprovalParam = TollChargeApprovalParam
  { rideId :: Text,
    tollNames :: Maybe [Text],
    amount :: HighPrecMoney,
    currency :: Currency,
    approvalTimeoutSeconds :: Int
  }
  deriving (Generic, Show, ToJSON)

pendingTollChargeApprovalKey :: Id DRide.Ride -> Text
pendingTollChargeApprovalKey rideId = "TollChargeApprovalRequest:RideId-" <> rideId.getId

pendingTollChargeApprovalRedisTtlSec :: Int
pendingTollChargeApprovalRedisTtlSec = 21600

-- The rider's app can show the approval prompt when its bundle version is at least the configured minimum.
-- Without a configured minimum, or with an unknown rider version, the prompt is not supported.
getTollChargeApprovalMode :: Maybe Text -> Text -> Flow TollChargeApprovalModeRes
getTollChargeApprovalMode apiKey bppBookingId = do
  validateInternalApiKey apiKey
  booking <- B.runInReplica $ QBooking.findByBPPBookingId (Id bppBookingId) >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId)
  person <- B.runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  let mbMinimumVersion = (either (const Nothing) Just . textToVersion) =<< riderConfig.manualChargeApprovalMinCustomerVersion
      customerApprovalSupported = case (person.clientBundleVersion, mbMinimumVersion) of
        (Just clientVersion, Just minimumVersion) -> clientVersion >= minimumVersion
        _ -> False
  pure TollChargeApprovalModeRes {customerApprovalSupported}

-- Called by the BPP once a driver has declared a toll charge that needs the rider's approval. The
-- request is kept until the rider answers or the approval window passes, and the rider is notified.
requestTollChargeApproval :: Maybe Text -> Text -> RequestTollChargeApprovalReq -> Flow APISuccess
requestTollChargeApproval apiKey bppBookingId req = do
  validateInternalApiKey apiKey
  booking <- B.runInReplica $ QBooking.findByBPPBookingId (Id bppBookingId) >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId)
  ride <- B.runInReplica $ QRide.findByBPPRideId (Id req.bppRideId) >>= fromMaybeM (RideDoesNotExist req.bppRideId)
  unless (ride.bookingId == booking.id && ride.status == DRideStatus.INPROGRESS) $ throwError $ RideInvalidStatus "Toll charge approval needs the booking's ride in progress"
  person <- B.runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  now <- getCurrentTime
  Redis.setExp
    (pendingTollChargeApprovalKey ride.id)
    PendingTollChargeApproval
      { tollNames = req.tollNames,
        amount = req.amount,
        currency = req.currency,
        requestedAt = now,
        approvalTimeoutSeconds = req.approvalTimeoutSeconds
      }
    pendingTollChargeApprovalRedisTtlSec
  let approvalParam =
        TollChargeApprovalParam
          { rideId = ride.id.getId,
            tollNames = req.tollNames,
            amount = req.amount,
            currency = req.currency,
            approvalTimeoutSeconds = req.approvalTimeoutSeconds
          }
  Notify.dynamicNotifyPerson
    person
    (Notify.createNotificationReq "TOLL_CHARGE_APPROVAL_REQUIRED" identity)
    approvalParam
    (Notification.Entity Notification.Product ride.id.getId approvalParam)
    Nothing
    [("amount", show req.amount), ("tollName", maybe "toll" (T.intercalate ", ") req.tollNames)]
    Nothing
    Nothing
  pure Success

validateInternalApiKey :: Maybe Text -> Flow ()
validateInternalApiKey apiKey = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
