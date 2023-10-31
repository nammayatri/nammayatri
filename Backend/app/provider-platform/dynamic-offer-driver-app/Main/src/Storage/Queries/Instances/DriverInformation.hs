{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Instances.DriverInformation where

import qualified Data.ByteString as BS
import qualified Database.Beam.Query ()
import Domain.Types.DriverInformation as DriverInfo
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Beam.DriverInformation as BeamDI

instance FromTType' BeamDI.DriverInformation DriverInformation where
  fromTType' BeamDI.DriverInformationT {..} = do
    pure $
      Just
        DriverInformation
          { driverId = Id driverId,
            adminId = Id <$> adminId,
            merchantId = Id <$> merchantId,
            referralCode = EncryptedHashed <$> (Encrypted <$> referralCode) <*> Just (DbHash BS.empty),
            ..
          }

instance ToTType' BeamDI.DriverInformation DriverInformation where
  toTType' DriverInformation {..} = do
    BeamDI.DriverInformationT
      { BeamDI.driverId = getId driverId,
        BeamDI.adminId = getId <$> adminId,
        BeamDI.merchantId = getId <$> merchantId,
        BeamDI.active = active,
        BeamDI.onRide = onRide,
        BeamDI.enabled = enabled,
        BeamDI.blocked = blocked,
        BeamDI.blockedReason = blockedReason,
        BeamDI.blockExpiryTime = blockExpiryTime,
        BeamDI.numOfLocks = numOfLocks,
        BeamDI.verified = verified,
        BeamDI.subscribed = subscribed,
        BeamDI.paymentPending = paymentPending,
        BeamDI.aadhaarVerified = aadhaarVerified,
        BeamDI.referralCode = referralCode <&> unEncrypted . (.encrypted),
        BeamDI.lastEnabledOn = lastEnabledOn,
        BeamDI.canDowngradeToSedan = canDowngradeToSedan,
        BeamDI.canDowngradeToHatchback = canDowngradeToHatchback,
        BeamDI.canDowngradeToTaxi = canDowngradeToTaxi,
        BeamDI.mode = mode,
        BeamDI.autoPayStatus = autoPayStatus,
        BeamDI.payerVpa = payerVpa,
        BeamDI.enabledAt = enabledAt,
        BeamDI.createdAt = createdAt,
        BeamDI.updatedAt = updatedAt,
        BeamDI.compAadhaarImagePath = compAadhaarImagePath,
        BeamDI.availableUpiApps = availableUpiApps,
        BeamDI.optForRental = optForRental
      }
