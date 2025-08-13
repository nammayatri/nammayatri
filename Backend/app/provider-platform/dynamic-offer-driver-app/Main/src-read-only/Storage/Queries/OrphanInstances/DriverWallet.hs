{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverWallet where

import qualified Domain.Types.DriverWallet
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverWallet as Beam

instance FromTType' Beam.DriverWallet Domain.Types.DriverWallet.DriverWallet where
  fromTType' (Beam.DriverWalletT {..}) = do
    pure $
      Just
        Domain.Types.DriverWallet.DriverWallet
          { collectionAmount = collectionAmount,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            driverPayable = driverPayable,
            gstDeduction = gstDeduction,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            merchantPayable = merchantPayable,
            payoutOrderId = Kernel.Types.Id.Id <$> payoutOrderId,
            payoutStatus = payoutStatus,
            rideId = Kernel.Types.Id.Id <$> rideId,
            runningBalance = runningBalance,
            transactionType = transactionType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverWallet Domain.Types.DriverWallet.DriverWallet where
  toTType' (Domain.Types.DriverWallet.DriverWallet {..}) = do
    Beam.DriverWalletT
      { Beam.collectionAmount = collectionAmount,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverPayable = driverPayable,
        Beam.gstDeduction = gstDeduction,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.merchantPayable = merchantPayable,
        Beam.payoutOrderId = Kernel.Types.Id.getId <$> payoutOrderId,
        Beam.payoutStatus = payoutStatus,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.runningBalance = runningBalance,
        Beam.transactionType = transactionType,
        Beam.updatedAt = updatedAt
      }
