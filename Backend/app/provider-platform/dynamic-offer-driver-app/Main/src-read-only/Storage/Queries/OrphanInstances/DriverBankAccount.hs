{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverBankAccount where

import qualified Domain.Types.DriverBankAccount
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverBankAccount as Beam

instance FromTType' Beam.DriverBankAccount Domain.Types.DriverBankAccount.DriverBankAccount where
  fromTType' (Beam.DriverBankAccountT {..}) = do
    currentAccountLink' <- Kernel.Prelude.mapM parseBaseUrl currentAccountLink
    pure $
      Just
        Domain.Types.DriverBankAccount.DriverBankAccount
          { accountId = accountId,
            chargesEnabled = chargesEnabled,
            currentAccountLink = currentAccountLink',
            currentAccountLinkExpiry = currentAccountLinkExpiry,
            detailsSubmitted = detailsSubmitted,
            driverId = Kernel.Types.Id.Id driverId,
            paymentMode = paymentMode,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverBankAccount Domain.Types.DriverBankAccount.DriverBankAccount where
  toTType' (Domain.Types.DriverBankAccount.DriverBankAccount {..}) = do
    Beam.DriverBankAccountT
      { Beam.accountId = accountId,
        Beam.chargesEnabled = chargesEnabled,
        Beam.currentAccountLink = Kernel.Prelude.fmap showBaseUrl currentAccountLink,
        Beam.currentAccountLinkExpiry = currentAccountLinkExpiry,
        Beam.detailsSubmitted = detailsSubmitted,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.paymentMode = paymentMode,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
