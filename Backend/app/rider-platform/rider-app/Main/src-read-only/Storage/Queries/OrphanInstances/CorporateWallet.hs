{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateWallet where

import qualified Domain.Types.CorporateWallet
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateWallet as Beam

instance FromTType' Beam.CorporateWallet Domain.Types.CorporateWallet.CorporateWallet where
  fromTType' (Beam.CorporateWalletT {..}) = do
    pure $
      Just
        Domain.Types.CorporateWallet.CorporateWallet
          { id = Kernel.Types.Id.Id id,
            corporateEntityId = Kernel.Types.Id.Id corporateEntityId,
            balance = Kernel.Prelude.realToFrac balance,
            currency = Kernel.Prelude.fromMaybe Kernel.Prelude.INR (Kernel.Prelude.readMaybe (Kernel.Prelude.toString currency)),
            status = Kernel.Prelude.fromMaybe Domain.Types.CorporateWallet.CW_ACTIVE (Kernel.Prelude.readMaybe (Kernel.Prelude.toString status)),
            graceStartedAt = graceStartedAt,
            lastTopUpAt = lastTopUpAt,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateWallet Domain.Types.CorporateWallet.CorporateWallet where
  toTType' (Domain.Types.CorporateWallet.CorporateWallet {..}) = do
    Beam.CorporateWalletT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.corporateEntityId = Kernel.Types.Id.getId corporateEntityId,
        Beam.balance = Kernel.Prelude.realToFrac balance,
        Beam.currency = Kernel.Prelude.show currency,
        Beam.status = Kernel.Prelude.show status,
        Beam.graceStartedAt = graceStartedAt,
        Beam.lastTopUpAt = lastTopUpAt,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
