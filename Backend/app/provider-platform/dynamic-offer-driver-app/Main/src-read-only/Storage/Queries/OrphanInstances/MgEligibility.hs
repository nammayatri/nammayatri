{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MgEligibility where

import qualified Domain.Types.MgEligibility
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MgEligibility as Beam

instance FromTType' Beam.MgEligibility Domain.Types.MgEligibility.MgEligibility where
  fromTType' (Beam.MgEligibilityT {..}) = do
    pure $
      Just
        Domain.Types.MgEligibility.MgEligibility
          { campaignId = Kernel.Types.Id.Id campaignId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            invitedAt = invitedAt,
            notifiedAt = notifiedAt,
            reason = reason,
            source = source,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MgEligibility Domain.Types.MgEligibility.MgEligibility where
  toTType' (Domain.Types.MgEligibility.MgEligibility {..}) = do
    Beam.MgEligibilityT
      { Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invitedAt = invitedAt,
        Beam.notifiedAt = notifiedAt,
        Beam.reason = reason,
        Beam.source = source,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
