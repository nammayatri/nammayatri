{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetReferral where

import qualified Domain.Types.FleetReferral
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetReferral as Beam

instance FromTType' Beam.FleetReferral Domain.Types.FleetReferral.FleetReferral where
  fromTType' (Beam.FleetReferralT {..}) = do
    pure $
      Just
        Domain.Types.FleetReferral.FleetReferral
          { fleetOwnerId = fleetOwnerId,
            linkedAt = linkedAt,
            referralCode = Kernel.Types.Id.Id referralCode,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetReferral Domain.Types.FleetReferral.FleetReferral where
  toTType' (Domain.Types.FleetReferral.FleetReferral {..}) = do
    Beam.FleetReferralT
      { Beam.fleetOwnerId = fleetOwnerId,
        Beam.linkedAt = linkedAt,
        Beam.referralCode = Kernel.Types.Id.getId referralCode,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
