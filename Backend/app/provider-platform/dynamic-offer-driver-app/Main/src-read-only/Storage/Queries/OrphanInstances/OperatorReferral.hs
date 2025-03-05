{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.OperatorReferral where

import qualified Domain.Types.OperatorReferral
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.OperatorReferral as Beam

instance FromTType' Beam.OperatorReferral Domain.Types.OperatorReferral.OperatorReferral where
  fromTType' (Beam.OperatorReferralT {..}) = do
    pure $
      Just
        Domain.Types.OperatorReferral.OperatorReferral
          { linkedAt = linkedAt,
            operatorId = operatorId,
            referralCode = Kernel.Types.Id.Id referralCode,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.OperatorReferral Domain.Types.OperatorReferral.OperatorReferral where
  toTType' (Domain.Types.OperatorReferral.OperatorReferral {..}) = do
    Beam.OperatorReferralT
      { Beam.linkedAt = linkedAt,
        Beam.operatorId = operatorId,
        Beam.referralCode = Kernel.Types.Id.getId referralCode,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
