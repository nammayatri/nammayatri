{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MultiModalFareLegRules where

import qualified Domain.Types.MultiModalFareLegRules
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MultiModalFareLegRules as Beam

instance FromTType' Beam.MultiModalFareLegRules Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules where
  fromTType' (Beam.MultiModalFareLegRulesT {..}) = do
    pure $
      Just
        Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules
          { amount = amount,
            currency = currency,
            fromTimeFrameId = Kernel.Types.Id.Id fromTimeFrameId,
            id = Kernel.Types.Id.Id id,
            maxDist = maxDist,
            minDist = minDist,
            networkId = Kernel.Types.Id.Id networkId,
            passengerType = passengerType,
            paymentMedia = paymentMedia,
            toTimeFrameId = Kernel.Types.Id.Id toTimeFrameId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MultiModalFareLegRules Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules where
  toTType' (Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules {..}) = do
    Beam.MultiModalFareLegRulesT
      { Beam.amount = amount,
        Beam.currency = currency,
        Beam.fromTimeFrameId = Kernel.Types.Id.getId fromTimeFrameId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxDist = maxDist,
        Beam.minDist = minDist,
        Beam.networkId = Kernel.Types.Id.getId networkId,
        Beam.passengerType = passengerType,
        Beam.paymentMedia = paymentMedia,
        Beam.toTimeFrameId = Kernel.Types.Id.getId toTimeFrameId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
