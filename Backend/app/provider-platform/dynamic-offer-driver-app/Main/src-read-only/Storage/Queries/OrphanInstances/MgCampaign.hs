{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MgCampaign where

import qualified Domain.Types.MgCampaign
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Storage.Beam.MgCampaign as Beam

instance FromTType' Beam.MgCampaign Domain.Types.MgCampaign.MgCampaign where
  fromTType' (Beam.MgCampaignT {..}) = do
    pure $
      Just
        Domain.Types.MgCampaign.MgCampaign
          { createdBy = createdBy,
            currency = currency,
            description = description,
            endDate = endDate,
            forceAssign = forceAssign,
            frequency = frequency,
            guaranteeAmount = guaranteeAmount,
            id = Kernel.Types.Id.Id id,
            maxBudget = maxBudget,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            onFleetChange = onFleetChange,
            optInCutoffMinutes = optInCutoffMinutes,
            settlementGraceMinutes = settlementGraceMinutes,
            startDate = startDate,
            status = status,
            vehicleCategories = fromMaybe [] (Kernel.Utils.Text.decodeFromText vehicleCategories),
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MgCampaign Domain.Types.MgCampaign.MgCampaign where
  toTType' (Domain.Types.MgCampaign.MgCampaign {..}) = do
    Beam.MgCampaignT
      { Beam.createdBy = createdBy,
        Beam.currency = currency,
        Beam.description = description,
        Beam.endDate = endDate,
        Beam.forceAssign = forceAssign,
        Beam.frequency = frequency,
        Beam.guaranteeAmount = guaranteeAmount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxBudget = maxBudget,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.onFleetChange = onFleetChange,
        Beam.optInCutoffMinutes = optInCutoffMinutes,
        Beam.settlementGraceMinutes = settlementGraceMinutes,
        Beam.startDate = startDate,
        Beam.status = status,
        Beam.vehicleCategories = Kernel.Utils.Text.encodeToText vehicleCategories,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
