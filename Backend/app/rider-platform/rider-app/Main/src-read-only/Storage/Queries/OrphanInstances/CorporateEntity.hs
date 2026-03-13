{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateEntity where

import qualified Domain.Types.CorporateEntity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateEntity as Beam

instance FromTType' Beam.CorporateEntity Domain.Types.CorporateEntity.CorporateEntity where
  fromTType' (Beam.CorporateEntityT {..}) = do
    pure $
      Just
        Domain.Types.CorporateEntity.CorporateEntity
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            name = name,
            registeredName = registeredName,
            gstin = gstin,
            industry = industry,
            contactPersonName = contactPersonName,
            contactEmail = contactEmail,
            contactPhone = contactPhone,
            billingAddress = billingAddress,
            billingModel = Kernel.Prelude.fromMaybe Domain.Types.CorporateEntity.PER_TRIP (Kernel.Prelude.readMaybe (Kernel.Prelude.toString billingModel)),
            billingCycleType = Kernel.Prelude.fromMaybe Domain.Types.CorporateEntity.MONTHLY (Kernel.Prelude.readMaybe (Kernel.Prelude.toString billingCycleType)),
            creditLimit = Kernel.Prelude.realToFrac creditLimit,
            currency = Kernel.Prelude.fromMaybe Kernel.Prelude.INR (Kernel.Prelude.readMaybe (Kernel.Prelude.toString currency)),
            status = Kernel.Prelude.fromMaybe Domain.Types.CorporateEntity.ONBOARDING (Kernel.Prelude.readMaybe (Kernel.Prelude.toString status)),
            contractStartDate = contractStartDate,
            contractEndDate = contractEndDate,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateEntity Domain.Types.CorporateEntity.CorporateEntity where
  toTType' (Domain.Types.CorporateEntity.CorporateEntity {..}) = do
    Beam.CorporateEntityT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.name = name,
        Beam.registeredName = registeredName,
        Beam.gstin = gstin,
        Beam.industry = industry,
        Beam.contactPersonName = contactPersonName,
        Beam.contactEmail = contactEmail,
        Beam.contactPhone = contactPhone,
        Beam.billingAddress = billingAddress,
        Beam.billingModel = Kernel.Prelude.show billingModel,
        Beam.billingCycleType = Kernel.Prelude.show billingCycleType,
        Beam.creditLimit = Kernel.Prelude.realToFrac creditLimit,
        Beam.currency = Kernel.Prelude.show currency,
        Beam.status = Kernel.Prelude.show status,
        Beam.contractStartDate = contractStartDate,
        Beam.contractEndDate = contractEndDate,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
