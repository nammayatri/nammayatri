{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Insurance where

import qualified Domain.Types.Insurance
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Insurance as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Insurance.Insurance -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Insurance.Insurance] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Insurance.Insurance -> m (Maybe Domain.Types.Insurance.Insurance))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Insurance.Insurance -> m (Maybe Domain.Types.Insurance.Insurance))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Insurance.Insurance -> m ())
updateByPrimaryKey (Domain.Types.Insurance.Insurance {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.category category,
      Se.Set Beam.certificateUrl certificateUrl,
      Se.Set Beam.customerId (Kernel.Types.Id.getId customerId),
      Se.Set Beam.customerName customerName,
      Se.Set Beam.customerPhone customerPhone,
      Se.Set Beam.driverInsuredAmount driverInsuredAmount,
      Se.Set Beam.driverName driverName,
      Se.Set Beam.driverPhone driverPhone,
      Se.Set Beam.endDate endDate,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.fairBreakup fairBreakup,
      Se.Set Beam.insuredAmount insuredAmount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.partnerId partnerId,
      Se.Set Beam.plan plan,
      Se.Set Beam.policyId policyId,
      Se.Set Beam.policyNumber policyNumber,
      Se.Set Beam.startDate startDate,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Insurance Domain.Types.Insurance.Insurance where
  fromTType' (Beam.InsuranceT {..}) = do
    pure $
      Just
        Domain.Types.Insurance.Insurance
          { category = category,
            certificateUrl = certificateUrl,
            createdAt = createdAt,
            customerId = Kernel.Types.Id.Id customerId,
            customerName = customerName,
            customerPhone = customerPhone,
            driverInsuredAmount = driverInsuredAmount,
            driverName = driverName,
            driverPhone = driverPhone,
            endDate = endDate,
            entityType = entityType,
            fairBreakup = fairBreakup,
            id = Kernel.Types.Id.Id id,
            insuredAmount = insuredAmount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerId = partnerId,
            plan = plan,
            policyId = policyId,
            policyNumber = policyNumber,
            startDate = startDate,
            tripCategory = tripCategory,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Insurance Domain.Types.Insurance.Insurance where
  toTType' (Domain.Types.Insurance.Insurance {..}) = do
    Beam.InsuranceT
      { Beam.category = category,
        Beam.certificateUrl = certificateUrl,
        Beam.createdAt = createdAt,
        Beam.customerId = Kernel.Types.Id.getId customerId,
        Beam.customerName = customerName,
        Beam.customerPhone = customerPhone,
        Beam.driverInsuredAmount = driverInsuredAmount,
        Beam.driverName = driverName,
        Beam.driverPhone = driverPhone,
        Beam.endDate = endDate,
        Beam.entityType = entityType,
        Beam.fairBreakup = fairBreakup,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuredAmount = insuredAmount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerId = partnerId,
        Beam.plan = plan,
        Beam.policyId = policyId,
        Beam.policyNumber = policyNumber,
        Beam.startDate = startDate,
        Beam.tripCategory = tripCategory,
        Beam.updatedAt = updatedAt
      }
