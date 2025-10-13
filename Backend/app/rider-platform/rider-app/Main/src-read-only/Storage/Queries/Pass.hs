{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Pass where

import qualified Domain.Types.Pass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Pass as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Pass.Pass -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Pass.Pass] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Pass.Pass -> m (Maybe Domain.Types.Pass.Pass))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Pass.Pass -> m ())
updateByPrimaryKey (Domain.Types.Pass.Pass {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.applicableVehicleServiceTiers (Kernel.Types.Id.getId <$> applicableVehicleServiceTiers),
      Se.Set Beam.autoApply autoApply,
      Se.Set Beam.benefit benefit,
      Se.Set Beam.benefitDescription benefitDescription,
      Se.Set Beam.code code,
      Se.Set Beam.documentsRequired documentsRequired,
      Se.Set Beam.enable enable,
      Se.Set Beam.maxValidDays maxValidDays,
      Se.Set Beam.maxValidTrips maxValidTrips,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.order order,
      Se.Set Beam.passTypeId (Kernel.Types.Id.getId passTypeId),
      Se.Set Beam.purchaseEligibilityJsonLogic purchaseEligibilityJsonLogic,
      Se.Set Beam.redeemEligibilityJsonLogic redeemEligibilityJsonLogic,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Pass Domain.Types.Pass.Pass where
  fromTType' (Beam.PassT {..}) = do
    pure $
      Just
        Domain.Types.Pass.Pass
          { amount = amount,
            applicableVehicleServiceTiers = Kernel.Types.Id.Id <$> applicableVehicleServiceTiers,
            autoApply = autoApply,
            benefit = benefit,
            benefitDescription = benefitDescription,
            code = code,
            documentsRequired = documentsRequired,
            enable = enable,
            id = Kernel.Types.Id.Id id,
            maxValidDays = maxValidDays,
            maxValidTrips = maxValidTrips,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            order = order,
            passTypeId = Kernel.Types.Id.Id passTypeId,
            purchaseEligibilityJsonLogic = purchaseEligibilityJsonLogic,
            redeemEligibilityJsonLogic = redeemEligibilityJsonLogic,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Pass Domain.Types.Pass.Pass where
  toTType' (Domain.Types.Pass.Pass {..}) = do
    Beam.PassT
      { Beam.amount = amount,
        Beam.applicableVehicleServiceTiers = Kernel.Types.Id.getId <$> applicableVehicleServiceTiers,
        Beam.autoApply = autoApply,
        Beam.benefit = benefit,
        Beam.benefitDescription = benefitDescription,
        Beam.code = code,
        Beam.documentsRequired = documentsRequired,
        Beam.enable = enable,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxValidDays = maxValidDays,
        Beam.maxValidTrips = maxValidTrips,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.order = order,
        Beam.passTypeId = Kernel.Types.Id.getId passTypeId,
        Beam.purchaseEligibilityJsonLogic = purchaseEligibilityJsonLogic,
        Beam.redeemEligibilityJsonLogic = redeemEligibilityJsonLogic,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
