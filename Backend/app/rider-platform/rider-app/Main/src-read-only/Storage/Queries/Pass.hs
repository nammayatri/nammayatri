{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Pass where

import qualified Data.Aeson
import qualified Data.String.Conversions
import qualified Domain.Types.Pass
import qualified Domain.Types.PassType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Pass as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Pass.Pass -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Pass.Pass] -> m ())
createMany = traverse_ create

findAllByTypeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PassType.PassType -> m [Domain.Types.Pass.Pass])
findAllByTypeId passTypeId = do findAllWithKV [Se.Is Beam.passTypeId $ Se.Eq (Kernel.Types.Id.getId passTypeId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Pass.Pass -> m (Maybe Domain.Types.Pass.Pass))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Pass.Pass -> m (Maybe Domain.Types.Pass.Pass))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Pass.Pass -> m ())
updateByPrimaryKey (Domain.Types.Pass.Pass {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.benefit benefit,
      Se.Set Beam.code code,
      Se.Set Beam.days days,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.order order,
      Se.Set Beam.passTypeId (Kernel.Types.Id.getId passTypeId),
      Se.Set Beam.purchaseEligibilityJsonLogic (Data.String.Conversions.cs . Data.Aeson.encode <$> purchaseEligibilityJsonLogic),
      Se.Set Beam.redeemEligibilityJsonLogic (Data.String.Conversions.cs . Data.Aeson.encode <$> redeemEligibilityJsonLogic),
      Se.Set Beam.savings savings,
      Se.Set Beam.trips trips,
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType,
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
            benefit = benefit,
            code = code,
            days = days,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            order = order,
            passTypeId = Kernel.Types.Id.Id passTypeId,
            purchaseEligibilityJsonLogic = Kernel.Prelude.fromMaybe Data.Aeson.Null . Data.Aeson.decode . Data.String.Conversions.cs <$> purchaseEligibilityJsonLogic,
            redeemEligibilityJsonLogic = Kernel.Prelude.fromMaybe Data.Aeson.Null . Data.Aeson.decode . Data.String.Conversions.cs <$> redeemEligibilityJsonLogic,
            savings = savings,
            trips = trips,
            vehicleServiceTierType = vehicleServiceTierType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Pass Domain.Types.Pass.Pass where
  toTType' (Domain.Types.Pass.Pass {..}) = do
    Beam.PassT
      { Beam.amount = amount,
        Beam.benefit = benefit,
        Beam.code = code,
        Beam.days = days,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.order = order,
        Beam.passTypeId = Kernel.Types.Id.getId passTypeId,
        Beam.purchaseEligibilityJsonLogic = Data.String.Conversions.cs . Data.Aeson.encode <$> purchaseEligibilityJsonLogic,
        Beam.redeemEligibilityJsonLogic = Data.String.Conversions.cs . Data.Aeson.encode <$> redeemEligibilityJsonLogic,
        Beam.savings = savings,
        Beam.trips = trips,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
