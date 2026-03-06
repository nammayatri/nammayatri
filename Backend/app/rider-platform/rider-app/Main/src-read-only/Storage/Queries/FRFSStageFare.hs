{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSStageFare where

import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.FRFSStageFare
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSStageFare as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSStageFare.FRFSStageFare -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSStageFare.FRFSStageFare] -> m ())
createMany = traverse_ create

findAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> m ([Domain.Types.FRFSStageFare.FRFSStageFare]))
findAllByFarePolicyId farePolicyId = do findAllWithKV [Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> Kernel.Prelude.Int -> m (Maybe Domain.Types.FRFSStageFare.FRFSStageFare))
findByPrimaryKey farePolicyId stage = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId), Se.Is Beam.stage $ Se.Eq stage]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSStageFare.FRFSStageFare -> m ())
updateByPrimaryKey (Domain.Types.FRFSStageFare.FRFSStageFare {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.currency currency,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId), Se.Is Beam.stage $ Se.Eq stage]]

instance FromTType' Beam.FRFSStageFare Domain.Types.FRFSStageFare.FRFSStageFare where
  fromTType' (Beam.FRFSStageFareT {..}) = do
    pure $
      Just
        Domain.Types.FRFSStageFare.FRFSStageFare
          { amount = amount,
            currency = currency,
            farePolicyId = Kernel.Types.Id.Id farePolicyId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            stage = stage,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSStageFare Domain.Types.FRFSStageFare.FRFSStageFare where
  toTType' (Domain.Types.FRFSStageFare.FRFSStageFare {..}) = do
    Beam.FRFSStageFareT
      { Beam.amount = amount,
        Beam.currency = currency,
        Beam.farePolicyId = Kernel.Types.Id.getId farePolicyId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.stage = stage,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
