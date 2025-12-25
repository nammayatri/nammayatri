{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ConditionalCharges where

import qualified Data.Text
import qualified Domain.Types.ConditionalCharges
import qualified Domain.Types.Extra.ConditionalCharges
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ConditionalCharges as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ConditionalCharges.ConditionalCharges -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ConditionalCharges.ConditionalCharges] -> m ())
createMany = traverse_ create

findAllByFp :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m [Domain.Types.ConditionalCharges.ConditionalCharges])
findAllByFp farePolicyId = do findAllWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Extra.ConditionalCharges.ConditionalChargesCategories -> Data.Text.Text -> m (Maybe Domain.Types.ConditionalCharges.ConditionalCharges))
findByPrimaryKey chargeCategory farePolicyId = do findOneWithKV [Se.And [Se.Is Beam.chargeCategory $ Se.Eq chargeCategory, Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ConditionalCharges.ConditionalCharges -> m ())
updateByPrimaryKey (Domain.Types.ConditionalCharges.ConditionalCharges {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cgstPercentage (Kernel.Prelude.Just cgstPercentage),
      Se.Set Beam.charge charge,
      Se.Set Beam.sgstPercentage (Kernel.Prelude.Just sgstPercentage),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.chargeCategory $ Se.Eq chargeCategory, Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

instance FromTType' Beam.ConditionalCharges Domain.Types.ConditionalCharges.ConditionalCharges where
  fromTType' (Beam.ConditionalChargesT {..}) = do
    pure $
      Just
        Domain.Types.ConditionalCharges.ConditionalCharges
          { cgstPercentage = Kernel.Prelude.fromMaybe 0 cgstPercentage,
            charge = charge,
            chargeCategory = chargeCategory,
            farePolicyId = farePolicyId,
            sgstPercentage = Kernel.Prelude.fromMaybe 0 sgstPercentage,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ConditionalCharges Domain.Types.ConditionalCharges.ConditionalCharges where
  toTType' (Domain.Types.ConditionalCharges.ConditionalCharges {..}) = do
    Beam.ConditionalChargesT
      { Beam.cgstPercentage = Kernel.Prelude.Just cgstPercentage,
        Beam.charge = charge,
        Beam.chargeCategory = chargeCategory,
        Beam.farePolicyId = farePolicyId,
        Beam.sgstPercentage = Kernel.Prelude.Just sgstPercentage,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
