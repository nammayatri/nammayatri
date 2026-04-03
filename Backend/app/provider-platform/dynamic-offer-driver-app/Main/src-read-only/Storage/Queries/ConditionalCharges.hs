{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.ConditionalCharges where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.ConditionalCharges
import qualified Storage.Beam.ConditionalCharges as Beam
import qualified Domain.Types.Extra.ConditionalCharges
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ConditionalCharges.ConditionalCharges -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ConditionalCharges.ConditionalCharges] -> m ())
createMany = traverse_ create
findAllByFp :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m ([Domain.Types.ConditionalCharges.ConditionalCharges]))
findAllByFp farePolicyId = do findAllWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Domain.Types.Extra.ConditionalCharges.ConditionalChargesCategories -> Data.Text.Text -> m (Maybe Domain.Types.ConditionalCharges.ConditionalCharges))
findByPrimaryKey chargeCategory farePolicyId = do findOneWithKV [Se.And [Se.Is Beam.chargeCategory $ Se.Eq chargeCategory, Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ConditionalCharges.ConditionalCharges -> m ())
updateByPrimaryKey (Domain.Types.ConditionalCharges.ConditionalCharges {..}) = do {_now <- getCurrentTime;
                                                                                   updateWithKV [Se.Set Beam.cgstPercentage (Kernel.Prelude.Just cgstPercentage),
                                                                                                 Se.Set Beam.charge charge,
                                                                                                 Se.Set Beam.sgstPercentage (Kernel.Prelude.Just sgstPercentage),
                                                                                                 Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.chargeCategory $ Se.Eq chargeCategory, Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]}



instance FromTType' Beam.ConditionalCharges Domain.Types.ConditionalCharges.ConditionalCharges
    where fromTType' (Beam.ConditionalChargesT {..}) = do pure $ Just Domain.Types.ConditionalCharges.ConditionalCharges{cgstPercentage = Kernel.Prelude.fromMaybe 0 cgstPercentage,
                                                                                                                         charge = charge,
                                                                                                                         chargeCategory = chargeCategory,
                                                                                                                         farePolicyId = farePolicyId,
                                                                                                                         sgstPercentage = Kernel.Prelude.fromMaybe 0 sgstPercentage,
                                                                                                                         createdAt = createdAt,
                                                                                                                         updatedAt = updatedAt}
instance ToTType' Beam.ConditionalCharges Domain.Types.ConditionalCharges.ConditionalCharges
    where toTType' (Domain.Types.ConditionalCharges.ConditionalCharges {..}) = do Beam.ConditionalChargesT{Beam.cgstPercentage = Kernel.Prelude.Just cgstPercentage,
                                                                                                           Beam.charge = charge,
                                                                                                           Beam.chargeCategory = chargeCategory,
                                                                                                           Beam.farePolicyId = farePolicyId,
                                                                                                           Beam.sgstPercentage = Kernel.Prelude.Just sgstPercentage,
                                                                                                           Beam.createdAt = createdAt,
                                                                                                           Beam.updatedAt = updatedAt}



