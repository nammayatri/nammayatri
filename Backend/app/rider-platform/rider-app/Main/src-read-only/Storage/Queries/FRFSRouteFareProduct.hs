{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FRFSRouteFareProduct where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FRFSRouteFareProduct
import qualified Storage.Beam.FRFSRouteFareProduct as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.IntegratedBPPConfig
import qualified Kernel.Prelude
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct] -> m ())
createMany = traverse_ create
findByRouteCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                   (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ([Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct]))
findByRouteCode routeCode integratedBppConfigId = do findAllWithKV [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode,
                                                                            Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)]]
findByRouteCodeAndVehicleServiceTierId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                          (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m (Maybe Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct))
findByRouteCodeAndVehicleServiceTierId routeCode vehicleServiceTierId = do findOneWithKV [Se.And [Se.Is Beam.routeCode $ Se.Eq routeCode,
                                                                                                  Se.Is Beam.vehicleServiceTierId $ Se.Eq (Kernel.Types.Id.getId vehicleServiceTierId)]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct -> m (Maybe Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct -> m ())
updateByPrimaryKey (Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct {..}) = do {_now <- getCurrentTime;
                                                                                       updateWithKV [Se.Set Beam.farePolicyId (Kernel.Types.Id.getId farePolicyId),
                                                                                                     Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
                                                                                                     Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                                     Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                     Se.Set Beam.routeCode routeCode,
                                                                                                     Se.Set Beam.timeBounds timeBounds,
                                                                                                     Se.Set Beam.vehicleServiceTierId (Kernel.Types.Id.getId vehicleServiceTierId),
                                                                                                     Se.Set Beam.vehicleType vehicleType,
                                                                                                     Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.FRFSRouteFareProduct Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct
    where fromTType' (Beam.FRFSRouteFareProductT {..}) = do pure $ Just Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct{farePolicyId = Kernel.Types.Id.Id farePolicyId,
                                                                                                                               id = Kernel.Types.Id.Id id,
                                                                                                                               integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
                                                                                                                               merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                               merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                               routeCode = routeCode,
                                                                                                                               timeBounds = timeBounds,
                                                                                                                               vehicleServiceTierId = Kernel.Types.Id.Id vehicleServiceTierId,
                                                                                                                               vehicleType = vehicleType,
                                                                                                                               createdAt = createdAt,
                                                                                                                               updatedAt = updatedAt}
instance ToTType' Beam.FRFSRouteFareProduct Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct
    where toTType' (Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct {..}) = do Beam.FRFSRouteFareProductT{Beam.farePolicyId = Kernel.Types.Id.getId farePolicyId,
                                                                                                                 Beam.id = Kernel.Types.Id.getId id,
                                                                                                                 Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
                                                                                                                 Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                 Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                 Beam.routeCode = routeCode,
                                                                                                                 Beam.timeBounds = timeBounds,
                                                                                                                 Beam.vehicleServiceTierId = Kernel.Types.Id.getId vehicleServiceTierId,
                                                                                                                 Beam.vehicleType = vehicleType,
                                                                                                                 Beam.createdAt = createdAt,
                                                                                                                 Beam.updatedAt = updatedAt}



