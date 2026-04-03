{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.OperationHub (module Storage.Queries.OperationHub, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.OperationHubExtra as ReExport
import qualified Domain.Types.OperationHub
import qualified Storage.Beam.OperationHub as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperationHub.OperationHub -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.OperationHub.OperationHub] -> m ())
createMany = traverse_ create
findAllByCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.OperationHub.OperationHub]))
findAllByCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub -> m (Maybe Domain.Types.OperationHub.OperationHub))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperationHub.OperationHub -> m ())
updateByPrimaryKey (Domain.Types.OperationHub.OperationHub {..}) = do {_now <- getCurrentTime;
                                                                       updateWithKV [Se.Set Beam.address address,
                                                                                     Se.Set Beam.description description,
                                                                                     Se.Set Beam.lat lat,
                                                                                     Se.Set Beam.lon lon,
                                                                                     Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                     Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                     Se.Set Beam.mobileNumber mobileNumber,
                                                                                     Se.Set Beam.name name,
                                                                                     Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



