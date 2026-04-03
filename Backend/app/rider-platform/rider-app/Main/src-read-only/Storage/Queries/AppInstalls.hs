{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.AppInstalls (module Storage.Queries.AppInstalls, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.AppInstallsExtra as ReExport
import Storage.Queries.Transformers.AppInstalls
import qualified Domain.Types.AppInstalls
import qualified Storage.Beam.AppInstalls as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.Version
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppInstalls.AppInstalls -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AppInstalls.AppInstalls] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AppInstalls.AppInstalls -> m (Maybe Domain.Types.AppInstalls.AppInstalls))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppInstalls.AppInstalls -> m ())
updateByPrimaryKey (Domain.Types.AppInstalls.AppInstalls {..}) = do {_now <- getCurrentTime;
                                                                     updateWithKV [Se.Set Beam.appVersion (Kernel.Prelude.fmap Kernel.Utils.Version.versionToText appVersion),
                                                                                   Se.Set Beam.bundleVersion (Kernel.Prelude.fmap Kernel.Utils.Version.versionToText bundleVersion),
                                                                                   Se.Set Beam.deviceToken deviceToken,
                                                                                   Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                   Se.Set Beam.platform platform,
                                                                                   Se.Set Beam.source source,
                                                                                   Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



