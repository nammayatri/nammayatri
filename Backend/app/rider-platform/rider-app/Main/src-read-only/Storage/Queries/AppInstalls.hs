{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AppInstalls (module Storage.Queries.AppInstalls, module ReExport) where

import qualified Domain.Types.AppInstalls
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.AppInstalls as Beam
import Storage.Queries.AppInstallsExtra as ReExport
import Storage.Queries.Transformers.AppInstalls

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppInstalls.AppInstalls -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AppInstalls.AppInstalls] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AppInstalls.AppInstalls -> m (Maybe Domain.Types.AppInstalls.AppInstalls))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppInstalls.AppInstalls -> m ())
updateByPrimaryKey (Domain.Types.AppInstalls.AppInstalls {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.appVersion (Kernel.Prelude.fmap Kernel.Utils.Version.versionToText appVersion),
      Se.Set Beam.bundleVersion (Kernel.Prelude.fmap Kernel.Utils.Version.versionToText bundleVersion),
      Se.Set Beam.deviceToken deviceToken,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.platform platform,
      Se.Set Beam.source source,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
