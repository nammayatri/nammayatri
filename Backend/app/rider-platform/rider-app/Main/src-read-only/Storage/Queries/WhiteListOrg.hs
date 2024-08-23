{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WhiteListOrg (module Storage.Queries.WhiteListOrg, module ReExport) where

import qualified Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as Beam
import Storage.Queries.Transformers.WhiteListOrg
import Storage.Queries.WhiteListOrgExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WhiteListOrg.WhiteListOrg -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.WhiteListOrg.WhiteListOrg] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.WhiteListOrg.WhiteListOrg -> m (Maybe Domain.Types.WhiteListOrg.WhiteListOrg))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WhiteListOrg.WhiteListOrg -> m ())
updateByPrimaryKey (Domain.Types.WhiteListOrg.WhiteListOrg {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt (Kernel.Prelude.Just createdAt),
      Se.Set Beam.domain domain,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.subscriberId (Kernel.Types.Id.getShortId subscriberId),
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
