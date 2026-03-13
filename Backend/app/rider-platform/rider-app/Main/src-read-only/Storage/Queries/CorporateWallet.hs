{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateWallet (module Storage.Queries.CorporateWallet, module ReExport) where

import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporateWallet
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateWallet as Beam
import Storage.Queries.CorporateWalletExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateWallet ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateWallet.CorporateWallet -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateWallet.CorporateWallet] -> m ())
createMany = traverse_ create

findByCorporateEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m (Maybe Domain.Types.CorporateWallet.CorporateWallet))
findByCorporateEntityId corporateEntityId = do findOneWithKV [Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId)]

updateBalance ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateWallet.CorporateWallet -> m ())
updateBalance balance lastTopUpAt updatedAt id = do
  updateWithKV
    [ Se.Set Beam.balance (Kernel.Prelude.realToFrac balance),
      Se.Set Beam.lastTopUpAt lastTopUpAt,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.CorporateWallet.CorporateWalletStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateWallet.CorporateWallet -> m ())
updateStatus status updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status (Kernel.Prelude.show status),
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateWallet.CorporateWallet -> m (Maybe Domain.Types.CorporateWallet.CorporateWallet))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
