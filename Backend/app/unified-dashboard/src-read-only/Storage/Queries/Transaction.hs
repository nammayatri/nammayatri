{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transaction (module Storage.Queries.Transaction, module ReExport) where

import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Transaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Transaction as Beam
import Storage.Queries.TransactionExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Transaction.Transaction -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Transaction.Transaction] -> m ())
createMany = traverse_ create

fetchLastTransaction ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Domain.Types.AccessMatrix.UserActionType -> Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName -> m ([Domain.Types.Transaction.Transaction]))
fetchLastTransaction limit offset endpoint serverName = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.endpoint $ Se.Eq endpoint,
          Se.Is Beam.serverName $ Se.Eq serverName
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Transaction.Transaction -> m (Maybe Domain.Types.Transaction.Transaction))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Transaction.Transaction -> m (Maybe Domain.Types.Transaction.Transaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Transaction.Transaction -> m ())
updateByPrimaryKey (Domain.Types.Transaction.Transaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.commonDriverId commonDriverId,
      Se.Set Beam.commonRideId commonRideId,
      Se.Set Beam.endpoint endpoint,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.request request,
      Se.Set Beam.requestorId (Kernel.Types.Id.getId <$> requestorId),
      Se.Set Beam.response response,
      Se.Set Beam.responseError responseError,
      Se.Set Beam.serverName serverName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
