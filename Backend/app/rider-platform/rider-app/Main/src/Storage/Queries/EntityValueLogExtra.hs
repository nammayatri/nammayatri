module Storage.Queries.EntityValueLogExtra where

import "this" Domain.Types.EntityValueLog
import Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.EntityValueLog as Beam
import Storage.Queries.OrphanInstances.EntityValueLog ()

findAllByEntityTypeAndOperationTypeInCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  OperationType ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Int ->
  Int ->
  m [EntityValueLog]
findAllByEntityTypeAndOperationTypeInCity entityType operationType merchantOperatingCityId fromTime toTime limit offset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.operationType $ Se.Eq operationType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromTime,
          Se.Is Beam.createdAt $ Se.LessThan toTime
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllByEntityTypeAndEntityIdPaged ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Int ->
  Int ->
  m [EntityValueLog]
findAllByEntityTypeAndEntityIdPaged entityType entityId limit offset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.entityId $ Se.Eq entityId
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findPendingByEntityTypeAndEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Text ->
  Text ->
  OperationType ->
  m [EntityValueLog]
findPendingByEntityTypeAndEntityId merchantOperatingCityId entityType entityId operationType =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.operationType $ Se.Eq operationType,
          Se.Is Beam.status $ Se.Eq PENDING
        ]
    ]
