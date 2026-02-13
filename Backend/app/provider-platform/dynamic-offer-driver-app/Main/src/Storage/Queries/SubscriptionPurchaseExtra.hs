module Storage.Queries.SubscriptionPurchaseExtra where

import Domain.Types.Extra.Plan (ServiceNames)
import Domain.Types.SubscriptionPurchase
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import qualified Sequelize as Se
import Kernel.Types.CacheFlow
import qualified Storage.Beam.SubscriptionPurchase as Beam
import Storage.Queries.OrphanInstances.SubscriptionPurchase ()

findLatestActiveByOwnerAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  SubscriptionOwnerType ->
  ServiceNames ->
  m (Maybe SubscriptionPurchase)
findLatestActiveByOwnerAndServiceName ownerId ownerType serviceName =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.ownerId $ Se.Eq ownerId,
          Se.Is Beam.ownerType $ Se.Eq ownerType,
          Se.Is Beam.status $ Se.Eq ACTIVE,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]
    (Se.Desc Beam.purchaseTimestamp)
    (Just 1)
    Nothing
    <&> listToMaybe

findAllByOwnerAndServiceNameWithPagination ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  SubscriptionOwnerType ->
  ServiceNames ->
  Maybe SubscriptionPurchaseStatus ->
  Maybe Int ->
  Maybe Int ->
  m [SubscriptionPurchase]
findAllByOwnerAndServiceNameWithPagination ownerId ownerType serviceName mbStatus limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is Beam.ownerId $ Se.Eq ownerId,
            Se.Is Beam.ownerType $ Se.Eq ownerType,
            Se.Is Beam.serviceName $ Se.Eq serviceName
          ]
            <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
