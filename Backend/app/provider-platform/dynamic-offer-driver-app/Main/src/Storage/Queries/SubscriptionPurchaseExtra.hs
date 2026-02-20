module Storage.Queries.SubscriptionPurchaseExtra where

import Control.Lens ((^?), _head)
import Data.List (partition)
import Domain.Types.Extra.Plan (ServiceNames)
import Domain.Types.SubscriptionPurchase
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SubscriptionPurchase as Beam
import Storage.Queries.OrphanInstances.SubscriptionPurchase ()

-- | Find all ACTIVE subscriptions for an owner, sorted by purchaseTimestamp ASC (FIFO order)
findAllActiveByOwnerAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  SubscriptionOwnerType ->
  ServiceNames ->
  m [SubscriptionPurchase]
findAllActiveByOwnerAndServiceName ownerId ownerType serviceName =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.ownerId $ Se.Eq ownerId,
          Se.Is Beam.ownerType $ Se.Eq ownerType,
          Se.Is Beam.status $ Se.Eq ACTIVE,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]
    (Se.Asc Beam.purchaseTimestamp)
    Nothing
    Nothing

-- | Find the latest active, non-expired subscription.
-- Using the provided expiry handler callback to process any expired subscriptions found.
findLatestActiveByOwnerAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (SubscriptionPurchase -> m ()) ->
  Text ->
  SubscriptionOwnerType ->
  ServiceNames ->
  m (Maybe SubscriptionPurchase)
findLatestActiveByOwnerAndServiceName handleExpiry ownerId ownerType serviceName = do
  now <- getCurrentTime
  allActive <- findAllActiveByOwnerAndServiceName ownerId ownerType serviceName
  -- Partition into expired and still-valid
  let (expired, valid) = partition (isExpired now) allActive
  -- Handle expired subscriptions (fallback for failed scheduler jobs)
  mapM_ handleExpiry expired
  -- Return latest non-expired (last in ASC-sorted list)
  pure $ reverse valid ^? _head
  where
    isExpired now purchase = case purchase.expiryDate of
      Just expiry -> expiry <= now
      Nothing -> False

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
