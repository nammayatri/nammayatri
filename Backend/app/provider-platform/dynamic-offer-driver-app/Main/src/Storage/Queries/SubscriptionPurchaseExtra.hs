module Storage.Queries.SubscriptionPurchaseExtra where

import Data.List (partition)
import Domain.Types.Extra.Plan (ServiceNames)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SubscriptionPurchase
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id (Id)
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
  pure $ listToMaybe $ reverse valid
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

-- | Find subscriptions by owner with merchant operating city filter (for dashboard APIs)
findAllByOwnerAndServiceNameWithPagination' ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  SubscriptionOwnerType ->
  ServiceNames ->
  Maybe SubscriptionPurchaseStatus ->
  Maybe Int ->
  Maybe Int ->
  m [SubscriptionPurchase]
findAllByOwnerAndServiceNameWithPagination' merchantOpCityId ownerId ownerType serviceName mbStatus limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
            Se.Is Beam.ownerId $ Se.Eq ownerId,
            Se.Is Beam.ownerType $ Se.Eq ownerType,
            Se.Is Beam.serviceName $ Se.Eq serviceName
          ]
            <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

-- | Find subscription purchases by merchant operating city and service name (for dashboard list-all)
findAllByMerchantOpCityIdAndServiceNameWithPagination ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  ServiceNames ->
  Maybe Int ->
  Maybe Int ->
  m [SubscriptionPurchase]
findAllByMerchantOpCityIdAndServiceNameWithPagination merchantOpCityId serviceName limit offset =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

-- | Find ACTIVE subscription purchases for a merchant operating city whose purchaseTimestamp falls in the date range
findActiveByDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m [SubscriptionPurchase]
findActiveByDateRange merchantOpCityId startTime endTime =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
          Se.Is Beam.status $ Se.Eq ACTIVE,
          Se.Is Beam.purchaseTimestamp $ Se.GreaterThanOrEq startTime,
          Se.Is Beam.purchaseTimestamp $ Se.LessThanOrEq endTime
        ]
    ]
