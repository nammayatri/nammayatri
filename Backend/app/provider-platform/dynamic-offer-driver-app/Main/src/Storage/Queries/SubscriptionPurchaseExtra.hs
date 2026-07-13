module Storage.Queries.SubscriptionPurchaseExtra where

import Data.List (partition, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Database.Beam as B
import Domain.Types.Extra.Plan (ServiceNames)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SubscriptionPurchase
import qualified Domain.Types.VehicleCategory as DVC
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as ITTDomain
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.SubscriptionPurchase as Beam
import Storage.Queries.OrphanInstances.SubscriptionPurchase ()

countActiveSubscriptionsForOwner ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  SubscriptionOwnerType ->
  m Int
countActiveSubscriptionsForOwner ownerId ownerType = do
  subs <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.ownerId $ Se.Eq ownerId,
            Se.Is Beam.ownerType $ Se.Eq ownerType,
            Se.Is Beam.status $ Se.Eq ACTIVE
          ]
      ]
  pure $ length subs

findActiveDistinctOwnersByOwnerIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Text] ->
  SubscriptionOwnerType ->
  m Int
findActiveDistinctOwnersByOwnerIds ownerIds ownerType = do
  if null ownerIds
    then pure 0
    else do
      subs <-
        findAllWithKV
          [Se.And [Se.Is Beam.ownerId $ Se.In ownerIds, Se.Is Beam.ownerType $ Se.Eq ownerType, Se.Is Beam.status $ Se.Eq ACTIVE]]
      pure $ Set.size $ Set.fromList $ map (.ownerId) subs

-- | Find all ACTIVE subscriptions for an owner, sorted by purchaseTimestamp ASC (FIFO order).
-- When mbVehicleCategory is Just, restricts results to that category only (wallet isolation).
findAllActiveByOwnerAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  SubscriptionOwnerType ->
  ServiceNames ->
  Maybe DVC.VehicleCategory ->
  m [SubscriptionPurchase]
findAllActiveByOwnerAndServiceName ownerId ownerType serviceName mbVehicleCategory =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.ownerId $ Se.Eq ownerId,
          Se.Is Beam.ownerType $ Se.Eq ownerType,
          Se.Is Beam.status $ Se.Eq ACTIVE,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
          <> [Se.Is Beam.vehicleCategory $ Se.Eq (Just vc) | Just vc <- [mbVehicleCategory]]
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
  Maybe DVC.VehicleCategory ->
  m (Maybe SubscriptionPurchase)
findLatestActiveByOwnerAndServiceName handleExpiry ownerId ownerType serviceName mbVehicleCategory = do
  now <- getCurrentTime
  allActive <- findAllActiveByOwnerAndServiceName ownerId ownerType serviceName mbVehicleCategory
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
  Maybe DVC.VehicleCategory ->
  Maybe Int ->
  Maybe Int ->
  m [SubscriptionPurchase]
findAllByOwnerAndServiceNameWithPagination ownerId ownerType serviceName mbStatus mbVehicleCategory limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is Beam.ownerId $ Se.Eq ownerId,
            Se.Is Beam.ownerType $ Se.Eq ownerType,
            Se.Is Beam.serviceName $ Se.Eq serviceName
          ]
            <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
            <> [Se.Is Beam.vehicleCategory $ Se.Eq (Just vc) | Just vc <- [mbVehicleCategory]]
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
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe Int ->
  Maybe Int ->
  m [SubscriptionPurchase]
findAllByOwnerAndServiceNameWithPagination' merchantOpCityId ownerId ownerType serviceName mbStatus mbAmountMin mbAmountMax limit offset =
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
            Se.Is Beam.ownerId $ Se.Eq ownerId,
            Se.Is Beam.ownerType $ Se.Eq ownerType,
            Se.Is Beam.serviceName $ Se.Eq serviceName
          ]
            <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
            <> [Se.Is Beam.planFee $ Se.GreaterThanOrEq amt | Just amt <- [mbAmountMin]]
            <> [Se.Is Beam.planFee $ Se.LessThanOrEq amt | Just amt <- [mbAmountMax]]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findAllByOwnerWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Text ->
  SubscriptionOwnerType ->
  Maybe SubscriptionPurchaseStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [SubscriptionPurchase]
findAllByOwnerWithFilters merchantOpCityId ownerId ownerType mbStatus mbFrom mbTo mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
          Se.Is Beam.ownerId $ Se.Eq ownerId,
          Se.Is Beam.ownerType $ Se.Eq ownerType
        ]
          <> [Se.Is Beam.status $ Se.Eq status | Just status <- [mbStatus]]
          <> [Se.Is Beam.purchaseTimestamp $ Se.GreaterThanOrEq fromTime | Just fromTime <- [mbFrom]]
          <> [Se.Is Beam.purchaseTimestamp $ Se.LessThanOrEq toTime | Just toTime <- [mbTo]]
    ]
    (Se.Desc Beam.purchaseTimestamp)
    mbLimit
    mbOffset

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

-- | Find subscription purchases by merchant operating city with optional filters (for finance dashboard).
-- When mbServiceName is Nothing, returns all service types. When mbFrom/mbTo are provided, filters by purchaseTimestamp.
-- When mbStatus is provided, filters by subscription status (PENDING, ACTIVE, EXPIRED, FAILED, EXHAUSTED).
findAllByMerchantOpCityIdWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe ServiceNames ->
  Maybe SubscriptionPurchaseStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe Int ->
  Maybe Int ->
  m [SubscriptionPurchase]
findAllByMerchantOpCityIdWithFilters merchantOpCityId mbServiceName mbStatus mbFrom mbTo mbAmountMin mbAmountMax mbLimit mbOffset = do
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId
        ]
          <> [Se.Is Beam.serviceName $ Se.Eq sn | Just sn <- [mbServiceName]]
          <> [Se.Is Beam.status $ Se.Eq st | Just st <- [mbStatus]]
          <> [Se.Is Beam.purchaseTimestamp $ Se.GreaterThanOrEq t | Just t <- [mbFrom]]
          <> [Se.Is Beam.purchaseTimestamp $ Se.LessThanOrEq t | Just t <- [mbTo]]
          <> [Se.Is Beam.planFee $ Se.GreaterThanOrEq amt | Just amt <- [mbAmountMin]]
          <> [Se.Is Beam.planFee $ Se.LessThanOrEq amt | Just amt <- [mbAmountMax]]
    ]
    (Se.Desc Beam.purchaseTimestamp)
    (Just limit)
    (Just offset)
  where
    maxLimit = 20
    defaultLimit = 10

-- | Paid subscription purchases (ACTIVE / EXPIRED / EXHAUSTED) whose purchaseTimestamp falls in the date range.
findPurchasedByDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m [SubscriptionPurchase]
findPurchasedByDateRange merchantOpCityId startTime endTime =
  sortBy (comparing (.purchaseTimestamp))
    <$> findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
            Se.Is Beam.status $ Se.In [ACTIVE, EXPIRED, EXHAUSTED],
            Se.Is Beam.purchaseTimestamp $ Se.GreaterThanOrEq startTime,
            Se.Is Beam.purchaseTimestamp $ Se.LessThanOrEq endTime
          ]
      ]

-- | Find ACTIVE subscription purchases for a merchant operating city whose purchaseTimestamp falls in the date range
findActiveByDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m [SubscriptionPurchase]
findActiveByDateRange merchantOpCityId startTime endTime =
  sortBy (comparing (.purchaseTimestamp))
    <$> findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId,
            Se.Is Beam.status $ Se.Eq ACTIVE,
            Se.Is Beam.purchaseTimestamp $ Se.GreaterThanOrEq startTime,
            Se.Is Beam.purchaseTimestamp $ Se.LessThanOrEq endTime
          ]
      ]

-- | Update the expiryDate and startDate for a specific subscription purchase.
-- Used when activating a queued purchase's expiry timer (deferred FIFO logic).
-- Called via SharedLogic.Finance.SubscriptionPurchase.activateSubscriptionPurchaseExpiry (audit)
updateExpiryAndStartDateById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe UTCTime ->
  Maybe UTCTime ->
  Finance.ActorInfo ->
  Id SubscriptionPurchase ->
  m ()
updateExpiryAndStartDateById newExpiryDate newStartDate actorInfo purchaseId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.expiryDate newExpiryDate,
      Se.Set Beam.startDate newStartDate,
      Se.Set Beam.updatedBy (Just actorInfo.actorType),
      Se.Set Beam.updatedById actorInfo.actorId,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId purchaseId)]

-- | Find a subscription purchase by its linked finance invoice ID.
-- Used to look up totalCredit (planRideCredit) for a SubscriptionPurchase invoice.
findByFinanceInvoiceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice ->
  m (Maybe SubscriptionPurchase)
findByFinanceInvoiceId invoiceId =
  findOneWithKV [Se.Is Beam.financeInvoiceId $ Se.Eq (Just (Kernel.Types.Id.getId invoiceId))]

findSubscriptionTotalsByDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m ((Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Int), [(Text, HighPrecMoney, HighPrecMoney, SubscriptionPurchaseStatus)])
findSubscriptionTotalsByDateRange merchantOpCityId startTime endTime = do
  dbConf <- getReplicaBeamConfig
  let filterQuery = B.filter_'
        ( \(sp, _) ->
            sp.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
              B.&&?. B.sqlBool_ (sp.status B./=. B.val_ PENDING)
              B.&&?. B.sqlBool_ (sp.status B./=. B.val_ FAILED)
              B.&&?. B.sqlBool_ (sp.purchaseTimestamp B.>=. B.val_ startTime)
              B.&&?. B.sqlBool_ (sp.purchaseTimestamp B.<=. B.val_ endTime)
        )
        do
          sp <- B.all_ (BeamCommon.subscriptionPurchase BeamCommon.atlasDB)
          itt <-
            B.join_
              (BeamCommon.indirectTaxTransaction BeamCommon.atlasDB)
              (\itt -> itt.referenceId B.==. sp.id B.&&. itt.transactionType B.==. B.val_ ITTDomain.Subscription)
          pure (sp, itt)
  aggRes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \(sp, itt) ->
                ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ sp.planFee,
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.cgstAmount,
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.sgstAmount,
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.igstAmount,
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ itt.taxableValue,
                  B.as_ @Int B.countAll_
                )
            )
            filterQuery
  let totals = case aggRes of
        Right [row] -> row
        _ -> (Nothing, Nothing, Nothing, Nothing, Nothing, 0)
  rowRes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          fmap
            ( \(sp, itt) ->
                ( sp.id,
                  sp.planFee,
                  itt.cgstAmount + itt.sgstAmount + itt.igstAmount + itt.taxableValue,
                  sp.status
                )
            )
            $ B.filter_'
              ( \(sp, _) ->
                  sp.merchantOperatingCityId B.==?. B.val_ merchantOpCityId.getId
                    B.&&?. B.sqlBool_ (sp.status B./=. B.val_ PENDING)
                    B.&&?. B.sqlBool_ (sp.status B./=. B.val_ FAILED)
                    B.&&?. B.sqlBool_ (sp.purchaseTimestamp B.>=. B.val_ startTime)
                    B.&&?. B.sqlBool_ (sp.purchaseTimestamp B.<=. B.val_ endTime)
              )
              do
                sp <- B.all_ (BeamCommon.subscriptionPurchase BeamCommon.atlasDB)
                itt <-
                  B.join_
                    (BeamCommon.indirectTaxTransaction BeamCommon.atlasDB)
                    (\itt -> itt.referenceId B.==. sp.id B.&&. itt.transactionType B.==. B.val_ ITTDomain.Subscription)
                pure (sp, itt)
  rows <- case rowRes of
    Right rs -> pure rs
    Left err -> do
      L.logError ("findSubscriptionTotalsByDateRange:rows" :: Text) $ "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure []
  case aggRes of
    Left err -> do
      L.logError ("findSubscriptionTotalsByDateRange" :: Text) $ "failed for mocId=" <> merchantOpCityId.getId <> " error=" <> show err
      pure (totals, [])
    _ -> pure (totals, rows)
