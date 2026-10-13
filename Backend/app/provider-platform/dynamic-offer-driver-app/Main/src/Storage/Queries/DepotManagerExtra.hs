module Storage.Queries.DepotManagerExtra where

import qualified Database.Beam as B
import qualified Domain.Types.DepotManager as DDepotManager
import Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, generateGUID, getCurrentTime, throwError)
import qualified Sequelize as Se
import Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DepotManager as Beam
import Storage.Queries.OrphanInstances.DepotManager ()

upsertLockTtlSeconds :: Int
upsertLockTtlSeconds = 10

mkUpsertLockKey :: Id DP.Person -> Maybe Text -> Text
mkUpsertLockKey personId depotId = "DepotManager:Upsert:" <> getId personId <> ":" <> fromMaybe "-" depotId

findAll ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Int ->
  Maybe Int ->
  m [DDepotManager.DepotManager]
findAll merchantId merchantOperatingCityId mLimit mOffset = do
  let limit = fromMaybe 20 mLimit
      offset = fromMaybe 0 mOffset
  findAllWithOptionsDb
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq $ getId merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq $ getId merchantOperatingCityId
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllByDepotCode ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  m [DDepotManager.DepotManager]
findAllByDepotCode merchantId merchantOperatingCityId depotId mLimit mOffset = do
  let limit = fromMaybe 20 mLimit
      offset = fromMaybe 0 mOffset
  findAllWithOptionsDb
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq $ getId merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq $ getId merchantOperatingCityId,
          Se.Is Beam.depotCode $ Se.Eq $ Just depotId
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

-- Redis lock on (personId, depotId) serialises the read-then-write window; prevents PK-violation races.
upsertDepotManagerDetail ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  m ()
upsertDepotManagerDetail merchant merchantOperatingCity personId depotId mbIsAdmin mbEnabled mbIsBlockAllowed = do
  Hedis.withLockRedis (mkUpsertLockKey personId depotId) upsertLockTtlSeconds $ do
    mbExistingDepotManager <-
      findOneWithDb
        [ Se.And
            [ Se.Is Beam.personId $ Se.Eq (getId personId),
              Se.Is Beam.depotCode $ Se.Eq depotId
            ]
        ]
    now <- getCurrentTime
    dbConf <- getMasterBeamConfig
    case mbExistingDepotManager of
      Just existing -> do
        -- The unique index on (personId, depotCode) is city-agnostic, so without this a caller in
        -- another city would silently move the row -- and its capability flags -- out of its owner.
        when (existing.merchantId /= merchant.id) $
          throwError $ InvalidRequest "Depot manager does not belong to this merchant"
        when (existing.merchantOperatingCityId /= merchantOperatingCity.id) $
          throwError $ InvalidRequest "Depot manager does not belong to this city"
        let isAdmin = fromMaybe existing.isAdmin mbIsAdmin
            -- Keep the stored value: re-enabling a revoked manager must be explicit.
            enabled = fromMaybe existing.enabled mbEnabled
            isBlockAllowed = maybe existing.isBlockAllowed Just mbIsBlockAllowed
        void $
          L.runDB dbConf $
            L.updateRows $
              B.update'
                (BeamCommon.depotManager BeamCommon.atlasDB)
                ( \row ->
                    mconcat
                      [ Beam.isAdmin row B.<-. B.val_ isAdmin,
                        Beam.enabled row B.<-. B.val_ enabled,
                        Beam.isBlockAllowed row B.<-. B.val_ isBlockAllowed,
                        Beam.updatedAt row B.<-. B.val_ now
                      ]
                )
                (\row -> Beam.id row B.==?. B.val_ (getId existing.id))
      Nothing -> do
        newId <- generateGUID
        let isAdmin = fromMaybe False mbIsAdmin
            enabled = fromMaybe True mbEnabled
        let newDepotManager =
              DDepotManager.DepotManager
                { DDepotManager.createdAt = now,
                  DDepotManager.depotCode = depotId,
                  DDepotManager.enabled = enabled,
                  DDepotManager.id = newId,
                  DDepotManager.isAdmin = isAdmin,
                  DDepotManager.isBlockAllowed = mbIsBlockAllowed,
                  DDepotManager.merchantId = merchant.id,
                  DDepotManager.merchantOperatingCityId = merchantOperatingCity.id,
                  DDepotManager.personId = personId,
                  DDepotManager.updatedAt = now
                }
        void $
          L.runDB dbConf $
            L.insertRows $
              B.insert (BeamCommon.depotManager BeamCommon.atlasDB) $
                B.insertValues [toTType' newDepotManager]

findLatestByPersonId ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Maybe DDepotManager.DepotManager)
findLatestByPersonId merchantId merchantOperatingCityId personId = do
  results <-
    findAllWithOptionsDb
      [ Se.And
          [ Se.Is Beam.personId $ Se.Eq (getId personId),
            Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
            Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)
          ]
      ]
      (Se.Desc Beam.updatedAt)
      (Just 1)
      Nothing
  pure $ listToMaybe results

-- Deterministic single-row picker for multi-depot persons: prefers admin, ties broken by latest updatedAt.
findBestForOperatorByPersonId ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  m (Maybe DDepotManager.DepotManager)
findBestForOperatorByPersonId personId = do
  results <-
    findAllWithOptionsDb
      [Se.Is Beam.personId $ Se.Eq (getId personId)]
      (Se.Desc Beam.updatedAt)
      Nothing
      Nothing
  pure $ pickOperatorScope results

-- | Disabled rows never win, and a row without a depot code can't serve as a dispatcher's scope,
-- so an admin only outranks when it actually carries one.
pickOperatorScope :: [DDepotManager.DepotManager] -> Maybe DDepotManager.DepotManager
pickOperatorScope results =
  let enabledRows = filter (.enabled) results
      scoped = filter (isJust . (.depotCode)) enabledRows
      admins = filter (.isAdmin) scoped
   in listToMaybe (admins <> scoped <> enabledRows)

-- | Scope row plus block permission from a single read. Block is ORed across every enabled row to
-- match requireBlockPermittedOperator, so /verify never reports a capability the API would refuse.
findOperatorScopeAndBlock ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  m (Maybe DDepotManager.DepotManager, Bool)
findOperatorScopeAndBlock personId = do
  results <-
    findAllWithOptionsDb
      [Se.Is Beam.personId $ Se.Eq (getId personId)]
      (Se.Desc Beam.updatedAt)
      Nothing
      Nothing
  pure (pickOperatorScope results, any (\dm -> dm.enabled && dm.isBlockAllowed == Just True) results)

deleteByPersonIdAndDepotCode ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Maybe Text ->
  Id DP.Person ->
  m ()
deleteByPersonIdAndDepotCode depotId personId = do
  Hedis.withLockRedis (mkUpsertLockKey personId depotId) upsertLockTtlSeconds $
    deleteWithDb
      [ Se.And
          [ Se.Is Beam.depotCode $ Se.Eq depotId,
            Se.Is Beam.personId $ Se.Eq (getId personId)
          ]
      ]
