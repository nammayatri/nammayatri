module Storage.Queries.DepotManagerExtra where

import qualified Database.Beam as B
import qualified Domain.Types.Depot as DDepot
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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime, throwError)
import qualified Sequelize as Se
import Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DepotManager as Beam
import qualified Storage.Queries.Depot as QDepot
import Storage.Queries.OrphanInstances.DepotManager ()

upsertLockTtlSeconds :: Int
upsertLockTtlSeconds = 10

mkUpsertLockKey :: Id DP.Person -> Id DDepot.Depot -> Text
mkUpsertLockKey personId depotId = "DepotManager:Upsert:" <> getId personId <> ":" <> getId depotId

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
  Id DDepot.Depot ->
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
          Se.Is Beam.depotCode $ Se.Eq $ getId depotId
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
  Id DDepot.Depot ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  m ()
upsertDepotManagerDetail merchant merchantOperatingCity personId depotId mbIsAdmin mbEnabled mbIsBlockAllowed = do
  depot <-
    QDepot.findByPrimaryKey depotId
      >>= fromMaybeM (InvalidRequest $ "Depot not found: " <> getId depotId)
  when (depot.merchantId /= merchant.id) $
    throwError (InvalidRequest "Depot does not belong to this merchant")
  when (depot.merchantOperatingCityId /= merchantOperatingCity.id) $
    throwError (InvalidRequest "Depot does not belong to this city")
  Hedis.withLockRedis (mkUpsertLockKey personId depotId) upsertLockTtlSeconds $ do
    mbExistingDepotManager <-
      findOneWithDb
        [ Se.And
            [ Se.Is Beam.personId $ Se.Eq (getId personId),
              Se.Is Beam.depotCode $ Se.Eq (getId depotId)
            ]
        ]
    now <- getCurrentTime
    dbConf <- getMasterBeamConfig
    case mbExistingDepotManager of
      Just existing -> do
        let isAdmin = fromMaybe existing.isAdmin mbIsAdmin
            -- Re-enable by default so a disabled row can be restored via upsert.
            enabled = fromMaybe True mbEnabled
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
                        Beam.merchantId row B.<-. B.val_ (getId merchant.id),
                        Beam.merchantOperatingCityId row B.<-. B.val_ (getId merchantOperatingCity.id),
                        Beam.updatedAt row B.<-. B.val_ now
                      ]
                )
                ( \row ->
                    Beam.personId row B.==?. B.val_ (getId personId)
                      B.&&?. Beam.depotCode row B.==?. B.val_ (getId depotId)
                )
      Nothing -> do
        let isAdmin = fromMaybe False mbIsAdmin
            enabled = fromMaybe True mbEnabled
        let newDepotManager =
              DDepotManager.DepotManager
                { DDepotManager.createdAt = now,
                  DDepotManager.depotCode = depotId,
                  DDepotManager.enabled = enabled,
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
  let admins = filter (.isAdmin) results
  pure $ maybe (listToMaybe results) Just (listToMaybe admins)

deleteByPrimaryKey ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DDepot.Depot ->
  Id DP.Person ->
  m ()
deleteByPrimaryKey depotId personId = do
  Hedis.withLockRedis (mkUpsertLockKey personId depotId) upsertLockTtlSeconds $
    deleteWithDb
      [ Se.And
          [ Se.Is Beam.depotCode $ Se.Eq (getId depotId),
            Se.Is Beam.personId $ Se.Eq (getId personId)
          ]
      ]
