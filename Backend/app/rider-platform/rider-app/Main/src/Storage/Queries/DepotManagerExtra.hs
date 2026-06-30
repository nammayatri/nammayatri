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
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DepotManager as Beam
import Storage.Queries.OrphanInstances.DepotManager ()

findAll ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  m [DDepotManager.DepotManager]
findAll merchantId mLimit mOffset = do
  let limit = fromMaybe 20 mLimit
      offset = fromMaybe 0 mOffset
  findAllWithOptionsDb
    [Se.Is Beam.merchantId $ Se.Eq $ getId merchantId]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllByDepotCode ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DDepot.Depot ->
  Maybe Int ->
  Maybe Int ->
  m [DDepotManager.DepotManager]
findAllByDepotCode merchantId depotId mLimit mOffset = do
  let limit = fromMaybe 20 mLimit
      offset = fromMaybe 0 mOffset
  findAllWithOptionsDb
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq $ getId merchantId,
          Se.Is Beam.depotCode $ Se.Eq $ getId depotId
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

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
  mbExistingDepotManager <- findOneWithDb [Se.Is Beam.personId $ Se.Eq (getId personId)]
  now <- getCurrentTime
  dbConf <- getMasterBeamConfig
  case mbExistingDepotManager of
    Just existing -> do
      let isAdmin = fromMaybe existing.isAdmin mbIsAdmin
          enabled = fromMaybe existing.enabled mbEnabled
          isBlockAllowed = maybe existing.isBlockAllowed Just mbIsBlockAllowed
      void $
        L.runDB dbConf $
          L.updateRows $
            B.update'
              (BeamCommon.depotManager BeamCommon.atlasDB)
              ( \row ->
                  mconcat
                    [ Beam.depotCode row B.<-. B.val_ (getId depotId),
                      Beam.isAdmin row B.<-. B.val_ isAdmin,
                      Beam.enabled row B.<-. B.val_ enabled,
                      Beam.isBlockAllowed row B.<-. B.val_ isBlockAllowed,
                      Beam.updatedAt row B.<-. B.val_ now
                    ]
              )
              (\row -> Beam.personId row B.==?. B.val_ (getId personId))
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
  Id DP.Person ->
  m (Maybe DDepotManager.DepotManager)
findLatestByPersonId personId = do
  results <-
    findAllWithOptionsDb
      [Se.Is Beam.personId $ Se.Eq (getId personId)]
      (Se.Desc Beam.updatedAt)
      (Just 1)
      Nothing
  pure $ listToMaybe results

deleteByPrimaryKey ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DDepot.Depot ->
  Id DP.Person ->
  m ()
deleteByPrimaryKey depotId personId = do
  deleteWithDb
    [ Se.And
        [ Se.Is Beam.depotCode $ Se.Eq (getId depotId),
          Se.Is Beam.personId $ Se.Eq (getId personId)
        ]
    ]
