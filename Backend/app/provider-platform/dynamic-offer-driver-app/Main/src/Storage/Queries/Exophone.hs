{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Exophone
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Database.Beam as B
import Domain.Types.Exophone as DE (Exophone (..))
import qualified Domain.Types.Merchant as DM
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Exophone as BeamE

-- create :: Exophone -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DE.Exophone -> m (MeshResult ())
create exophone = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.ExophoneT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainExophoneToBeam exophone)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findAllByPhone :: Transactionable m => Text -> m [Exophone]
-- findAllByPhone phone = do
--   findAll $ do
--     exophone <- from $ table @ExophoneT
--     where_ $ just (exophone ^. ExophoneMerchantId) ==. subSelect subQuery
--     return exophone
--   where
--     subQuery = do
--       exophone1 <- from $ table @ExophoneT
--       where_ $
--         exophone1 ^. ExophonePrimaryPhone ==. val phone
--           ||. exophone1 ^. ExophoneBackupPhone ==. val phone
--       return (exophone1 ^. ExophoneMerchantId)

findAllMerchantIdsByPhone :: L.MonadFlow m => Text -> m [Id DM.Merchant]
findAllMerchantIdsByPhone phone = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.ExophoneT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      res <- either (pure []) (transformBeamExophoneToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Or [Se.Is BeamE.primaryPhone $ Se.Eq phone, Se.Is BeamE.backupPhone $ Se.Eq phone]]
      pure $ DE.merchantId <$> res
    Nothing -> pure []

findAllByPhone :: L.MonadFlow m => Text -> m [Exophone]
findAllByPhone phone = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.ExophoneT
  let updatedMeshConfig = setMeshConfig modelName
  merchIds <- findAllMerchantIdsByPhone phone
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamExophoneToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamE.merchantId $ Se.In $ getId <$> merchIds]
    Nothing -> pure []

-- findAllByMerchantId :: Transactionable m => Id DM.Merchant -> m [Exophone]
-- findAllByMerchantId merchantId = do
--   findAll $ do
--     exophone <- from $ table @ExophoneT
--     where_ $ exophone ^. ExophoneMerchantId ==. val (toKey merchantId)
--     return exophone

findAllByMerchantId :: L.MonadFlow m => Id DM.Merchant -> m [Exophone]
findAllByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.ExophoneT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamExophoneToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamE.merchantId $ Se.Eq merchantId]
    Nothing -> pure []

-- findAllExophones :: Transactionable m => m [Exophone]
-- findAllExophones = findAll $ from $ table @ExophoneT

findAllExophones :: L.MonadFlow m => m [Exophone]
findAllExophones = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.ExophoneT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamExophoneToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig []
    Nothing -> pure []

-- updateAffectedPhones :: [Text] -> SqlDB ()
-- updateAffectedPhones primaryPhones = do
--   let indianMobileCode = val "+91"
--   now <- getCurrentTime
--   let primaryPhonesList = valList primaryPhones
--   Esq.update $ \tbl -> do
--     let isPrimaryDown =
--           tbl ^. ExophonePrimaryPhone `in_` primaryPhonesList
--             ||. (indianMobileCode ++. tbl ^. ExophonePrimaryPhone) `in_` primaryPhonesList
--     set
--       tbl
--       [ ExophoneIsPrimaryDown =. isPrimaryDown,
--         ExophoneUpdatedAt =. val now
--       ]
--     where_ $ isPrimaryDown !=. tbl ^. ExophoneIsPrimaryDown

updateAffectedPhonesHelper :: (L.MonadFlow m, MonadTime m) => [Text] -> m Bool
updateAffectedPhonesHelper primaryNumbers = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let indianMobileCode = "+91"
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      geoms <-
        L.runDB c $
          L.findRow $
            B.select $
              B.limit_ 1 $
                B.filter_'
                  ( \BeamE.ExophoneT {..} ->
                      B.sqlBool_ (primaryPhone `B.in_` (B.val_ <$> primaryNumbers))
                        B.||?. B.sqlBool_ (B.concat_ [indianMobileCode, primaryPhone] `B.in_` (B.val_ <$> primaryNumbers))
                  )
                  $
                  -- B.all_ (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
                  B.all_ (BeamCommon.exophone BeamCommon.atlasDB)
      case geoms of
        Right (Just _) -> do
          pure True
        _ -> pure False
    Left _ -> pure (error "DB Config not found")

updateAffectedPhones :: (L.MonadFlow m, MonadTime m) => [Text] -> m ()
updateAffectedPhones primaryPhones = do
  now <- getCurrentTime
  isPrimary <- updateAffectedPhonesHelper primaryPhones
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      conn <- L.getOrInitSqlConn dbConf'
      case conn of
        Right c -> do
          _ <-
            L.runDB c $
              L.updateRows $
                B.update
                  (BeamCommon.exophone BeamCommon.atlasDB)
                  ( \BeamE.ExophoneT {..} ->
                      (isPrimaryDown B.<-. B.val_ isPrimary)
                        <> (updatedAt B.<-. B.val_ now)
                  )
                  (\BeamE.ExophoneT {..} -> isPrimaryDown B.==. B.val_ isPrimary)
          void $ pure Nothing
        Left _ -> pure (error "DB Config not found")
    Nothing -> pure (error "DB Config not found")

-- updateAffectedPhones' :: (L.MonadFlow m, MonadTime m) => [Text] -> m (MeshResult ())
-- updateAffectedPhones' primaryPhones = do
--   let indianMobileCode = "+91"
--   now <- getCurrentTime
--   let primaryPhonesList = valList primaryPhones
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   case dbConf of
--     Just dbConf' -> do
--       conn <- L.getOrInitSqlConn dbConf'
--       case conn of
--         Right c -> do

--     Nothing -> pure (error "DB Config not found")

-- updateAffectedPhones' :: (L.MonadFlow m, MonadTime m) => [Text] -> m (MeshResult ())
-- updateAffectedPhones' primaryPhones = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
-- let modelName = Se.modelTableName @BeamE.ExophoneT
-- let updatedMeshConfig = setMeshConfig modelName
--   now <- getCurrentTime
--   let indianMobileCode = "+91"
--   -- let isPrimaryDown = Se.Or [ Se.Is BeamE.primaryPhone $ Se.In primaryPhones, Se.Is (indianMobileCode ++ BeamE.primaryPhone) $ Se.In primaryPhones ]
--   let isPrimaryDown = Se.Or [ Se.Is BeamE.primaryPhone $ Se.In primaryPhones, Se.Is (\eT@BeamE.ExophoneT {..} -> (primaryPhone eT) ++ indianMobileCode) $ Se.In primaryPhones ]
--   case dbConf of
--     Just dbConf' ->
--       KV.updateWoReturningWithKVConnector
--         dbConf'
--         updatedMeshConfig
--         [ Se.Set BeamE.isPrimaryDown isPrimaryDown,
--           Se.Set BeamE.updatedAt now
--         ]
--         [Se.Is BeamE.isPrimaryDown (Se.Eq isPrimaryDown)]
--     Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- deleteByMerchantId :: Id DM.Merchant -> SqlDB ()
-- deleteByMerchantId merchantId = do
--   Esq.delete $ do
--     exophone <- from $ table @ExophoneT
--     where_ $ exophone ^. ExophoneMerchantId ==. val (toKey merchantId)

deleteByMerchantId :: L.MonadFlow m => Id DM.Merchant -> m ()
deleteByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamE.ExophoneT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamE.merchantId (Se.Eq merchantId)]
    Nothing -> pure ()

transformBeamExophoneToDomain :: BeamE.Exophone -> Exophone
transformBeamExophoneToDomain BeamE.ExophoneT {..} = do
  Exophone
    { id = Id id,
      merchantId = Id merchantId,
      primaryPhone = primaryPhone,
      backupPhone = backupPhone,
      isPrimaryDown = isPrimaryDown,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainExophoneToBeam :: Exophone -> BeamE.Exophone
transformDomainExophoneToBeam Exophone {..} =
  BeamE.ExophoneT
    { BeamE.id = getId id,
      BeamE.merchantId = getId merchantId,
      BeamE.primaryPhone = primaryPhone,
      BeamE.backupPhone = backupPhone,
      BeamE.isPrimaryDown = isPrimaryDown,
      BeamE.createdAt = createdAt,
      BeamE.updatedAt = updatedAt
    }
