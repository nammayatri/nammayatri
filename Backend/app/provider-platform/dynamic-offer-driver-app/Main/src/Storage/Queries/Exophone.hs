{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Exophone
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Database.Beam as B
import Domain.Types.Exophone as DE (Exophone (..))
import qualified Domain.Types.Merchant as DM
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, deleteWithKV, findAllWithKV, getMasterBeamConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Exophone as BeamE

-- create :: Exophone -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => DE.Exophone -> m ()
create = createWithKV

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

findAllMerchantIdsByPhone :: (L.MonadFlow m, Log m) => Text -> m [Id DM.Merchant]
findAllMerchantIdsByPhone phone = findAllWithKV [Se.Or [Se.Is BeamE.primaryPhone $ Se.Eq phone, Se.Is BeamE.backupPhone $ Se.Eq phone]] <&> (DE.merchantId <$>)

findAllByPhone :: (L.MonadFlow m, Log m) => Text -> m [Exophone]
findAllByPhone phone = do
  merchIds <- findAllMerchantIdsByPhone phone
  findAllWithKV [Se.Is BeamE.merchantId $ Se.In $ getId <$> merchIds]

-- findAllByMerchantId :: Transactionable m => Id DM.Merchant -> m [Exophone]
-- findAllByMerchantId merchantId = do
--   findAll $ do
--     exophone <- from $ table @ExophoneT
--     where_ $ exophone ^. ExophoneMerchantId ==. val (toKey merchantId)
--     return exophone

findAllByMerchantId :: (L.MonadFlow m, Log m) => Id DM.Merchant -> m [Exophone]
findAllByMerchantId (Id merchantId) = do
  findAllWithKV [Se.Is BeamE.merchantId $ Se.Eq merchantId]

-- findAllExophones :: Transactionable m => m [Exophone]
-- findAllExophones = findAll $ from $ table @ExophoneT

findAllExophones :: (L.MonadFlow m, Log m) => m [Exophone]
findAllExophones = findAllWithKV [Se.Is BeamE.id $ Se.Not $ Se.Eq $ getId ""]

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
  dbConf <- getMasterBeamConfig
  let indianMobileCode = "+91"
  geoms <-
    L.runDB dbConf $
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
  either (const (pure False)) (pure . isJust) geoms

updateAffectedPhones :: (L.MonadFlow m, MonadTime m) => [Text] -> m ()
updateAffectedPhones primaryPhones = do
  now <- getCurrentTime
  isPrimary <- updateAffectedPhonesHelper primaryPhones
  dbConf <- getMasterBeamConfig
  void $
    L.runDB dbConf $
      L.updateRows $
        B.update
          (BeamCommon.exophone BeamCommon.atlasDB)
          ( \BeamE.ExophoneT {..} ->
              (isPrimaryDown B.<-. B.val_ isPrimary)
                <> (updatedAt B.<-. B.val_ now)
          )
          (\BeamE.ExophoneT {..} -> isPrimaryDown B.==. B.val_ isPrimary)

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
-- updatedMeshConfig <- setMeshConfig modelName
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

deleteByMerchantId :: (L.MonadFlow m, Log m) => Id DM.Merchant -> m ()
deleteByMerchantId (Id merchantId) = deleteWithKV [Se.Is BeamE.merchantId (Se.Eq merchantId)]

instance FromTType' BeamE.Exophone Exophone where
  fromTType' BeamE.ExophoneT {..} = do
    pure $
      Just
        Exophone
          { id = Id id,
            merchantId = Id merchantId,
            primaryPhone = primaryPhone,
            backupPhone = backupPhone,
            isPrimaryDown = isPrimaryDown,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamE.Exophone Exophone where
  toTType' Exophone {..} = do
    BeamE.ExophoneT
      { BeamE.id = getId id,
        BeamE.merchantId = getId merchantId,
        BeamE.primaryPhone = primaryPhone,
        BeamE.backupPhone = backupPhone,
        BeamE.isPrimaryDown = isPrimaryDown,
        BeamE.createdAt = createdAt,
        BeamE.updatedAt = updatedAt
      }
