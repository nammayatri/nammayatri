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

import Domain.Types.Exophone as DE
import qualified Domain.Types.Merchant as DM
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Exophone as BeamE
import Storage.Tabular.Exophone

-- create :: Exophone -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DE.Exophone -> m (MeshResult ())
create exophone = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainExophoneToBeam exophone)
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

findAllByPhone :: L.MonadFlow m => Text -> m [Exophone]
findAllByPhone phone = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamExophoneToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Or [Se.Is BeamE.primaryPhone $ Se.Eq phone, Se.Is BeamE.backupPhone $ Se.Eq phone]]
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
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamExophoneToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamE.merchantId $ Se.Eq merchantId]
    Nothing -> pure []

-- findAllExophones :: Transactionable m => m [Exophone]
-- findAllExophones = findAll $ from $ table @ExophoneT

findAllExophones :: L.MonadFlow m => m [Exophone]
findAllExophones = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamExophoneToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig []
    Nothing -> pure []

updateAffectedPhones :: [Text] -> SqlDB ()
updateAffectedPhones primaryPhones = do
  let indianMobileCode = val "+91"
  now <- getCurrentTime
  let primaryPhonesList = valList primaryPhones
  Esq.update $ \tbl -> do
    let isPrimaryDown =
          tbl ^. ExophonePrimaryPhone `in_` primaryPhonesList
            ||. (indianMobileCode ++. tbl ^. ExophonePrimaryPhone) `in_` primaryPhonesList
    set
      tbl
      [ ExophoneIsPrimaryDown =. isPrimaryDown,
        ExophoneUpdatedAt =. val now
      ]
    where_ $ isPrimaryDown !=. tbl ^. ExophoneIsPrimaryDown

-- deleteByMerchantId :: Id DM.Merchant -> SqlDB ()
-- deleteByMerchantId merchantId = do
--   Esq.delete $ do
--     exophone <- from $ table @ExophoneT
--     where_ $ exophone ^. ExophoneMerchantId ==. val (toKey merchantId)

deleteByMerchantId :: L.MonadFlow m => Id DM.Merchant -> m ()
deleteByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
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
  BeamE.defaultExophone
    { BeamE.id = getId id,
      BeamE.merchantId = getId merchantId,
      BeamE.primaryPhone = primaryPhone,
      BeamE.backupPhone = backupPhone,
      BeamE.isPrimaryDown = isPrimaryDown,
      BeamE.createdAt = createdAt,
      BeamE.updatedAt = updatedAt
    }
