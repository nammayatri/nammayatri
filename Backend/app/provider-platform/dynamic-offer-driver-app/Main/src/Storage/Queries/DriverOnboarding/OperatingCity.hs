{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.OperatingCity where

import Data.Text
import qualified Database.Beam as B
import Database.Beam.Postgres
import Domain.Types.DriverOnboarding.OperatingCity
import Domain.Types.Merchant
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.OperatingCity as BeamOC

-- import Storage.Tabular.DriverOnboarding.OperatingCity

-- create :: OperatingCity -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => OperatingCity -> m (MeshResult ())
create operatingCity = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamOC.OperatingCityT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainOperatingCityToBeam operatingCity)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById ::
--   Transactionable m =>
--   Id OperatingCity ->
--   m (Maybe OperatingCity)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id OperatingCity -> m (Maybe OperatingCity)
findById (Id ocId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamOC.OperatingCityT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamOperatingCityToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamOC.id $ Se.Eq ocId]
    Nothing -> pure Nothing

-- findByMerchantId ::
--   Transactionable m =>
--   Id Merchant ->
--   m (Maybe OperatingCity)
-- findByMerchantId personid = do
--   findOne $ do
--     vechileRegCert <- from $ table @OperatingCityT
--     where_ $ vechileRegCert ^. OperatingCityMerchantId ==. val (toKey personid)
--     return vechileRegCert

findByMerchantId :: L.MonadFlow m => Id Merchant -> m (Maybe OperatingCity)
findByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamOC.OperatingCityT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamOperatingCityToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamOC.merchantId $ Se.Eq merchantId]
    Nothing -> pure Nothing

-- findEnabledCityByName ::
--   Transactionable m =>
--   Text ->
--   m [OperatingCity]
-- findEnabledCityByName city =
--   Esq.findAll $ do
--     operatingCity <- from $ table @OperatingCityT
--     where_ $
--       lower_ (operatingCity ^. OperatingCityCityName) ==. val city
--         &&. operatingCity ^. OperatingCityEnabled
--     return operatingCity

findEnabledCityByName :: L.MonadFlow m => Text -> m [OperatingCity]
findEnabledCityByName city = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamOC.OperatingCityT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      either (pure []) (transformBeamOperatingCityToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.And
              [Se.Is BeamOC.cityName $ Se.Eq city, Se.Is BeamOC.enabled $ Se.Eq True]
          ]
    Nothing -> pure []

-- findEnabledCityByMerchantIdAndName ::
--   Transactionable m =>
--   Id Merchant ->
--   Text ->
--   m [OperatingCity]
-- findEnabledCityByMerchantIdAndName merchantId city =
--   Esq.findAll $ do
--     operatingCity <- from $ table @OperatingCityT
--     where_ $
--       lower_ (operatingCity ^. OperatingCityCityName) ==. val city
--         &&. operatingCity ^. OperatingCityMerchantId ==. val (toKey merchantId)
--         &&. operatingCity ^. OperatingCityEnabled
--     return operatingCity

findEnabledCityByMerchantIdAndName :: L.MonadFlow m => Id Merchant -> Text -> m [OperatingCity]
findEnabledCityByMerchantIdAndName (Id mId) city = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      operatingCities <-
        L.runDB c $
          L.findRows $
            B.select $
              B.filter_' (\BeamOC.OperatingCityT {..} -> (merchantId B.==?. B.val_ mId) B.&&?. (cityName B.==?. B.val_ city) B.&&?. (enabled B.==?. B.val_ True)) $
                B.all_ (meshModelTableEntity @BeamOC.OperatingCityT @Postgres @(Se.DatabaseWith BeamOC.OperatingCityT))
      pure (either (const []) (transformBeamOperatingCityToDomain <$>) operatingCities)
    Left _ -> pure []

transformBeamOperatingCityToDomain :: BeamOC.OperatingCity -> OperatingCity
transformBeamOperatingCityToDomain BeamOC.OperatingCityT {..} = do
  OperatingCity
    { id = Id id,
      merchantId = Id merchantId,
      cityName = cityName,
      enabled = enabled,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainOperatingCityToBeam :: OperatingCity -> BeamOC.OperatingCity
transformDomainOperatingCityToBeam OperatingCity {..} =
  BeamOC.OperatingCityT
    { BeamOC.id = getId id,
      BeamOC.merchantId = getId merchantId,
      BeamOC.cityName = cityName,
      BeamOC.enabled = enabled,
      BeamOC.createdAt = createdAt,
      BeamOC.updatedAt = updatedAt
    }
