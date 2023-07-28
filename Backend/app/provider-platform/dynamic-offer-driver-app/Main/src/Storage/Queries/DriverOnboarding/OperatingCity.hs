{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.OperatingCity where

import Data.Text
import qualified Database.Beam as B
import Database.Beam.Postgres
import Domain.Types.DriverOnboarding.OperatingCity
import Domain.Types.Merchant
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findOneWithKV, getMasterBeamConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverOnboarding.OperatingCity as BeamOC

-- create :: OperatingCity -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => OperatingCity -> m ()
create = createWithKV

-- findById ::
--   Transactionable m =>
--   Id OperatingCity ->
--   m (Maybe OperatingCity)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id OperatingCity -> m (Maybe OperatingCity)
findById (Id ocId) = findOneWithKV [Se.Is BeamOC.id $ Se.Eq ocId]

-- findByMerchantId ::
--   Transactionable m =>
--   Id Merchant ->
--   m (Maybe OperatingCity)
-- findByMerchantId personid = do
--   findOne $ do
--     vechileRegCert <- from $ table @OperatingCityT
--     return vechileRegCert

findByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m (Maybe OperatingCity)
findByMerchantId (Id merchantId) = findOneWithKV [Se.Is BeamOC.merchantId $ Se.Eq merchantId]

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

-- findEnabledCityByName :: (L.MonadFlow m, Log m) => Text -> m [OperatingCity]
-- findEnabledCityByName city =
--   findAllWithKV
--     [ Se.And
--         [Se.Is BeamOC.cityName $ Se.Eq city, Se.Is BeamOC.enabled $ Se.Eq True]
--     ]
findEnabledCityByName :: (L.MonadFlow m, Log m) => Text -> m [OperatingCity]
findEnabledCityByName city = do
  dbConf <- getMasterBeamConfig
  operatingCities <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_' (\BeamOC.OperatingCityT {..} -> (B.lower_ cityName B.==?. B.val_ city) B.&&?. (enabled B.==?. B.val_ True)) $
            B.all_ (BeamCommon.operatingCity BeamCommon.atlasDB)
  either (pure . const []) ((catMaybes <$>) . mapM fromTType') operatingCities

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

findEnabledCityByMerchantIdAndName :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m [OperatingCity]
findEnabledCityByMerchantIdAndName (Id mId) city = do
  dbConf <- getMasterBeamConfig
  operatingCities <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_' (\BeamOC.OperatingCityT {..} -> (merchantId B.==?. B.val_ mId) B.&&?. (B.lower_ cityName B.==?. B.val_ city) B.&&?. (enabled B.==?. B.val_ True)) $
            B.all_ (meshModelTableEntity @BeamOC.OperatingCityT @Postgres @(Se.DatabaseWith BeamOC.OperatingCityT))
  either (pure . const []) ((catMaybes <$>) . mapM fromTType') operatingCities

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

instance FromTType' BeamOC.OperatingCity OperatingCity where
  fromTType' BeamOC.OperatingCityT {..} = do
    pure $
      Just
        OperatingCity
          { id = Id id,
            merchantId = Id merchantId,
            cityName = cityName,
            enabled = enabled,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamOC.OperatingCity OperatingCity where
  toTType' OperatingCity {..} = do
    BeamOC.OperatingCityT
      { BeamOC.id = getId id,
        BeamOC.merchantId = getId merchantId,
        BeamOC.cityName = cityName,
        BeamOC.enabled = enabled,
        BeamOC.createdAt = createdAt,
        BeamOC.updatedAt = updatedAt
      }
