{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.OperatingCity where

import Data.Text
import qualified Database.Beam as B
import Domain.Types.DriverOnboarding.OperatingCity
import Domain.Types.Merchant
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverOnboarding.OperatingCity as BeamOC

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => OperatingCity -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id OperatingCity -> m (Maybe OperatingCity)
findById (Id ocId) = findOneWithKV [Se.Is BeamOC.id $ Se.Eq ocId]

findByMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m (Maybe OperatingCity)
findByMerchantId (Id merchantId) = findOneWithKV [Se.Is BeamOC.merchantId $ Se.Eq merchantId]

findEnabledCityByName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [OperatingCity]
findEnabledCityByName city = do
  dbConf <- getMasterBeamConfig
  operatingCities <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_' (\BeamOC.OperatingCityT {..} -> (B.lower_ cityName B.==?. B.val_ city) B.&&?. (enabled B.==?. B.val_ True)) $
            B.all_ (BeamCommon.operatingCity BeamCommon.atlasDB)
  either (pure . const []) ((catMaybes <$>) . mapM fromTType') operatingCities

findEnabledCityByMerchantIdAndName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Text -> m [OperatingCity]
findEnabledCityByMerchantIdAndName (Id mId) city = do
  dbConf <- getMasterBeamConfig
  operatingCities <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_' (\BeamOC.OperatingCityT {..} -> (merchantId B.==?. B.val_ mId) B.&&?. (B.lower_ cityName B.==?. B.val_ city) B.&&?. (enabled B.==?. B.val_ True)) $
            B.all_ (BeamCommon.operatingCity BeamCommon.atlasDB)
  either (pure . const []) ((catMaybes <$>) . mapM fromTType') operatingCities

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
