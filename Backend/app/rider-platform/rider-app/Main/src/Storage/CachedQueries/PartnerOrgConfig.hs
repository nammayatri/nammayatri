{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.PartnerOrgConfig
  ( findByIdAndCfgType,
  )
where

import Domain.Types.PartnerOrgConfig
import Domain.Types.PartnerOrganization
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PartnerOrgConfig as Queries

findByIdAndCfgType :: (CacheFlow m r, EsqDBFlow m r) => Id PartnerOrganization -> ConfigType -> m (Maybe PartnerOrgConfig)
findByIdAndCfgType partnerOrgId cfgType = do
  let key = makeIdAndConfigTypeKey partnerOrgId cfgType
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cachePartnerOrgConfigByIdAndType /=<< Queries.findByPartnerOrgIdAndConfigType partnerOrgId cfgType

makeIdAndConfigTypeKey :: Id PartnerOrganization -> ConfigType -> Text
makeIdAndConfigTypeKey partnerOrgId cfgType = "CachedQueries:PartnerOrgConfig:POrgId:" <> partnerOrgId.getId <> ":ConfigType:" <> show cfgType

cachePartnerOrgConfigByIdAndType :: (CacheFlow m r) => PartnerOrgConfig -> m ()
cachePartnerOrgConfigByIdAndType pOrgCfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeIdAndConfigTypeKey pOrgCfg.partnerOrgId (getConfigType pOrgCfg.config)
  Hedis.setExp key pOrgCfg expTime
