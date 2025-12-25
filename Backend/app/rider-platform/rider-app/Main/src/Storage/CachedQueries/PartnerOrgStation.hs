{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.PartnerOrgStation
  ( findByPOrgIdAndPOrgStationId,
    findByStationCodeAndPOrgId,
    findStationWithPOrgName,
    findStationWithPOrgIdAndStationId,
  )
where

import Domain.Types.IntegratedBPPConfig
import Domain.Types.PartnerOrgStation
import Domain.Types.PartnerOrganization
import Domain.Types.Station as DStation
import EulerHS.Prelude ((+||), (||+))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.PartnerOrgStation as Queries
import Tools.Error

findByPOrgIdAndPOrgStationId :: (CacheFlow m r, EsqDBFlow m r) => Id PartnerOrganization -> Id PartnerOrgStation -> m (Maybe PartnerOrgStation)
findByPOrgIdAndPOrgStationId partnerOrgId partnerOrgStationId = do
  let key = makePOrgIdAndPOrgStationIdKey partnerOrgId partnerOrgStationId
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheByPOrgIdAndPOrgStationId /=<< Queries.findByPOrgIdAndPOrgStationId partnerOrgId partnerOrgStationId

makePOrgIdAndPOrgStationIdKey :: Id PartnerOrganization -> Id PartnerOrgStation -> Text
makePOrgIdAndPOrgStationIdKey partnerOrgId partnerOrgStationId = "CachedQueries:PartnerOrgStation:POrgId:" <> partnerOrgId.getId <> ":POrgStationId:" <> partnerOrgStationId.getId

cacheByPOrgIdAndPOrgStationId :: (CacheFlow m r) => PartnerOrgStation -> m ()
cacheByPOrgIdAndPOrgStationId pOrgStation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makePOrgIdAndPOrgStationIdKey pOrgStation.partnerOrgId pOrgStation.partnerOrgStationId
  Hedis.setExp key pOrgStation expTime

findByStationCodeAndPOrgId :: (CacheFlow m r, EsqDBFlow m r) => Text -> Id PartnerOrganization -> m (Maybe PartnerOrgStation)
findByStationCodeAndPOrgId stationCode partnerOrgId = do
  let key = makeStationCodeAndPOrgIdKey stationCode partnerOrgId
  Hedis.safeGet key >>= \case
    Just a -> return $ Just a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheByStationIdAndPOrgId /=<< Queries.findByStationCodeAndPOrgId stationCode partnerOrgId

makeStationCodeAndPOrgIdKey :: Text -> Id PartnerOrganization -> Text
makeStationCodeAndPOrgIdKey stationCode partnerOrgId = "CachedQueries:PartnerOrgStation:StationCode:" <> stationCode <> ":POrgId:" <> partnerOrgId.getId

cacheByStationIdAndPOrgId :: (CacheFlow m r) => PartnerOrgStation -> m ()
cacheByStationIdAndPOrgId partnerOrgStation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeStationCodeAndPOrgIdKey partnerOrgStation.stationCode partnerOrgStation.partnerOrgId
  Hedis.setExp key partnerOrgStation expTime

findStationWithPOrgName :: (CacheFlow m r, EsqDBFlow m r, HasShortDurationRetryCfg r c) => Id PartnerOrganization -> Id PartnerOrgStation -> IntegratedBPPConfig -> m Station
findStationWithPOrgName partnerOrgId partnerOrgStationId integratedBPPConfig = do
  partnerOrgStation <- findByPOrgIdAndPOrgStationId partnerOrgId partnerOrgStationId >>= fromMaybeM (PartnerOrgStationDoesNotExist partnerOrgId.getId partnerOrgStationId.getId)
  station <- OTPRest.getStationByGtfsIdAndStopCode partnerOrgStation.stationCode integratedBPPConfig >>= fromMaybeM (StationNotFound $ "StationCode:" +|| partnerOrgStation.stationCode ||+ "")
  let stationWithPOrgName = station {DStation.name = partnerOrgStation.name}
  return stationWithPOrgName

findStationWithPOrgIdAndStationId :: (CacheFlow m r, EsqDBFlow m r, HasShortDurationRetryCfg r c) => Text -> Id PartnerOrganization -> IntegratedBPPConfig -> m Station
findStationWithPOrgIdAndStationId stationCode partnerOrgId integratedBPPConfig = do
  partnerOrgStation <- findByStationCodeAndPOrgId stationCode partnerOrgId
  let pOrgName = partnerOrgStation <&> (.name)
  station <- OTPRest.getStationByGtfsIdAndStopCode stationCode integratedBPPConfig >>= fromMaybeM (StationNotFound $ "StationCode:" +|| stationCode ||+ "")
  let stationWithPOrgName = station {DStation.name = fromMaybe station.name pOrgName}
  return stationWithPOrgName
