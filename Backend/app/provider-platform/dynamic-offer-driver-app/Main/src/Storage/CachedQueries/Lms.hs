{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Lms where

import qualified Domain.Types.LmsModule as LmsModule
import qualified Domain.Types.LmsModuleTranslation as DTLMT
import qualified Domain.Types.LmsModuleVideoInformation as DTLMVI
import qualified Domain.Types.LmsVideoTranslation as DTLVT
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.QuestionInformation as DTQI
import qualified Domain.Types.QuestionModuleMapping as DTQMM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.LmsModule as SQLM
import qualified Storage.Queries.LmsModuleTranslation as SQLMT
import qualified Storage.Queries.LmsModuleVideoInformation as SQLMVI
import qualified Storage.Queries.LmsVideoTranslation as SQLVT
import qualified Storage.Queries.QuestionInformation as SQQI
import qualified Storage.Queries.QuestionModuleMapping as SQQMM

--- get all lms Modules

getAllModules :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Id DMOC.MerchantOperatingCity -> m [LmsModule.LmsModule]
getAllModules mbLimit mbOffset merchantOperatingCityId =
  Hedis.safeGet (makeAllModuleKeyByMerchantOpCityId merchantOperatingCityId) >>= \case
    Just a -> return a
    Nothing -> cacheAllModulesByMerchantOpCityId merchantOperatingCityId /=<< SQLM.getAllModules mbLimit mbOffset merchantOperatingCityId

cacheAllModulesByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> [LmsModule.LmsModule] -> m ()
cacheAllModulesByMerchantOpCityId merchantOperatingCityId modules = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAllModuleKeyByMerchantOpCityId merchantOperatingCityId) modules expTime

makeAllModuleKeyByMerchantOpCityId :: Id DMOC.MerchantOperatingCity -> Text
makeAllModuleKeyByMerchantOpCityId merchantOperatingCityId = "CachedQueries:AllModules:MerchantOperatingCityId-" <> merchantOperatingCityId.getId

-- get all lms modules with merchantOperatingCity and modulesection

getAllModulesWithModuleSection :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Id DMOC.MerchantOperatingCity -> Maybe LmsModule.ModuleSection -> m [LmsModule.LmsModule]
getAllModulesWithModuleSection mbLimit mbOffset merchantOperatingCityId mbModuleSection =
  Hedis.safeGet (makeAllModuleKeyByMerchantOpCityIdAndModuleSection merchantOperatingCityId mbModuleSection) >>= \case
    Just a -> return a
    Nothing -> cacheAllModulesByMerchantOpCityIdAndModuleSection merchantOperatingCityId mbModuleSection /=<< SQLM.getAllModulesWithModuleSection mbLimit mbOffset merchantOperatingCityId mbModuleSection

cacheAllModulesByMerchantOpCityIdAndModuleSection :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe LmsModule.ModuleSection -> [LmsModule.LmsModule] -> m ()
cacheAllModulesByMerchantOpCityIdAndModuleSection merchantOperatingCityId mbmoduleSection modules = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAllModuleKeyByMerchantOpCityIdAndModuleSection merchantOperatingCityId mbmoduleSection) modules expTime

makeAllModuleKeyByMerchantOpCityIdAndModuleSection :: Id DMOC.MerchantOperatingCity -> Maybe LmsModule.ModuleSection -> Text
makeAllModuleKeyByMerchantOpCityIdAndModuleSection merchantOperatingCityId mbModuleSection = "CachedQueries:AllModules:MerchantOperatingCityId-" <> merchantOperatingCityId.getId <> ":ModuleSection-" <> (show mbModuleSection)

--- get all lms module translations

getAllModuleTranslations :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id LmsModule.LmsModule -> m [DTLMT.LmsModuleTranslation]
getAllModuleTranslations moduleId =
  Hedis.safeGet (makeAllTranslationsForAModuleKey moduleId) >>= \case
    Just a -> return a
    Nothing -> cacheAllTranslationsForAModule moduleId /=<< SQLMT.getAllTranslationsByModuleId moduleId

cacheAllTranslationsForAModule :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id LmsModule.LmsModule -> [DTLMT.LmsModuleTranslation] -> m ()
cacheAllTranslationsForAModule moduleId translations = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAllTranslationsForAModuleKey moduleId) translations expTime

makeAllTranslationsForAModuleKey :: Id LmsModule.LmsModule -> Text
makeAllTranslationsForAModuleKey moduleId = "CachedQueries:AllTranslations:moduleId-" <> moduleId.getId

--- get all videos

getAllVideos :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id LmsModule.LmsModule -> [DTLMVI.VideoStatus] -> m [DTLMVI.LmsModuleVideoInformation]
getAllVideos moduleId videoStatus =
  Hedis.safeGet (makeAllVideosKeyByModuleId moduleId) >>= \case
    Just a -> return a
    Nothing -> cacheAllVideosByModuleId moduleId /=<< SQLMVI.getAllVideos moduleId videoStatus

cacheAllVideosByModuleId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id LmsModule.LmsModule -> [DTLMVI.LmsModuleVideoInformation] -> m ()
cacheAllVideosByModuleId moduleId videos = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAllVideosKeyByModuleId moduleId) videos expTime

makeAllVideosKeyByModuleId :: Id LmsModule.LmsModule -> Text
makeAllVideosKeyByModuleId moduleId = "CachedQueries:AllVideos:ModuleId-" <> moduleId.getId

--- get all video translations

getAllTranslationsForVideoId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DTLMVI.LmsModuleVideoInformation -> m [DTLVT.LmsVideoTranslation]
getAllTranslationsForVideoId videoId =
  Hedis.safeGet (makeAllTranslationsForAVideoKey videoId) >>= \case
    Just a -> return a
    Nothing -> cacheAllTranslationsForAVideo videoId /=<< SQLVT.getAllTranslationsForVideoId videoId

cacheAllTranslationsForAVideo :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DTLMVI.LmsModuleVideoInformation -> [DTLVT.LmsVideoTranslation] -> m ()
cacheAllTranslationsForAVideo videoId translations = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAllTranslationsForAVideoKey videoId) translations expTime

makeAllTranslationsForAVideoKey :: Id DTLMVI.LmsModuleVideoInformation -> Text
makeAllTranslationsForAVideoKey videoId = "CachedQueries:AllTranslations:videoId-" <> videoId.getId

--- get all quiz Ids

getAllQuestions :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id LmsModule.LmsModule -> m [DTQMM.QuestionModuleMapping]
getAllQuestions moduleId =
  Hedis.safeGet (makeAllQuestionsByModuleIdKey moduleId) >>= \case
    Just a -> return a
    Nothing -> cacheAllQuestionsByModuleId moduleId /=<< SQQMM.findAllWithModuleId moduleId

cacheAllQuestionsByModuleId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id LmsModule.LmsModule -> [DTQMM.QuestionModuleMapping] -> m ()
cacheAllQuestionsByModuleId moduleId questions = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAllQuestionsByModuleIdKey moduleId) questions expTime

makeAllQuestionsByModuleIdKey :: Id LmsModule.LmsModule -> Text
makeAllQuestionsByModuleIdKey moduleId = "CachedQueries:AllQuestions:ModuleId-" <> moduleId.getId

--- get all question translations

getAllTranslationsForQuestionId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DTQMM.QuestionModuleMapping -> m [DTQI.QuestionInformation]
getAllTranslationsForQuestionId questionId =
  Hedis.safeGet (makeAllTranslationsForAQuestionKey questionId) >>= \case
    Just a -> return a
    Nothing -> cacheAllTranslationsForAQuestion questionId /=<< SQQI.getAllTranslationsByQuestionId questionId

cacheAllTranslationsForAQuestion :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DTQMM.QuestionModuleMapping -> [DTQI.QuestionInformation] -> m ()
cacheAllTranslationsForAQuestion questionId translations = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeAllTranslationsForAQuestionKey questionId) translations expTime

makeAllTranslationsForAQuestionKey :: Id DTQMM.QuestionModuleMapping -> Text
makeAllTranslationsForAQuestionKey questionId = "CachedQueries:AllTranslations:questionId-" <> questionId.getId
