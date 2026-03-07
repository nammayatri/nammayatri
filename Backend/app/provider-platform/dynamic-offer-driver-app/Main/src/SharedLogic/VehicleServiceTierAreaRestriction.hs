{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.VehicleServiceTierAreaRestriction
  ( areaToText,
    vstAreasCacheKey,
    populateVSTAreasCache,
    isAreaAllowedForVSTMaybe,
    clearVSTAreasCache,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.VehicleServiceTier
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow, logDebug, logInfo)
import qualified Lib.Types.SpecialLocation as SL

areaToText :: SL.Area -> Maybe Text
areaToText a@(SL.PickupDrop _ _) = Just (SL.areaToText a)
areaToText _ = Nothing

vstAreasCacheKey :: Id VehicleServiceTier -> Id MerchantOperatingCity -> Text
vstAreasCacheKey vstId cityId =
  "vst:allowed_areas:hash:" <> vstId.getId <> ":" <> cityId.getId

vstAreasPopulatedKey :: Id VehicleServiceTier -> Id MerchantOperatingCity -> Text
vstAreasPopulatedKey vstId cityId =
  "vst:allowed_areas:populated:" <> vstId.getId <> ":" <> cityId.getId

populateVSTAreasCache ::
  (Redis.HedisFlow m r, CacheFlow m r, MonadFlow m) =>
  VehicleServiceTier ->
  m ()
populateVSTAreasCache vst =
  case vst.allowedAreas of
    Nothing -> do
      logDebug $ "VST area cache: skip populate (no allowed_areas) for vstId=" <> vst.id.getId
      return ()
    Just list | null list -> do
      logDebug $ "VST area cache: skip populate (empty allowed_areas) for vstId=" <> vst.id.getId
      return ()
    Just areasList -> do
      let hashKey = vstAreasCacheKey vst.id vst.merchantOperatingCityId
          thirtyDaysInSeconds = 30 * 24 * 60 * 60 :: Int
      let populatedKey = vstAreasPopulatedKey vst.id vst.merchantOperatingCityId
      logInfo $ "VST area cache: writing to Redis key=" <> hashKey <> " areasCount=" <> show (length areasList)
      forM_ areasList $ \area ->
        Redis.hSetExp hashKey (SL.areaToText area) ("1" :: Text) thirtyDaysInSeconds
      void $ Redis.setExp populatedKey ("1" :: Text) thirtyDaysInSeconds
      logDebug $ "VST area cache: populated Redis for vstId=" <> vst.id.getId

isAreaAllowedForVSTMaybe ::
  (Redis.HedisFlow m r, MonadFlow m, CacheFlow m r) =>
  VehicleServiceTier ->
  Maybe SL.Area ->
  m Bool
isAreaAllowedForVSTMaybe _ Nothing = do
  logDebug "VST area check skipped (no PickupDrop area resolved), allowing"
  return True
isAreaAllowedForVSTMaybe vst (Just area) =
  case vst.allowedAreas of
    Nothing -> do
      logDebug $ "VST area check: allowed (no restrictions) vstId=" <> vst.id.getId <> " area=" <> show area
      return True
    Just list | null list -> do
      logDebug $ "VST area check: allowed (empty restrictions) vstId=" <> vst.id.getId <> " area=" <> show area
      return True
    Just _ -> case areaToText area of
      Nothing -> do
        logInfo $ "VST area check: area is not PickupDrop (cannot check cache), denying vstId=" <> vst.id.getId <> " area=" <> show area
        return False
      Just areaText -> checkAreaInCache vst areaText

checkAreaInCache ::
  (Redis.HedisFlow m r, MonadFlow m, CacheFlow m r) =>
  VehicleServiceTier ->
  Text ->
  m Bool
checkAreaInCache vst areaText = do
  let hashKey = vstAreasCacheKey vst.id vst.merchantOperatingCityId
  mbValue <- Redis.hGet @Text hashKey areaText
  case mbValue of
    Just _ -> do
      logDebug $ "VST area check: cache HIT vstId=" <> vst.id.getId <> " areaText=" <> areaText
      return True
    Nothing -> do
      mbPopulated <- Redis.get @Text (vstAreasPopulatedKey vst.id vst.merchantOperatingCityId)
      if isNothing mbPopulated
        then do
          logInfo $ "VST area check: cache MISS (empty) vstId=" <> vst.id.getId <> " areaText=" <> areaText <> " key=" <> hashKey <> " (warming from passed-in VST)"
          populateVSTAreasCache vst
          return $ maybe False (elem areaText . map SL.areaToText) vst.allowedAreas
        else do
          logDebug $ "VST area check: area not in allowed list, denying vstId=" <> vst.id.getId <> " areaText=" <> areaText
          return False

clearVSTAreasCache :: (Redis.HedisFlow m r) => VehicleServiceTier -> m ()
clearVSTAreasCache vst = do
  void $ Redis.del $ vstAreasCacheKey vst.id vst.merchantOperatingCityId
  void $ Redis.del $ vstAreasPopulatedKey vst.id vst.merchantOperatingCityId
