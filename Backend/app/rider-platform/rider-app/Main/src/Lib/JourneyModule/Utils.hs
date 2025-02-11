module Lib.JourneyModule.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Trip as DTrip
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Rollout as CQRollout
import Tools.DynamicLogic
import Tools.Error

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f = go 0
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> DTrip.MultimodalTravelMode
convertMultiModalModeToTripMode input distance maximumWalkDistance = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Subway -> DTrip.Subway
  MultiModal.Walk -> if distance > maximumWalkDistance then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi

getVersionTag :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> Spec.VehicleCategory -> Maybe Text -> m Int -- TODO: add stickiness logic
getVersionTag mocId vehicleCategory searchId = do
  maybe (doLookup vehicleCategory mocId) (getVersionTagBySearchIdAndVehicleCategory vehicleCategory mocId) searchId

doLookup :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Spec.VehicleCategory -> Id MerchantOperatingCity -> m Int
doLookup vehicleCategory mocId = do
  rollouts <- CQRollout.findAllByMerchantOperatingCityAndVehicleType (Just mocId) vehicleCategory
  chosenRollout <- chooseLogic rollouts >>= fromMaybeM (InternalError $ "No rollout found for vehicleType" <> show vehicleCategory <> " and mocId: " <> show mocId)
  pure chosenRollout.versionTag

getVersionTagBySearchIdAndVehicleCategory :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Spec.VehicleCategory -> Id MerchantOperatingCity -> Text -> m Int
getVersionTagBySearchIdAndVehicleCategory vehicleCategory mocId searchId = do
  Hedis.safeGet (makeVersionTagKey searchId vehicleCategory) >>= \case
    Just a -> return a
    Nothing -> findAndCacheVersionTagBySearchIdAndVehicleCategory searchId vehicleCategory mocId

findAndCacheVersionTagBySearchIdAndVehicleCategory :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> Spec.VehicleCategory -> Id MerchantOperatingCity -> m Int
findAndCacheVersionTagBySearchIdAndVehicleCategory searchId vehicleCategory mocId = do
  versionTag <- doLookup vehicleCategory mocId
  cacheVersionTag searchId vehicleCategory versionTag
  pure versionTag

cacheVersionTag :: (CacheFlow m r, EsqDBFlow m r) => Text -> Spec.VehicleCategory -> Int -> m ()
cacheVersionTag searchId vehicleCategory versionTag = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeVersionTagKey searchId vehicleCategory) versionTag expTime

makeVersionTagKey :: Text -> Spec.VehicleCategory -> Text
makeVersionTagKey searchId vehicleCategory = "CachedQueries:JourneyModule:VersionTag:" <> searchId <> ":" <> show vehicleCategory
