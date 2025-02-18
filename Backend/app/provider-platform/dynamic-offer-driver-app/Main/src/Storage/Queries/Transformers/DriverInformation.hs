module Storage.Queries.Transformers.DriverInformation where

import qualified Domain.Types.Extra.Plan as DPlan
import Kernel.Beam.Functions (updateOneWithKV)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Lib.Queries.SpecialLocation as SpecialLocation
import Sequelize as Se
import qualified Storage.Beam.DriverInformation as Beam

getPreferredPrimarySpecialLoc :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> m (Maybe SpecialLocation.SpecialLocationWarrior)
getPreferredPrimarySpecialLoc Nothing = return Nothing
getPreferredPrimarySpecialLoc (Just specialLocId) = do
  Hedis.safeGet (makeSpecialLocIdKey specialLocId) >>= \case
    Just a -> pure a
    Nothing -> do
      mbSpecialLoc <- SpecialLocation.findById (Id specialLocId)
      case mbSpecialLoc of
        Nothing -> return Nothing
        Just specialLoc -> do
          specialLocation <- SpecialLocation.specialLocToSpecialLocWarrior specialLoc
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          let idKey = makeSpecialLocIdKey specialLocId
          Hedis.setExp idKey specialLocation expTime
          return $ Just specialLocation

makeSpecialLocIdKey :: Text -> Text
makeSpecialLocIdKey locId = "CachedQueries:SpecialLoc:Id:" <> locId

backfillServiceEnabledForSubscription ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  Maybe [DPlan.ServiceNames] ->
  m [DPlan.ServiceNames]
backfillServiceEnabledForSubscription driverId enableSubscription = do
  case enableSubscription of
    Just elems -> return elems
    _ -> do
      _now <- getCurrentTime
      updateOneWithKV
        [ Se.Set Beam.servicesEnabledForSubscription (Just [DPlan.YATRI_SUBSCRIPTION]),
          Se.Set Beam.updatedAt _now
        ]
        [Se.Is Beam.driverId $ Se.Eq driverId]
      return [DPlan.YATRI_SUBSCRIPTION]
